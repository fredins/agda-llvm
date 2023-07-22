{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NondecreasingIndentation   #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-binds #-}


module Agda.Llvm.Compiler (llvmBackend) where

import           Control.DeepSeq                       (NFData)
import           Control.Monad                         (forM_, liftM2,
                                                        replicateM, replicateM_,
                                                        unless, zipWithM,
                                                        zipWithM_, (<=<))
import           Control.Monad.Reader                  (MonadIO (liftIO),
                                                        MonadReader (ask, local),
                                                        Reader, ReaderT, asks,
                                                        runReader, runReaderT,
                                                        when)
import           Control.Monad.State                   (MonadState (put), State,
                                                        StateT, evalState,
                                                        evalStateT, execState,
                                                        forM, get, gets, modify,
                                                        runState)
import           Data.Function                         (on)
import           Data.List                             (deleteBy, find, insert,
                                                        intercalate,
                                                        intersectBy, lookup,
                                                        mapAccumL, nub,
                                                        partition, singleton,
                                                        sort, sortOn, union,
                                                        unionBy, (\\))
-- import           Data.List.NonEmpty                    (NonEmpty ((:|)))
import qualified Data.List.NonEmpty.Extra              as NonEmpty
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Maybe                            (catMaybes, fromJust,
                                                        fromMaybe, isJust,
                                                        listToMaybe, mapMaybe)
import           Data.Set                              (Set)
import qualified Data.Set                              as Set
import           Data.Tuple.Extra                      (secondM)
import           Debug.Trace                           (trace, traceShow)
import           Prelude                               hiding ((!!))

import           Agda.Compiler.Backend                 hiding (Prim, initEnv)
import           Agda.Compiler.JS.Pretty               (vsep)
import           Agda.Compiler.ToTreeless              (closedTermToTreeless)
import           Agda.Compiler.Treeless.Builtin        (translateBuiltins)
import           Agda.Compiler.Treeless.NormalizeNames (normalizeNames)
import           Agda.Interaction.Options
import           Agda.Llvm.Grin
import qualified Agda.Llvm.Llvm                        as L
import           Agda.Syntax.Common                    (NameId (NameId))
import           Agda.Syntax.Internal                  (Type)
import qualified Agda.Syntax.Internal                  as I
import           Agda.Syntax.Literal                   (Literal (LitChar, LitNat))
import           Agda.Syntax.TopLevelModuleName
import qualified Agda.TypeChecking.Monad.Base          as Base
import           Agda.TypeChecking.Substitute          (Abstract (abstract),
                                                        Subst (applySubst),
                                                        raise, raiseFrom,
                                                        raiseFromS, raiseS, wkS)
import           Agda.Utils.Function                   (applyWhen, applyWhenM)
import           Agda.Utils.Functor
import           Agda.Utils.Impossible
import           Agda.Utils.List
import           Agda.Utils.List1                      (List1, pattern (:|))
import qualified Agda.Utils.List1                      as List1
import           Agda.Utils.Maybe                      (allJustM, boolToMaybe,
                                                        caseMaybe, caseMaybeM,
                                                        fromMaybeM, maybeM,
                                                        whenJust, whenJustM)
import           Agda.Utils.Monad                      (ifM, mapMaybeM)
import           Agda.Utils.Pretty
import           Agda.Utils.Tuple                      (swap)
import           Control.Applicative                   (Applicative (liftA2))
import           Control.Monad.Identity                (Identity (Identity),
                                                        runIdentity)
import           Data.Bifunctor                        (Bifunctor (bimap, first, second))
import           Data.Bitraversable                    (bimapM)
import           Data.Foldable                         (foldlM, foldrM)
import           Data.Foldable.Extra                   (notNull)
-- import qualified Data.List.NonEmpty.Extra              as List1
import           Data.List.Extra                       (firstJust)
import           GHC.Generics                          (Generic)

llvmBackend :: Backend
llvmBackend = Backend llvmBackend'

llvmBackend' :: Backend' LlvmOptions LlvmEnv LlvmModuleEnv LlvmModule (Maybe GrinDefinition)
llvmBackend' = Backend'
  {backendName           = "LLVM"
  ,backendVersion        = Nothing
  ,options               = defaultLlvmOptions
  ,commandLineFlags      = llvmCommandLineFlags
  ,isEnabled             = flagLlvmCompile
  ,preCompile            = llvmPreCompile
  ,postCompile           = llvmPostCompile
  ,preModule             = \_ _ _ -> pure $ pure $ Recompile LlvmModuleEnv
  ,postModule            = llvmPostModule
  ,compileDef            = llvmCompileDef
  ,scopeCheckingSuffices = False
  ,mayEraseType          = const $ pure True
  }

newtype LlvmOptions = LlvmOptions
  {flagLlvmCompile :: Bool
  }deriving (Generic, NFData)


data LlvmEnv = LlvmEnv {}

data LlvmModuleEnv = LlvmModuleEnv
newtype LlvmModule = LlvmModule [GrinDefinition]

type Builtins = Map I.Term BuiltinId

defaultLlvmOptions :: LlvmOptions
defaultLlvmOptions = LlvmOptions
  {flagLlvmCompile = False}

llvmCommandLineFlags :: [OptDescr (Flag LlvmOptions)]
llvmCommandLineFlags =
    [ Option []  ["llvm"] (NoArg enable)
      "compile program using the LLVM backend"
    ]
  where
    enable o = pure o{ flagLlvmCompile = True }

llvmPreCompile :: LlvmOptions -> TCM LlvmEnv
llvmPreCompile _ = pure LlvmEnv

-- TODO need to filter unreachable functions
llvmCompileDef :: LlvmEnv
               -> LlvmModuleEnv
               -> IsMain
               -> Definition
               -> TCM (Maybe GrinDefinition)
llvmCompileDef env menv isMainModule def@Defn{defName, defType, theDef=Function{funCompiled}} = do

    -- let term = case prettyShow defName of
    --       "DownFrom.downFrom" -> env.terms !! 1
    --       "DownFrom.sum"      -> env.terms !! 2
    --       "DownFrom.main"     -> env.terms !! 3
    --       s                   -> error $ "ERROR UNREACHABLE " ++ show defType

    -- when (isMain == IsMain && isNamedMain) $ do
    --   pure ()
      -- liftIO $ putStrLn $ "IS MAIN: " ++ prettyShow defName

    -- (x :: NameId) <- fresh
    -- (i :: Gid) <- freshGid
    -- liftIO $ putStrLn $ "FRESH: " ++ prettyShow i

    treeless <- maybeM __IMPOSSIBLE__ normalizeNames $ toTreeless LazyEvaluation defName
    let (arity, treeless') = skipLambdas treeless

    term' <- treelessToGrin isMain (simplifyApp treeless') arity
    gArgs <- replicateM arity freshAbs
    gReturn <- boolToMaybe (not isMain) <$> freshAbs

    let gDef  = GrinDefinition
          { gType = Just defType
          , gName = prettyShow defName
          , gArity = arity
          , gTerm = term'
          , gTreeless = Just $ simplifyApp treeless
          , gArgs = gArgs
          , gReturn = gReturn
          }

    pure $ Just gDef
  where
    isMain = isNamedMain && isMainModule == IsMain
    isNamedMain = "main" == (prettyShow . nameConcrete . qnameName) defName


llvmCompileDef env menv isMain def@Defn{defName, theDef=Primitive{primName}} = do
  -- liftIO $ putStrLn $ render $ sep [text "Primitive:", pretty defName, pretty primName]
  pure Nothing

llvmCompileDef env menv isMain def@Defn{defName, theDef=Datatype{dataCons}} = do
  -- liftIO $ putStrLn $ render $ sep [text "Datatype: ", pretty defName, pretty dataCons]
  pure Nothing

llvmCompileDef env menv isMain def@Defn{defName, defType, theDef=Constructor{conArity, conData}} = do
  -- liftIO $ putStrLn $ render $ sep [text "Constructor:", pretty defName, pretty conData, pretty conArity]
  pure Nothing

llvmCompileDef env menv isMain def = pure Nothing


llvmPostModule :: LlvmEnv
               -> LlvmModuleEnv
               -> IsMain
               -> TopLevelModuleName
               -> [Maybe GrinDefinition]
               -> TCM LlvmModule
llvmPostModule _ _ _ topModName defs =
  pure $ LlvmModule $ catMaybes defs



llvmPostCompile :: LlvmEnv
                -> IsMain
                -> Map TopLevelModuleName LlvmModule
                -> TCM ()
llvmPostCompile env _ mods = do

    liftIO $ putStrLn "\n------------------------------------------------------------------------"
    liftIO $ putStrLn "-- * Treeless"
    liftIO $ putStrLn "------------------------------------------------------------------------\n"
    liftIO $ putStrLn $ intercalate "\n\n" $ for defs $ \GrinDefinition{gName, gTreeless, gType} ->
      render $ vcat
        [ pretty gName <+> text ":" <+> pretty gType
        , pretty gName <+> text "=" <+> pretty gTreeless
        ]

    liftIO $ putStrLn "\n------------------------------------------------------------------------"
    liftIO $ putStrLn "-- * GRIN"
    liftIO $ putStrLn "------------------------------------------------------------------------\n"
    liftIO $ putStrLn (intercalate "\n\n" $ map prettyShow defs)

    liftIO $ putStrLn "\n------------------------------------------------------------------------"
    liftIO $ putStrLn "-- * Heap points-to analysis"
    liftIO $ putStrLn "------------------------------------------------------------------------\n"
    -- liftIO $ putStrLn "Variable table:"
    -- liftIO $ putStrLn (prettyShow $ genVariableTable defs)

    let (heap, env, share) = heapPointsTo' defs
    liftIO $ putStrLn "\nAbstract heap: "
    liftIO $ putStrLn $ prettyShow heap
    liftIO $ putStrLn "\nAbstract env: "
    liftIO $ putStrLn $ prettyShow env
    liftIO $ putStrLn "\nShared: "
    liftIO $ putStrLn $ prettyShow share


    -- let (heap, env, share) = heapPointsTo defs
    -- liftIO $ putStrLn "\nAbstract heap: "
    -- liftIO $ putStrLn $ prettyShow heap
    -- liftIO $ putStrLn "\nAbstract env: "
    -- liftIO $ putStrLn $ prettyShow env
    let cxt = solveEquations' defs heap env
    let heap' = sortAbsHeap' cxt.fHeap
    let env' = sortAbsEnv' cxt.fEnv
    liftIO $ putStrLn "\nSolved heap: "
    liftIO $ putStrLn $ prettyShow heap'
    liftIO $ putStrLn "\nSolved env: "
    liftIO $ putStrLn $ prettyShow env'




    -- builtinThings <- getsTC $ foldr1 Map.union .
    --                  (\m -> map (iBuiltin . maybe __IMPOSSIBLE__ miInterface . flip Map.lookup m) $ Map.keys mods)
    --                  . stDecodedModules . stPersistentState
    --
    -- let builtins = Map.fromList $ catMaybes $ flip map (Map.toList builtinThings) $ \case
    --       (BuiltinName n, Builtin t) -> Just (t, n)
    --       _                          -> Nothing

    -- liftIO $ putStrLn $ render $ vcat [text "BUILTINS:", nest 2 $ pretty builtins]

    -- let llvmInstructions = runReader (evalStateT (grinToLlvm defs) initEnv)
    --                      $ initCxt defs builtins
    -- liftIO $ putStrLn "\n------------------------------------------------------------------------"
    -- liftIO $ putStrLn "-- * LLVM"
    -- liftIO $ putStrLn "------------------------------------------------------------------------\n"
    -- liftIO $ putStrLn $ intercalate "\n\n" $ map prettyShow llvmInstructions

    -- liftIO $ putStrLn "\n------------------------------------------------------------------------"
    -- liftIO $ putStrLn "-- * Manual GRIN"
    -- liftIO $ putStrLn "------------------------------------------------------------------------\n"
    -- liftIO $ putStrLn $ intercalate "\n\n" $ map prettyShow manual



  where
    -- mkEl unEl = I.El {unEl=unEl, _getSort=undefined}
    -- mkDom unDom = I.Dom {domTactic=undefined, domName=undefined, domIsFinite=undefined, domInfo=undefined, unDom=unDom}
    -- mkNoAbs unAbs = I.NoAbs {absName=undefined, unAbs=unAbs}

    defs = concatMap (\(LlvmModule xs) -> xs) (Map.elems mods)

  -- go (acc, I.El _ (I.Pi I.Dom{unDom} I.NoAbs{unAbs})) = go (snoc acc unDom , unAbs)



-----------------------------------------------------------------------
-- * Treeless transformations
-----------------------------------------------------------------------

-- | Skip initial lambdas
skipLambdas :: TTerm -> (Int, TTerm)
skipLambdas = go . (0, ) where
  go (n, TLam t) = go (n + 1, t)
  go p           = p

-- | Simplify complicated applications
--
-- f a (g b) (h c)
-- >>>
-- let b' = g b in
-- let c' = h c in
-- f a b' c'
simplifyApp :: TTerm -> TTerm
simplifyApp t | (f, as@(_:_)) <- tAppView t  = go f as where
  -- Create a let expression and raise de Bruijn indices, until
  -- simple application
  go f as
    | Just (before, t, after) <- splitArgs as =
      simplifyApp $ mkLet t
                  $ mkTApp (raise1 f)
                  $ raise1 before ++ TVar 0 : raise1 after
    | otherwise = mkTApp f as

  raise1 :: Subst a => a -> a
  raise1 = applySubst $ raiseS 1

  -- Split arguments on the first application
  splitArgs :: Args -> Maybe (Args, TTerm, Args)
  splitArgs []              = Nothing
  splitArgs (a@TApp{} : as) = Just ([], a, as)
  splitArgs (a : as)
    | Just (before, t, after) <- splitArgs as = Just (a:before, t, after)
    | otherwise = Nothing

simplifyApp t = case t of
  TCase n info def alts -> TCase n info (simplifyApp def) (simplifyAppAlts alts)
  TLet t1 t2            -> on TLet simplifyApp t1 t2
  TLit _                -> t
  TCon _                -> t
  TError _              -> t
  TVar _                -> t
  TPrim _               -> t
  TDef _                -> t
  TLam t                -> TLam $ simplifyApp t
  _                     -> error $ "TODO " ++ show t

simplifyAppAlts :: [TAlt] -> [TAlt]
simplifyAppAlts = map go where
  go alt@TACon{aBody} = alt{aBody = simplifyApp aBody}
  go alt@TALit{aBody} = alt{aBody = simplifyApp aBody}
  go _                = error "TODO"


-----------------------------------------------------------------------
-- * GRIN code generation
-----------------------------------------------------------------------

-- TODO
-- • Refactor (use Reader instead of State)
-- • Reuse evaluated variables
-- • Fill in rest of the patterns
-- • Assign an unique tag (@tTag@) instead of 0?

-- Preconditions:
-- • Separate applications
-- • Lambda lifted
-- • No polymorphic functions?
-- • Saturated constructors
treelessToGrin :: Bool -> TTerm -> Int -> TCM Term
treelessToGrin isMain t arity = evalStateT (rScheme t) $ initGEnv arity isMain


rScheme :: TTerm -> G Term
rScheme (TCase n CaseInfo{caseType=CTNat} def alts) = do
  alts' <- mapM (aScheme . raise 1) alts
  def' <- rScheme (raise 1 def)
  Bind (eval n) <$> altNode natTag (Case 0 def' alts')

rScheme (TCase n CaseInfo{caseType=CTData _} def alts) = do
  alts' <- mapM (aScheme . raise 1) alts
  def' <- rScheme (raise 1 def)
  Bind (eval n) <$> altVar (Case 0 def' alts')

-- | 𝓡 [_[]_] = unit (C_[]_)
rScheme (TCon q) = pure $ Unit $ Node tag [] where
  tag = CTag{tTag = 0, tCon = prettyShow q, tArity = 0}
rScheme (TLit lit) = pure $ Unit $ Node natTag [Lit lit]
rScheme (TError TUnreachable) = pure $ Error TUnreachable



rScheme (TApp t as) = do
    isMain <- gets isMain
    alt <- altVar . Bind (eval 0) =<< altNode natTag (printf 0)
    let res t
          | isMain = Bind t alt
          | otherwise = t
    case t of
      TPrim prim -> value <$> appPrim res prim as
      TDef q     -> value <$> appDef res q as
      TCon q     -> value <$> appCon res q as
      _          -> __IMPOSSIBLE__
  where


    -- | 𝓡 [x + y] = eval @1 ; λ Cnat #1 →
    --               eval @1 ; λ Cnat #1 →
    --               add @1 @0 ; λ #1 →
    --               unit @0
    appPrim res prim as = do
        fin <- Bind (App (Prim prim) vs) <$> altVar (res $ Unit $ Var 0)
        evals' <- foldrM (\t ts -> Bind t <$> altVar ts) fin evals
        pure $ mkWithOffset (length evals) evals'
      where
        (evals, vs) =  foldr f ([], []) as

        f (TLit lit) (es, vs) = (es, Lit lit : vs)
        f (TVar n)   (es, vs) = (eval (on (-) length evals es + n - 1)  : es, Var (length es) : vs)
        f _          _        = __IMPOSSIBLE__



    -- | 𝓡 [foo x y] = foo x y
    appDef res q as = do
      let
        nLits = foldl (\n -> \case{TLit _ -> succ n ; _ -> n}) 0 as

        f (TLit lit) (ss, vs) = do
            s <- store $ Node natTag [Lit lit]
            pure (s : ss, Var (length ss) : vs)
        f (TVar n)   (ss, vs) = pure (ss, Var (n + nLits) : vs)
        f _          _        = __IMPOSSIBLE__

      (stores, vs) <- foldrM f ([], []) as
      let fin = res $ App (Def $ prettyShow q) vs
      stores' <- foldrM (\t ts -> Bind t <$> altVar ts) fin stores

      pure $ mkWithOffset nLits stores'

    -- | 𝓡 [_∷_ x xs] = unit (C_∷_ @1 @0)
    appCon res q as = do
        let
          nLits = foldl (\n -> \case{TLit _ -> succ n ; _ -> n}) 0 as

          f (TLit lit) (ss, vs) = do
            s <- store $ Node natTag [Lit lit]
            pure (s : ss, Var (length ss) : vs)
          f (TVar n)   (ss, vs) = pure (ss, Var (n + nLits) : vs)
          f _          _        = __IMPOSSIBLE__

        (stores, vs) <- foldrM f ([], []) as
        let fin = res $ Unit $ Node tag vs
        stores' <- foldrM (\t ts -> Bind t <$> altVar ts) fin stores

        pure $ mkWithOffset (length stores) stores'
      where
        tag = CTag{tTag = 0, tCon = prettyShow q, tArity = length as}








-- | 𝓡 [let t1 in t2] = 𝓒 [t1] ; λ #1 → 𝓡 [t2]
rScheme (TLet t1 t2)
  | TApp t as <- t1 = do
    WithOffset{value=t1', offset} <- cSchemeApp t as
    t2' <- rScheme $ raiseFrom 1 offset t2
    pure $ t1' t2'

  | TLet t1 t2 <- t1 = do
    t1' <- cScheme t1
    t2' <- rScheme t2
    Bind t1' <$> altVar t2'

rScheme t = error $ "TODO rScheme " ++ show t

aScheme :: TAlt -> G Alt
aScheme TALit{aLit, aBody} = do
  aBody' <- rScheme aBody
  pure $ AltLit aLit aBody'

aScheme TACon{aCon, aArity, aBody} = do
    aBody' <- rScheme aBody
    altNode tag aBody'
  where
    tag = CTag{tTag = 0, tCon = prettyShow aCon, tArity = aArity}

aScheme alt                = error $ "TODO aScheme " ++ show alt

cScheme :: TTerm -> G Term
cScheme t = error $ "TODO cScheme " ++ show t

-- | 𝓒 [foo x y] = store (Ffoo @1 @0)
--
--   𝓒 [a + 4] = store (Cnat 4) λ #1 →
--               store (Prim.add @1 @0)
cSchemeApp :: TTerm -> Args -> G (WithOffset (Term -> Term))
cSchemeApp t as = do
    let
        nLits = foldl (\n -> \case{TLit _ -> succ n ; _ -> n}) 0 as
        f (TLit lit) (ss, vs) = do
          s <- store $ Node natTag [Lit lit]
          pure (s : ss, Var (length ss) : vs)
        f (TVar n)   (ss, vs) = pure (ss, Var (n + nLits) : vs)
        f _          _        = __IMPOSSIBLE__


    (stores, vs) <-  foldrM f ([], []) as

    -- let fin' = res $ Unit $ Node tag vs
    t <- store $ Node tag vs
    abs <- freshAbs
    let fin = Bind t . AltVar abs
    stores' <- foldrM (\t ts -> (\abs -> Bind t . AltVar abs . ts) <$> freshAbs) fin stores
    pure $ mkWithOffset nLits stores'
  where
  tag
    | TDef q <- t = FTag{tTag = 0, tDef = prettyShow q, tArity = length as}
    | TPrim prim <- t =  FTag {tTag=0, tDef=primStr prim, tArity=length as}
    | otherwise = __IMPOSSIBLE__



natTag :: Tag
natTag = CTag{tTag = 0, tCon = "nat" , tArity = 1}

cnat :: Value
cnat = VNode natTag [Bas]

eval :: Int -> Term
eval = App (Def "eval") . singleton . Var

printf :: Int -> Term
printf = App (Def "printf") . singleton . Var


primStr :: TPrim -> String
primStr PAdd64 = "Prim.add"
primStr PAdd   = "Prim.add"
primStr PSub64 = "Prim.sub"
primStr PSub   = "Prim.sub"
primStr p      = error $ "TODO primStr " ++ show p

data GVarInfo = GVarInfo
  { isEvaluated      :: Bool
  , evaluationOffset :: Maybe Int -- negative
  }

mkVar :: GVarInfo
mkVar = GVarInfo{isEvaluated=False, evaluationOffset=Nothing}

data GEnv = GEnv
  { gVars  :: [GVarInfo]
  , isMain :: Bool
  }

initGEnv :: Int -> Bool -> GEnv
initGEnv arity isMain = GEnv {gVars = replicate arity mkVar, isMain = isMain}

type G = StateT GEnv TCM


-- type G = State GEnv



data WithOffset a = WithOffset
  { offset :: Int
  , value  :: a
  }

mkWithOffset :: Int -> a -> WithOffset a
mkWithOffset n a = WithOffset{offset = n, value = a}

-----------------------------------------------------------------------
-- * GRIN heap points-to analysis
-----------------------------------------------------------------------

-- • "determine for each call to eval, a safe approximation to what different node
--   values (or rather tags) that eval might find when it fetches a node from the
--   heap via its argument pointer." (s. 67)
--
-- • The abstract heap (also called store) maps locations to a set of nodes.
--   Locations are defined as {1, 2, ...,maxloc} where maxloc is total number
--   of store operations. Additionally, locations with F-tags also contain the
--   possible return types of the function. This is due to the eval function
--   which will be generated later, but has the ability to @update@ the node with
--   the evaluated value.
--
-- • The Abstract enviroment maps variables to a set of values:
--
-- • The heap points-to analysis also incorporates a sharing analysis,
--   which determines for each abstract location if it is shared or unique.
--   An abstract location is shared if a concrete instance of the abstract
--   location is subject to @fetch@ more than once. In practice, this is
--   evident if the location is a possible value of a variable which is used
--   more than once.

-- TODO
-- • Sharing analysis
-- • Refactor


heapPointsTo' :: [GrinDefinition] -> (AbsHeap', AbsEnv', Set Gid)
heapPointsTo' defs = (heap', env', shared)
  where
    heap' = AbsHeap' $ sortOn fst $ unAbsHeap' equationsState.absHeap'
    env' = AbsEnv' $ sortOn fst $ unAbsEnv' equationsState.absEnv'
    shared = equationsState.shared'
    equationsState =
      foldl1 (<>) $ for defs $ \def ->
        runReader (deriveEquations' def.gTerm) $
        initHCxt defs def






newtype Multiplicities = Multiplicities{unMultiplicities :: Map Abs Int}

instance Semigroup Multiplicities where
  (<>) x = Multiplicities . on (Map.unionWith (+)) unMultiplicities x

instance Monoid Multiplicities where
  mempty = Multiplicities mempty

instance Pretty Multiplicities where
  pretty (Multiplicities ms) =
      vcat $ map prettyEntry $ Map.toList ms
    where
      prettyEntry (x, n) =
        text (prettyShow x ++ ":") <+> pretty n

countMultiplicities :: GrinDefinition -> Multiplicities
countMultiplicities def = evalState (go def.gTerm) def.gArgs where
  go :: Term -> State [Abs] Multiplicities
  go (Case i t alts) =
    goVal (Var i) <> (foldl (<>) mempty <$> mapM goAlt alts) <> go t
  go (Bind t alt) = go t <> goAlt alt
  go (Store _ v) = goVal v
  go (Unit v) = goVal v
  go (App v vs) = goVal v <> (foldl (<>) mempty <$> mapM goVal vs)
  go (Fetch v) = goVal v
  go (Update v1 v2) = on (<>) goVal v1 v2
  go (Error _) = pure mempty

  goAlt :: Alt -> State [Abs] Multiplicities
  goAlt (AltVar abs t)     = modify (abs:) *> go t
  goAlt (AltNode _ abss t) = modify (reverse abss ++) *> go t
  goAlt (AltLit _ t)       = go t
  goAlt (AltEmpty t)       = go t

  goVal :: Val -> State [Abs] Multiplicities
  goVal (Var n) = gets $ (!! n) <&> \abs -> Multiplicities $ Map.singleton abs 1
  goVal (Lit _) = pure mempty
  goVal (Node _ vs) = foldl (<>) mempty <$> mapM goVal vs
  goVal (Def _) = pure mempty
  goVal (Prim _) = pure mempty
  goVal Empty = pure mempty



newtype AbsHeap = AbsHeap{unAbsHeap :: [(Location, Value)]} deriving Eq
newtype AbsEnv = AbsEnv{unAbsEnv :: [(Variable, Value)] } deriving Eq

sortAbsHeap :: AbsHeap -> AbsHeap
sortAbsHeap (AbsHeap heap) = AbsHeap $ sortOn fst heap

sortAbsEnv :: AbsEnv -> AbsEnv
sortAbsEnv (AbsEnv env) = AbsEnv $ sortOn fst env

sortAbsHeap' :: AbsHeap' -> AbsHeap'
sortAbsHeap' (AbsHeap' heap) = AbsHeap' $ sortOn fst heap

sortAbsEnv' :: AbsEnv' -> AbsEnv'
sortAbsEnv' (AbsEnv' env) = AbsEnv' $ sortOn fst env


instance Pretty AbsHeap where
  pretty (AbsHeap heap) =
      vcat $ map prettyEntry heap
    where
      prettyEntry :: (Location, Value) -> Doc
      prettyEntry (x, v) =
            pretty x
        <+> text "→"
        <+> pretty v


instance Pretty AbsEnv where
  pretty (AbsEnv env) =
      vcat $ map prettyEntry env
    where
      prettyEntry :: (Variable, Value) -> Doc
      prettyEntry (x, v) =
            pretty x
        <+> text "→"
        <+> pretty v

data Value = VNode Tag [Value]
           | Bas
           | Location Location
           | Variable Variable
           | Pick Value Tag Int
           | EVAL Value
           | FETCH Value
           | Union Value Value
           | Loc Loc
           | Abs Abs
             deriving (Eq, Ord)

instance Pretty Value where
  pretty (VNode tag vs) =
    pretty tag <+> text ("[" ++ intercalate ", " (map prettyShow vs) ++ "]")
  pretty Bas            = text "BAS"
  pretty (Location loc) = pretty loc
  pretty (Variable x)   = pretty x
  pretty (Pick v tag i) = pretty v <+> text "↓" <+> pretty tag <+> text "↓" <+> pretty i
  pretty (EVAL v) = text $ "EVAL(" ++ prettyShow v ++ ")"
  pretty (FETCH v) = text $ "FETCH(" ++ prettyShow v ++ ")"
  pretty (Union v1 v2) = pretty v1 <+> text "∪" <+> pretty v2
  pretty (Abs abs) = pretty abs
  pretty (Loc loc) = pretty loc

vnodeView :: Value -> Maybe (Tag, [Value])
vnodeView (VNode tag vs) = Just (tag, vs)
vnodeView _              = Nothing

mkUnion :: Value -> Value -> Value
mkUnion a b
  | a == b = a
  | otherwise = Union a b

newtype Location = MkLocation{unLocation :: Int} deriving (Eq, Ord, Enum)

instance Pretty Location where
  pretty (MkLocation l) = text $ "l" ++ prettyShow l

newtype Variable = MkVariable{unVariable :: Int} deriving (Eq, Ord, Enum)

instance Pretty Variable where
  pretty (MkVariable n) = text $ "x" ++ prettyShow n


data CxtReader = CxtReader
  { gDef          :: GrinDefinition
  , variableTable :: VariableTable
  }

data CxtState = CxtState
  { heap         :: AbsHeap
  , env          :: AbsEnv
  , lastVariable :: Variable
  , lastLocation :: Location
  }

initCxtState :: CxtState
initCxtState = CxtState
  { heap=AbsHeap []
  , env=AbsEnv []
  , lastVariable=MkVariable (-1) -- ew TODO use Maybe
  , lastLocation=MkLocation (-1)
  }

type H = ReaderT CxtReader (State CxtState)

-- | Generate variable table
genVariableTable :: [GrinDefinition] -> VariableTable
genVariableTable defs = snd $ foldl genEntry (0, VariableTable []) defs where

  genEntry :: (Int, VariableTable) -> GrinDefinition -> (Int, VariableTable)
  genEntry (n, VariableTable vt) GrinDefinition{gTerm, gArity, gName} =
      (n'', VariableTable $ snoc vt (gName, map MkVariable [n .. n'' - 1]))
    where
      n'' | isSuffixOf "main" gName = n'
          | otherwise = n' + 1
      n' = n + gArity + countVars gTerm


  countVars :: Term -> Int
  countVars (Bind t alt)    = countVars t + countVarsAlt alt
  countVars (Case _ t alts) = countVars t + sum (map countVarsAlt alts)
  countVars _               = 0

  countVarsAlt :: Alt -> Int
  countVarsAlt (AltVar _ t)       = 1 + countVars t
  countVarsAlt (AltNode _ gids t) = length gids + countVars t
  countVarsAlt (AltEmpty t)       = countVars t
  countVarsAlt (AltLit _ t)       = countVars t

isSuffixOf :: String -> String -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf s1 s2@(_:ss)
  | on (==) length s1 s2 = s1 == s2
  | on (<) length s1 s2  = isSuffixOf s1 ss
  | otherwise = False

newtype VariableTable = VariableTable { unVariableTable :: [(String, [Variable])] }


instance Pretty VariableTable where
  pretty (VariableTable entries) =
      vcat $ map prettyEntry entries
    where
      prettyEntry (n, xs) =
        text n <+> "→" <+> vcat (map pretty xs)



newtype AbsHeap' = AbsHeap'{unAbsHeap' :: [(Loc, Value)]} deriving Eq

instance Semigroup AbsHeap' where
  (<>) = composeAbsHeap

composeAbsHeap :: AbsHeap' -> AbsHeap' -> AbsHeap'
composeAbsHeap (AbsHeap' h1) (AbsHeap' h2) =
    AbsHeap' $ (h1 \\ common) ++ common' ++ (h2 \\ common)
  where
    common = let insec = intersectBy (on (==) fst) in insec h1 h2 ++ insec h2 h1
    locs = nub $ map fst common
    common'
      | null common = []
      | otherwise =
        for locs $ \loc ->
          let vs = mapMaybe (\(loc', v) -> boolToMaybe (loc'==loc) v) common in
          (loc, foldl1 mkUnion vs)


instance Semigroup AbsEnv' where
  (<>) = composeAbsEnv

composeAbsEnv :: AbsEnv' -> AbsEnv' -> AbsEnv'
composeAbsEnv (AbsEnv' e1) (AbsEnv' e2) =
    AbsEnv' $ (e1 \\ common) ++ common' ++ (e2 \\ common)
  where
    common = let insec = intersectBy (on (==) fst) in insec e1 e2 ++ insec e2 e1
    abss = nub $ map fst common
    common'
      | null common = []
      | otherwise =
        for abss $ \abs ->
          let vs = mapMaybe (\(abs', v) -> boolToMaybe (abs'==abs) v) common in
          (abs, foldl1 mkUnion vs)

newtype AbsEnv' = AbsEnv'{unAbsEnv' :: [(Abs, Value)]}deriving Eq

instance Pretty AbsHeap' where
  pretty (AbsHeap' heap) =
      vcat $ map prettyEntry heap
    where
      prettyEntry :: (Loc, Value) -> Doc
      prettyEntry (x, v) =
            pretty x
        <+> text "→"
        <+> pretty v


instance Pretty AbsEnv' where
  pretty (AbsEnv' env) =
      vcat $ map prettyEntry env
    where
      prettyEntry :: (Abs, Value) -> Doc
      prettyEntry (x, v) =
            pretty x
        <+> text "→"
        <+> pretty v




newtype SharingHeap = SharingHeap{unSharingHeap :: Map Loc Bool}

instance Semigroup SharingHeap where
  (<>) = composeSharingHeap

composeSharingHeap :: SharingHeap -> SharingHeap -> SharingHeap
composeSharingHeap = on (SharingHeap .: Map.unionWith (||)) unSharingHeap

composeSharingHeap' :: AbsHeap -> AbsEnv -> SharingHeap -> SharingHeap -> SharingHeap
composeSharingHeap' heap env (SharingHeap sh1) (SharingHeap sh2) = undefined
  where
    x = mapMaybe snd $ Map.toList $ Map.intersectionWithKey f sh1 sh2
    f _ False False = Nothing
    f _ True True   = Nothing
    f k False True  = Just k
    f _ True False  = __IMPOSSIBLE__

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

type H' = Reader HCxt

data HCxt = HCxt
  { absHeap    :: AbsHeap'
  , absEnv     :: AbsEnv'
  , defs       :: [GrinDefinition]
  , abss       :: [Abs]
  , locs       :: [Loc]
  , shared     :: Set Gid
  , currentDef :: GrinDefinition
  }

initHCxt :: [GrinDefinition] -> GrinDefinition -> HCxt
initHCxt defs currentDef =
    HCxt
      { absHeap = AbsHeap' []
      , absEnv = AbsEnv' []
      , defs = defs
      , abss = currentDef.gArgs
      , locs = []
      , shared = shared
      , currentDef = currentDef
      }
  where
    shared =
      Set.fromList $
      mapMaybe (\(MkAbs gid, n) -> boolToMaybe (n > 1) gid) $
      Map.toList $
      unMultiplicities $
      foldl (\ms def -> ms <> countMultiplicities def) mempty defs


instance Semigroup HRet where
  h1 <> h2 =
    let heap = on (<>) absHeap' h1 h2
        env = on (<>) absEnv' h1 h2 in
    HRet
      { absHeap' = heap
      , absEnv'  = env
      , shared' = on (composeShared heap env) shared' h1 h2
      }

data HRet = HRet
  { absHeap' :: AbsHeap'
  , absEnv'  :: AbsEnv'
  , shared'  :: Set Gid
  }

composeShared :: AbsHeap' -> AbsEnv' -> Set Gid -> Set Gid -> Set Gid
composeShared heap env = updateShared heap env .: Set.union

updateShared :: AbsHeap' -> AbsEnv' -> Set Gid -> Set Gid
updateShared absHeap absEnv shared
  | shared' == shared = shared
  | otherwise = updateShared absHeap absEnv shared'
  where
    shared' = foldl addGids shared vs
    vs =
      nub $
      mapMaybe (\(MkLoc gid, v) -> boolToMaybe (Set.member gid shared) v) (unAbsHeap' absHeap) ++
      mapMaybe (\(MkAbs gid, v) -> boolToMaybe (Set.member gid shared) v) (unAbsEnv' absEnv)

makeShared :: AbsHeap' -> AbsEnv' -> Set Gid -> Value -> Set Gid
makeShared heap env = updateShared heap env .: addGids

addGids :: Set Gid -> Value -> Set Gid
addGids s (Loc (MkLoc gid)) = Set.insert gid s
addGids s (Abs (MkAbs gid)) = Set.insert gid s
addGids s (VNode _ vs)      = foldl addGids s vs
addGids s Bas               = s
addGids s (Union v1 v2)     = addGids (addGids s v1) v2
addGids s (Pick v _ _)      = addGids s v
addGids s (EVAL v)          = addGids s v
addGids s (FETCH v)         = addGids s v
addGids _ _                 = __IMPOSSIBLE__

localAbss :: MonadReader HCxt m => [Abs] -> m a -> m a
localAbss = foldl (\f abs -> f . localAbs abs) id

localAbs :: MonadReader HCxt m => Abs -> m a -> m a
localAbs abs = local $ \cxt -> cxt{abss = abs : cxt.abss}

localLoc :: MonadReader HCxt m => Loc -> m a -> m a
localLoc loc = local $ \cxt -> cxt{locs = loc : cxt.locs}

-- TODO use lenses?
-- | Adds l → v to the abstract heap and makes v shared if x is shared
localAbsHeap :: MonadReader HCxt m => (Loc, Value) -> m a -> m a
localAbsHeap (loc, v) = local $ \cxt ->
  let heap = unAbsHeap' cxt.absHeap
      heap'
        | any ((==loc) . fst) heap =
          for heap $ \(loc', v') ->
            if loc == loc' then (loc', mkUnion v' v ) else (loc', v')
        | otherwise = insert (loc, v) $ unAbsHeap' cxt.absHeap
      absHeap = AbsHeap' heap' in

  applyWhen
    (Set.member (unLoc loc) cxt.shared)
    (\cxt -> cxt{shared = makeShared absHeap cxt.absEnv cxt.shared v})
    cxt{absHeap = absHeap}

-- | Adds x → v to the abstract enviroment and makes v shared if x is shared
localAbsEnv :: MonadReader HCxt m => (Abs, Value) -> m a -> m a
localAbsEnv (abs, v) = local $ \cxt ->
  let env = unAbsEnv' cxt.absEnv
      env'
        | any ((==abs) . fst) env =
          for env $ \(abs', v') ->
            if abs == abs' then (abs', mkUnion v' v ) else (abs', v')
        | otherwise = insert (abs, v) env
      absEnv' = AbsEnv' env' in

  applyWhen
    (Set.member (unAbs abs) cxt.shared)
    (\cxt -> cxt{shared = makeShared cxt.absHeap absEnv' cxt.shared v})
    cxt{absEnv = absEnv'}



deriveEquations' :: Term -> H' HRet
deriveEquations' term = case term of

    Bind (Store loc (Node tag vs)) (AltVar abs t) -> do
        vs' <- mapM valToValue vs
        defs <- asks defs
        case findName defs tag of
          Nothing ->
            localLoc loc $
            localAbs abs $
            localAbsEnv (abs, Loc loc) $
            localAbsHeap (loc, VNode tag vs') $
            deriveEquations' t

          -- non-primitve F-tag or P-tag
          Just def -> do
            h <- localLoc loc $
                 localAbs abs $
                 localAbsEnv (abs, Loc loc) $
                 localAbsHeap (loc, VNode tag vs'  ) $
                 foldl (.: localAbsEnv) id (zip def.gArgs vs') $
                 deriveEquations' t

            if Set.member (unLoc loc) h.shared' then
              localLoc loc $
              localAbs abs $
              localAbsEnv (abs, Loc loc) $
              localAbsHeap (loc, VNode tag vs' `mkUnion` Abs (fromMaybe __IMPOSSIBLE__ def.gReturn)) $
              foldl (.: localAbsEnv) id (zip def.gArgs vs') $
              deriveEquations' t
            else
              pure h
      where
        findName defs FTag{tDef} = find ((==tDef) . gName) defs
        findName defs PTag{tDef} = find ((==tDef) . gName) defs
        findName _ _             = Nothing

        valToValue :: MonadReader HCxt m => Val -> m Value
        valToValue (Var n) = Abs . fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup' n
        valToValue (Lit _) = pure Bas
        valToValue  _      = __IMPOSSIBLE__

    Bind (App (Def "eval") [Var n]) (AltVar abs (Case 0 t alts)) ->
      deBruijnLookup' n <&> EVAL . FETCH . Abs . fromMaybe __IMPOSSIBLE__ >>= \v ->
        localAbs abs $
        localAbsEnv (abs, v) $ do
          hs <- mapM (deriveEquationsAlt abs) alts
          h <- deriveEquations' t
          pure $ foldl (<>) h hs
      where
        deriveEquationsAlt :: Abs -> Alt -> H' HRet
        deriveEquationsAlt abs1 (AltNode tag abss t) =
          let localPicks = zipWith (\abs2 i -> localAbsEnv (abs2, Pick (Abs abs1) tag i)) abss [0 ..] in
          foldl (.: localAbs) id abss $
          foldl (.) id localPicks $
          deriveEquations' t
        deriveEquationsAlt _ (AltEmpty t) = deriveEquations' t
        deriveEquationsAlt _ AltVar{}     = __IMPOSSIBLE__ -- TODO investigate if this is possible (thesis indicate it is not)
        deriveEquationsAlt _ AltLit{}     = __IMPOSSIBLE__

    Bind (App (Def "eval") [Var _]) (AltNode tag [abs] (Case 0 t alts)) | tag == natTag ->
        localAbs abs $
        localAbsEnv (abs, Bas) $ do
          hs <- mapM deriveEquationsAlt alts
          h <- deriveEquations' t
          pure $ foldl (<>) h hs
      where
        deriveEquationsAlt (AltLit _ t) = deriveEquations' t
        deriveEquationsAlt (AltEmpty t) = deriveEquations' t
        deriveEquationsAlt AltVar{}     = __IMPOSSIBLE__ -- TODO investigate if this is possible (thesis indicate it is not)
        deriveEquationsAlt AltNode{}    = __IMPOSSIBLE__

    Bind (App (Def "eval") [Var n]) (AltVar abs t) ->
      deBruijnLookup' n <&> EVAL . FETCH . Abs . fromMaybe __IMPOSSIBLE__  >>= \v ->
        localAbs abs $
        localAbsEnv (abs, v) $
        deriveEquations' t

    Bind (App (Def "eval") [Var _]) (AltNode tag [abs] t) | tag == natTag ->
      localAbs abs $
      localAbsEnv (abs, Bas) $
      deriveEquations' t

   -- TODO add return value of def?
    Bind (App (Def defName) vs) (AltVar abs t) -> do
      gArgs <- asks $ maybe __IMPOSSIBLE__ gArgs . find ((defName==) . gName) . defs
      mapM valToValue vs >>= \vs' ->
        localAbs abs $
        foldl (.: localAbsEnv) id (zip gArgs vs') $
        deriveEquations' t
      where
        valToValue :: MonadReader HCxt m => Val -> m Value
        valToValue (Var n) = Abs . fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup' n
        valToValue (Lit _) = pure Bas
        valToValue  _      = __IMPOSSIBLE__

   -- TODO add return value of def?
   -- TODO add arguments
    Bind (App (Def defName) vs) (AltNode tag abss t) ->
      mapM valToValue vs >>= \vs' ->
        localAbss abss $
        foldl (.: localAbsEnv) id (zip abss vs') $
        deriveEquations' t
      where
        valToValue :: MonadReader HCxt m => Val -> m Value
        valToValue (Var n) = Abs . fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup' n
        valToValue (Lit _) = pure Bas
        valToValue  _      = __IMPOSSIBLE__

    Bind (App (Prim _) _) (AltVar abs t)  ->
      localAbs abs $
      localAbsEnv (abs, cnat) $
      deriveEquations' t

    Bind (App (Prim _) _) (AltNode tag [abs] t) | tag == natTag ->
      localAbs abs $
      localAbsEnv (abs, Bas) $
      deriveEquations' t


    Unit v -> do
        abs <- asks $ fromMaybe __IMPOSSIBLE__ . gReturn . currentDef
        v' <- valToValue v
        localAbs abs $ localAbsEnv (abs, v') retHCxt
      where
        valToValue :: MonadReader HCxt m => Val -> m Value
        valToValue (Var n) = Abs . fromMaybe __IMPOSSIBLE__ <$> deBruijnLookup' n
        valToValue (Node tag vs) = VNode tag <$> mapM valToValue vs
        valToValue (Lit _) = pure Bas
        valToValue _ = __IMPOSSIBLE__

    App (Def "printf") _ -> retHCxt
    Error _ -> retHCxt

    t -> error $ "NOT IMPLEMENTED " ++ show t

retHCxt :: MonadReader HCxt m => m HRet
retHCxt =
  ask <&> \cxt ->
    HRet {absHeap' = cxt.absHeap, absEnv' = cxt.absEnv, shared' = cxt.shared}


  -- Should never happen
  -- Bind t1@(Store loc _) (AltVar abs t2) -> do
  --   h1 <- deriveEquations' t1
  --   h2 <- localLoc loc $
  --         localAbs abs $
  --         localAbsEnv (abs, Loc loc)  $
  --         deriveEquations' t2
  --   pure $ h1 <> h2

  -- Should never happen
  -- Bind (Store loc (Lit _)) (AltVar abs t) ->
  --   localLoc loc $
  --   localAbs abs $
  --   localAbsHeap (loc, Bas) $
  --   localAbsEnv (abs, Loc loc) $
  --   deriveEquations' t


deBruijnLookup' :: MonadReader HCxt m => Int -> m (Maybe Abs)
deBruijnLookup' n = asks $ (!!! n) . abss

deBruijnLookup'' :: [Abs] -> Int -> Abs
deBruijnLookup'' abss n = fromMaybe __IMPOSSIBLE__ $ abss !!! n





-- TODO

type F = Reader FixCxt

data FixCxt = FixCxt
  { fHeap :: AbsHeap'
  , fEnv  :: AbsEnv'
  } deriving Eq

instance Pretty FixCxt where
  pretty (FixCxt {fHeap, fEnv}) =
    vcat
      [ text "Abstract heap:"
      , pretty fHeap
      , text "Abstract enviroment"
      , pretty fEnv
      ]

instance Semigroup FixCxt where
  cxt1 <> cxt2 = FixCxt
    { fHeap = AbsHeap' $ on unionNub (unAbsHeap' . fHeap) cxt1 cxt2
    , fEnv = AbsEnv' $ on unionNub (unAbsEnv' . fEnv) cxt1 cxt2
    }

instance Monoid FixCxt where
  mempty = FixCxt{fHeap = AbsHeap' [], fEnv = AbsEnv' []}

unionNub :: Eq a => [a] -> [a] -> [a]
unionNub xs = union xs . filter (`notElem` xs)

-- | Non-overloaded version of '\\' (non-associative).
differenceBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
differenceBy eq =  foldl (flip $ deleteBy eq)

-- | Solve equations
solveEquations :: [GrinDefinition] -> AbsHeap' -> AbsEnv' -> (AbsHeap', AbsEnv')
solveEquations defs (AbsHeap' heapEqs) (AbsEnv' envEqs) =
    fix initCxt
  where
    envEqsLookup :: Abs -> Maybe Value
    envEqsLookup abs = lookup abs envEqs

    heapEqsLookup :: Loc -> Maybe Value
    heapEqsLookup loc = lookup loc heapEqs

    initCxt = mempty{fEnv = AbsEnv' $ map (\abs -> (abs, fromMaybe __IMPOSSIBLE__ $ envEqsLookup abs)) $ mapMaybe gReturn defs}

    fix :: FixCxt -> (AbsHeap', AbsEnv')
    fix cxt@FixCxt{fHeap, fEnv = AbsEnv' es}
      | e:_ <- differenceBy (on (==) fst) envEqs es =
        fix $ fixFixCxt $ collectNewFixCxt $ envInsert' cxt e
      | otherwise = (fHeap, AbsEnv' es)

    collectNewFixCxt :: FixCxt -> FixCxt
    collectNewFixCxt cxt
      | Nothing <- mcxt  = cxt
      | Just cxt' <- mcxt = collectNewFixCxt $ cxt <> cxt'
      where
        mcxt =
          flip runReader cxt $ liftA2 (<>)
          (go $ unAbsEnv' $ cxt.fEnv)
          (go $ unAbsHeap' $ cxt.fHeap)
        go xs = foldl (<>) Nothing <$> mapM (collectNew . snd) xs

    collectNew :: Value -> F (Maybe FixCxt)
    collectNew (Union a b) = on (<>) collectNew a b
    collectNew (Abs x) =
      maybe (Just cxt) (const Nothing) <$> envLookup x
     where
      v = fromMaybe __IMPOSSIBLE__ $ lookup x envEqs
      cxt = FixCxt {fHeap=AbsHeap' [], fEnv = AbsEnv' [(x, v)]}
    collectNew (Loc l) =
      maybe (Just cxt) (const Nothing) <$> heapLookup l
     where
      v = fromMaybe __IMPOSSIBLE__ $ lookup l heapEqs
      cxt = FixCxt{fHeap = AbsHeap' [(l, v)], fEnv = AbsEnv' []}
    collectNew (FETCH v) = collectNew v
    collectNew (EVAL v) = collectNew v
    collectNew (VNode _ vs) = foldl (<>) Nothing <$> mapM collectNew vs
    collectNew (Pick v _ _) = collectNew v
    collectNew Bas = pure Nothing
    collectNew _ = __IMPOSSIBLE__

    fixFixCxt :: FixCxt -> FixCxt
    fixFixCxt cxt
      | cxt == cxt' = cxt
      | otherwise = fixFixCxt cxt'
      where
        cxt' =
          let f cxt (l, v) = heapUpdate cxt (l, runReader (sub defs v) cxt) in
          let cxt' = foldl f cxt $ unAbsHeap' cxt.fHeap in
          let g cxt (x, v) = envUpdate cxt (x, runReader (sub defs v) cxt) in
          foldl g cxt' $ unAbsEnv' cxt'.fEnv

solveEquations' :: [GrinDefinition] -> AbsHeap' -> AbsEnv' -> FixCxt
solveEquations' defs (AbsHeap' heapEqs) (AbsEnv' envEqs) = fix initFixCxt where

  initFixCxt :: FixCxt
  initFixCxt =
    let env =
          AbsEnv' $ for (mapMaybe gReturn defs) $ \abs ->
            (abs, fromMaybe __IMPOSSIBLE__ $ envEqsLookup abs) in
    mempty{fEnv = env }

  envEqsLookup :: Abs -> Maybe Value
  envEqsLookup abs = lookup abs envEqs

  heapEqsLookup :: Loc -> Maybe Value
  heapEqsLookup loc = lookup loc heapEqs

  fix :: FixCxt -> FixCxt
  fix cxt
    | cxt == cxt' = cxt'
    | otherwise   = fix cxt'
    where
      cxt' =
        caseMaybe (nextHeapEq cxt) cxt $
          \entry -> fixCurrent $ addMissingEqs $ heapInsert' entry cxt

      fixCurrent :: FixCxt -> FixCxt
      fixCurrent cxt
        | cxt == cxt' = cxt'
        | otherwise = fixCurrent cxt'
        where
          -- Simplify and update each entry in abstract heap and enviroment
          cxt' =
            let cxt' =
                  foldl
                    (\cxt (l, v) -> absHeapUpdate l (simplify defs cxt v) cxt)
                    cxt
                    (unAbsHeap' cxt.fHeap) in
            foldl
              (\cxt (x, v) -> absEnvUpdate x (simplify defs cxt v) cxt)
              cxt'
              (unAbsEnv' cxt'.fEnv)


  -- | Returns the next missing abstract heap equation (e.g. `~l4 → FDownFrom.downFrom [~x2]`).
  nextHeapEq :: FixCxt -> Maybe (Loc, Value)
  nextHeapEq =
    listToMaybe . differenceBy (on (==) fst) heapEqs . unAbsHeap' . fHeap

  -- | Adds missing equations that are referenced in the current equations.
  addMissingEqs :: FixCxt -> FixCxt
  addMissingEqs cxt
    | cxt == cxt' = cxt'
    | otherwise = addMissingEqs cxt'
    where
      cxt' =
        foldr (<>) cxt $
          mapMaybe (collectEqs cxt) $
            map snd (unAbsHeap' cxt.fHeap) ++ map snd (unAbsEnv' cxt.fEnv)

  -- | Collect all equations refererenced equations.
  collectEqs :: FixCxt -> Value -> Maybe FixCxt
  collectEqs cxt (Union v1 v2) = on (<>) (collectEqs cxt) v1 v2
  collectEqs cxt (Abs abs)
    | Just _ <- lookup abs (unAbsEnv' cxt.fEnv) = Nothing
    | otherwise = Just $ FixCxt {fHeap=AbsHeap' [], fEnv = AbsEnv' [(abs, v)]}
    where
      v = fromMaybe __IMPOSSIBLE__ $ lookup abs envEqs

  collectEqs cxt (Loc loc)
    | Just _ <- lookup loc (unAbsHeap' cxt.fHeap) = Nothing
    | otherwise = Just $ FixCxt {fHeap = AbsHeap' [(loc, v)], fEnv = AbsEnv' []}
    where
      v = fromMaybe __IMPOSSIBLE__ $ lookup loc heapEqs

  collectEqs cxt (FETCH v) = collectEqs cxt v
  collectEqs cxt (EVAL v1) =  collectEqs cxt v1
  collectEqs cxt (VNode _ vs) = foldl (<>) Nothing $ map (collectEqs cxt) vs
  collectEqs cxt (Pick v _ _) = collectEqs cxt v
  collectEqs _ Bas = Nothing
  collectEqs _ _ = __IMPOSSIBLE__

gatherEntriesBy1 :: Ord a => (b -> b -> b) -> List1 (a, b) -> List1 (a, b) -> List1 (a, b)
gatherEntriesBy1 f xs ys =
    List1.map (foldr1 (\(a, b1) (_, b2) -> (a, f b1 b2))) $
      List1.groupAllWith1 fst $ xs <> ys

simplify :: [GrinDefinition] -> FixCxt -> Value -> Value
simplify defs FixCxt{fHeap, fEnv} = go where

  envLookup :: Abs -> Maybe Value
  envLookup abs = lookup abs $ unAbsEnv' fEnv

  heapLookup :: Loc -> Maybe Value
  heapLookup loc = lookup loc $ unAbsHeap' fHeap

  defReturnLookup :: String -> Maybe Abs
  defReturnLookup name =
    firstJust (\def -> boolToMaybe (def.gName == name) $ fromMaybe __IMPOSSIBLE__ def.gReturn) defs

  -- TODO maybe add Ord instance for Value: Bas < Loc ... FETCH < Pick (or a sorting function)

  go :: Value -> Value
  go (Loc loc) = Loc loc
  go Bas = Bas
  go (VNode tag vs) = VNode tag $ map go vs

  -- Replace with pointee
  go (Abs abs) = fromMaybe (error $ "CAN'T FIND " ++ prettyShow abs) $ envLookup abs

  go (Union v1 v2)
    -- Filter duplicates
    | v1 == v2 = v1

    -- Filter self references
    | (_:_:_, _) <- partition isSelfReference [v1, v2] = __IMPOSSIBLE__
    | (_:_, v3:v3s) <- partition isSelfReference [v1, v2] = listToValue (v3 :| v3s)

    -- Gather node values of same tag
    -- {... tag [v₁,...,vᵢ,...],...} ∪ {... tag [w₁,...,wᵢ,...],...} =
    -- {... tag [v₁ ∪ w₁,...,vᵢ ∪ wᵢ,...],...}
    | (n1:n1s, v1s) <- mapMaybeAndRest vnodeView $ valueToList v1
    , (n2:n2s, v2s) <- mapMaybeAndRest vnodeView $ valueToList v2
    , _:_ <- intersectBy (on (==) fst) (n1 : n1s) (n2 : n2s) =
      let nodes = gatherEntriesBy1 (zipWith mkUnion) (n1 :| n1s) (n2 :| n2s)
          nodes' = List1.map (uncurry VNode) nodes in
      listToValue $ v1s `List1.prependList` nodes' `List1.appendList` v2s

    -- Recurse
    | otherwise = on mkUnion go v1 v2

    where
      isSelfReference v1
        | Abs abs <- v1
        , Just v2 <- envLookup abs = v2 == Union v1 v2
        | otherwise = False

  go (Pick v1 tag1 i)
    | VNode tag2 vs <- v1
    , tag1 == tag2 = fromMaybe __IMPOSSIBLE__ $ vs !!! i
    | VNode{} <- v1 = __IMPOSSIBLE__

    -- Recurse
    | EVAL{} <- v1 = Pick (go v1) tag1 i
    | FETCH{} <- v1 = Pick (go v1) tag1 i
    | Pick{} <- v1 = Pick (go v1) tag1 i
    | Abs{} <- v1 = Pick (go v1) tag1 i


    -- Filter everything which is confirmed wrong
    | Union{} <- v1
    , (_:_, v2 : v2s) <- partition isWrong $ valueToList v1 =
      Pick (listToValue $ v2 :| v2s) tag1 i

    -- Solve correct tags
    | Union{} <- v1
    , (v2 : v2s, v3s) <- mapMaybeAndRest isCorrect $ valueToList v1 =
      caseList v3s
        (listToValue $ v2 :| v2s)
        (\v3 v3s ->
          listToValue (v2 :| v2s) `mkUnion` Pick (listToValue $ v3 :| v3s) tag1 i)

    -- Recurse (do not distribute!)
    -- {..., tag[v₁,...,vᵢ,...],...} ↓ tag ↓ i =  vᵢ
    | Union v2 v3 <- v1 = Pick (on mkUnion go v2 v3) tag1 i

    | Bas <- v1 = __IMPOSSIBLE__
    | Loc{} <- v1 = error $ "BAD: " ++ prettyShow v1
    | Variable{} <- v1 = __IMPOSSIBLE__ -- FIXME
    | Location{} <- v1 = __IMPOSSIBLE__ -- FIXME

    where
      isWrong (VNode tag2 _) = tag1 /= tag2
      isWrong Bas            = True
      isWrong Loc{}          = True
      isWrong EVAL{}         = False
      isWrong FETCH{}        = False
      isWrong Pick{}         = False
      isWrong Abs{}          = False
      isWrong Union{}        = False
      isWrong Variable{}     = __IMPOSSIBLE__ -- FIXME
      isWrong Location{}     = __IMPOSSIBLE__ -- FIXME

      isCorrect (VNode tag2 vs) | tag1 == tag2 =
        Just $ fromMaybe __IMPOSSIBLE__ $ vs !!! i
      isCorrect _ = Nothing

  go (FETCH v1)
    | Loc loc <- v1 = fromMaybe __IMPOSSIBLE__ $ heapLookup loc

    -- Solve locations
    | Union{} <- v1
    , (loc : locs, v2s) <- mapMaybeAndRest isLocation $ valueToList v1 =
      let v3s = List1.map (fromMaybe __IMPOSSIBLE__ . heapLookup) $ loc :| locs in
      caseList v2s
        (listToValue v3s)
        (\v2 v2s -> listToValue v3s `mkUnion` FETCH (listToValue $ v2 :| v2s))

    -- Distribute FETCH
    | Union v2 v3 <- v1 = on mkUnion FETCH v2 v3

    -- Recurse
    | EVAL{} <- v1 = FETCH $ go v1
    | FETCH{} <- v1 = FETCH $ go v1
    | Pick{} <- v1 = FETCH $ go v1
    | Abs{} <- v1 = FETCH $ go v1


    | Bas <- v1 = __IMPOSSIBLE__
    | VNode{} <- v1 = __IMPOSSIBLE__
    | Variable{} <- v1 = __IMPOSSIBLE__ -- FIXME
    | Location{} <- v1 = __IMPOSSIBLE__ -- FIXME

    where
      isLocation :: Value -> Maybe Loc
      isLocation (Loc loc) = Just loc
      isLocation _         = Nothing

  go (EVAL v1)
    | VNode CTag{} _ <- v1 = v1

    -- Solve with return variable for known functions, and Cnat for primitives
    | VNode FTag{tDef} _ <- v1 =
      maybe cnat Abs $ defReturnLookup tDef
    | VNode PTag{tDef} _ <- v1 =
      maybe cnat Abs $ defReturnLookup tDef

    -- Solve C nodes
    | Union{} <- v1
    , (v2:v2s, v3s) <- mapMaybeAndRest isCNode $ valueToList v1 =
      caseList v3s
        (listToValue $ v2 :| v2s)
        (\v3 v3s -> listToValue (v2 :| v2s) `mkUnion` EVAL (listToValue $ v3 :| v3s))

    -- Solve F and P nodes
    | Union{} <- v1
    , (v2 : v2s, v3s) <- mapMaybeAndRest hasDefName $ valueToList v1 =
      let v2s' = List1.map (maybe cnat Abs . defReturnLookup) (v2 :| v2s) in
      caseList v3s
        (listToValue v2s')
        (\v3 v3s -> listToValue v2s' `mkUnion` EVAL (listToValue $ v3 :| v3s))

    -- Distribute
    | Union v2 v3 <- v1 = on mkUnion EVAL v2 v3

    -- Recurse
    | EVAL{} <- v1 = EVAL $ go v1
    | FETCH{} <- v1 = EVAL $ go v1
    | Pick{} <- v1 = EVAL $ go v1
    | Abs{} <- v1 = EVAL $ go v1


    | Bas <- v1 = __IMPOSSIBLE__
    | Loc{} <- v1 = __IMPOSSIBLE__
    | Variable{} <- v1 = __IMPOSSIBLE__ -- FIXME
    | Location{} <- v1 = __IMPOSSIBLE__ -- FIXME

    where
      isCNode (VNode tag@CTag{} vs) = Just $ VNode tag vs
      isCNode _                     = Nothing

      hasDefName (VNode PTag{tDef} _) = Just tDef
      hasDefName (VNode FTag{tDef} _) = Just tDef
      hasDefName _                    = Nothing

  go (Variable _) = __IMPOSSIBLE__
  go (Location _) = __IMPOSSIBLE__


-- TODO remove monad
sub :: [GrinDefinition] -> Value -> F Value

sub _ (Loc loc) = pure $ Loc loc
sub _ (Abs abs) = fromMaybeM __IMPOSSIBLE__ $ envLookup abs

sub defs (Union v1 v2)
  -- Remove duplicates
  | v1 == v2 = pure v1
  | (v1s, v2s) <- on (,) valueToList v1 v2
  , v1':v1s' <- intersectBy sameTag v1s v2s =
    pure $ listToValue $ for (v1' :| v1s') $ \case
      VNode tag vs
        | (_, vss) <- unzipWith (fromMaybe __IMPOSSIBLE__ . vnodeView) $ filter (hasTag tag) v2s ->
          VNode tag $ foldl (zipWith mkUnion) vs vss
      v -> v
 -- Remove possible self-references, otherwise recurse.
 | otherwise =
   let go v3 FixCxt{fHeap, fEnv}
         | Loc loc <- v3
         , Just v4 <- heapLookup' loc fHeap
         , v4 == Union v1 v2 = Nothing
         | Abs abs <- v3
         , Just v4 <- envLookup' abs fEnv
         , v4 == Union v1 v2 = Nothing
         | otherwise = Just v3
   in do
     cxt <- ask
     case mapMaybe (`go` cxt) [v1, v2] of
       [v]      -> pure v
       [v1, v2] -> on (liftM2 mkUnion) (sub defs) v1 v2
       _        -> __IMPOSSIBLE__

  where
   --
    go = undefined
 -- | otherwise = pure $ Union a b

sub _ Bas            = pure Bas -- fix point
sub defs (VNode tag vs) = VNode tag <$> mapM (sub defs) vs

sub defs (FETCH v )
  | Just l <- isLocation v = fromMaybeM __IMPOSSIBLE__ $ heapLookup l
  | Union{} <- v
  , (l:ls, vs) <- mapMaybeAndRest isLocation $ valueToList v
  = do
    ws <- mapM (fromMaybeM __IMPOSSIBLE__ . heapLookup) $ l :| ls
    case vs of
      []   -> pure $ listToValue ws
      v:vs -> pure $ on (\l1 -> mkUnion l1 . FETCH) listToValue ws (v :| vs)

    -- vs2 <-
    -- pure $ listToValue $ xs' ++ ys
  | Union a b <- v = pure $ on mkUnion FETCH a b
  | otherwise = FETCH <$> sub defs v
  -- | otherwise = pure $ FETCH v

sub defs (EVAL v)
  | VNode CTag{} _ <- v = pure v
  | VNode FTag{tDef} _ <- v =
    pure $ maybe cnat (Abs . fromMaybe __IMPOSSIBLE__ . gReturn) $ find ((==tDef) . gName) defs
  | VNode PTag{tDef} _ <- v =
    pure $ maybe cnat (Abs . fromMaybe __IMPOSSIBLE__ . gReturn) $ find ((==tDef) . gName) defs

  -- solve correct values
  | Union{} <- v
  , all isCNode $ valueToList v = pure v
  | Union{} <- v
  , (v:vs, w:ws) <- partition isCNode $ valueToList v =
    pure $ on (\l1 -> mkUnion l1 . EVAL) listToValue (v :| vs) (w :| ws)

  -- Elaborate F and P-tags
  | Union{} <- v
  , (defNames, vs) <- mapMaybeAndRest defName $ valueToList v
  , w:ws <- map (maybe Bas (Abs . fromMaybe __IMPOSSIBLE__ . gReturn) . (\defName -> find  ((==defName). gName) defs)) defNames =
    case vs of
      []   -> pure $ listToValue $ w :| ws
      v:vs -> pure $ on (\l1 -> mkUnion l1 . EVAL) listToValue (w :| ws) (v :| vs)




  -- -- filter wrong tags
  -- | Union{} <- v
  -- , (xs, ys) <- partition (\v -> not $ isPNode v || isFNode v) $ valueToList v
  -- , on (&&) (not . null) xs ys = do
  --   pure $ EVAL $ listToValue xs

  | otherwise = EVAL <$> sub defs v

  where
    defName (VNode FTag{tDef} _) = Just tDef
    defName (VNode PTag{tDef} _) = Just tDef
    defName _                    = Nothing

    isCNode (VNode CTag{} _) = True
    isCNode _                = False

    isPNode (VNode FTag{} _) = True
    isPNode _                = False

    isFNode (VNode FTag{} _) = True
    isFNode _                = False

sub defs (Pick v tag1 i)
  | VNode tag2 vs <- v
  , tag1 == tag2 = pure $ vs !! i

  -- filter wrong tags
  | Union{} <- v
  , (_:_, v:vs) <- partition (hasWrongTag tag1) $ valueToList v =
    pure $ Pick (listToValue $ v :| vs) tag1 i

  -- solve correct values
  | Union{} <- v
  , vs <- valueToList v
  , (w:ws, xs) <- flip mapMaybeAndRest vs $ \case { VNode tag2 vs | tag2 == tag1 -> vs !!! i ; _  -> Nothing  } =
    case xs of
      []   -> pure $ listToValue (w :| ws)
      x:xs -> pure $ listToValue (w :| ws) `mkUnion` Pick (listToValue $ x :| xs) tag1 i

  | otherwise = (\v' -> Pick v' tag1 i) <$> sub defs v

sub _ (Variable x)   = __IMPOSSIBLE__
sub _ (Location l)   = __IMPOSSIBLE__

envLookup :: MonadReader FixCxt m => Abs -> m (Maybe Value)
envLookup x = asks $ envLookup' x . fEnv

heapLookup :: MonadReader FixCxt m => Loc -> m (Maybe Value)
heapLookup l = asks $ heapLookup' l . fHeap

envLookup' :: Abs -> AbsEnv' -> Maybe Value
envLookup' abs = lookup abs . unAbsEnv'

heapLookup' :: Loc -> AbsHeap' -> Maybe Value
heapLookup' loc = lookup loc . unAbsHeap'

absHeapUpdate :: Loc -> Value -> FixCxt -> FixCxt
absHeapUpdate loc v cxt =
    cxt{fHeap = AbsHeap' heap}
  where
    heap = for (unAbsHeap' cxt.fHeap) $
      \(loc', v') -> if loc == loc' then (loc', v) else (loc', v')

absEnvUpdate :: Abs -> Value -> FixCxt -> FixCxt
absEnvUpdate abs v cxt =
    cxt{fEnv = AbsEnv' env}
  where
    env = for (unAbsEnv' cxt.fEnv) $
      \(abs', v') -> if abs == abs' then (abs', v) else (abs', v')


heapUpdate :: FixCxt -> (Loc, Value) -> FixCxt
heapUpdate cxt (l, v) =
    cxt{fHeap = AbsHeap' hs}
  where
    hs = [ if l == l' then (l, v) else (l', v')
         | (l', v') <- unAbsHeap' cxt.fHeap
         ]

envUpdate :: FixCxt -> (Abs, Value) -> FixCxt
envUpdate cxt (x, v) =
    cxt{fEnv = AbsEnv' es}
  where
    es = [ if x == x' then (x, v) else (x', v')
         | (x', v') <- unAbsEnv' cxt.fEnv
         ]

envInsert' :: FixCxt -> (Abs, Value) -> FixCxt
envInsert' cxt e = cxt{fEnv = AbsEnv' $ insert e $ unAbsEnv' cxt.fEnv}

heapInsert' :: (Loc, Value) -> FixCxt -> FixCxt
heapInsert' entry cxt = cxt{fHeap = AbsHeap' $ insert entry $ unAbsHeap' cxt.fHeap}



hasWrongTag :: Tag -> Value -> Bool
hasWrongTag tag1 (VNode tag2 _) = tag1 /= tag2
hasWrongTag _ _                 = False

hasTag :: Tag -> Value -> Bool
hasTag tag1 (VNode tag2 _) = tag1 == tag2
hasTag _ _                 = False

sameTag :: Value -> Value -> Bool
sameTag (VNode tag1 _) (VNode tag2 _) = tag1 == tag2
sameTag _ _                           = False

valueToList :: Value -> [Value]
valueToList (Union a b) = on (++) valueToList a b
valueToList a           = [a]

-- partial!
listToValue :: List1 Value -> Value
listToValue = foldr1 mkUnion

isLocation :: Value -> Maybe Loc
isLocation (Loc l) = Just l
isLocation _       = Nothing

-- TODO sharing analysis

sharingAnalysis = undefined


-----------------------------------------------------------------------
-- * GRIN transformations
-----------------------------------------------------------------------


-- TODO


















-----------------------------------------------------------------------
-- * LLVM code generation
-----------------------------------------------------------------------

-- • Turns low-level GRIN into LLVM IR
-- • Node values are represented with the type %Node = type [ 4 x i64 ].
--   The first index contains the unique tag, and the rest of the values are
--   usually pointers to other Nodes. The only exception
--   is Cnat which contains an unboxed i64 in the second index.
-- • Uses utility functions (mkNode, selᵢ, ...) to generate LLVM IR


-- TODO

-- data Cxt = Cxt
--   { gDefs      :: [GrinDefinition]
--   , builtins   :: Builtins
--   , vars       :: [VarInfo]
--     -- ^ Only variables from the GRIN program.
--     --   The integer is the corresponding Llvm variable (e.g. %4).
--   , currentDef :: GrinDefinition
--   }
--
-- initCxt :: [GrinDefinition] -> Builtins -> Cxt
-- initCxt defs builtins = Cxt
--   { gDefs = defs
--   , builtins = builtins
--   , vars = []
--   , currentDef = head defs
--   }
--
-- data VarInfo = VarInfo
--   { name :: String
--   }
--
-- data Env = Env
--   { varCount :: Int
--   }
--
-- initEnv :: Env
-- initEnv = Env {varCount = 0}
--
--
--
-- type M a = StateT Env (Reader Cxt) a
--
-- grinToLlvm :: [GrinDefinition] -> M [L.Instruction]
-- grinToLlvm defs =
--   forM defs $ \def -> do
--     put initEnv
--     let vars = take def.gArity $ map (VarInfo . singleton) ['a' ..]
--     local (\cxt -> cxt {vars=vars , currentDef=def}) $ defToLlvm def
--
-- defToLlvm :: GrinDefinition -> M L.Instruction
-- defToLlvm GrinDefinition{gName, gType=Just typ, gTerm, gArity} = do
--   (ts, t) <- bimapM (mapM typeToLlvm) typeToLlvm $ returnType typ
--   body <- termToLlvm gTerm
--
--   pure $
--     L.Define
--       L.Fastcc
--       L.Ptr
--       (L.MkVar $ prettyShow gName)
--       (zip (replicate gArity L.Ptr) $ map singleton ['a' ..])
--       body
--
-- termToLlvm :: Term -> M [L.Instruction]
-- termToLlvm (Bind t alt) = liftA2 (++) (termToLlvm t) (altToLlvm alt)
--
-- termToLlvm (Fetch (Var n)) = do
--   x <- getVar n
--   getelementptr <- setVar $
--     L.Getelementptr nodeType
--       [ (L.Ptr, L.Var x)
--       , (L.I32, llvmLit 0)
--       , (L.I32, llvmLit 0)
--       ]
--   x <- L.Var <$> getLlvmVar
--   load <- setVar $
--     L.Load L.I8 L.Ptr x
--
--   pure
--     [ getelementptr
--     , load
--     ]
--
--
-- termToLlvm (Case n def alts) = do
--     x <- getVar n
--     clauses <- altsToLlvm
--     defClause <- L.Label "clause_def" <$> termToLlvm def
--
--     pure $
--       [ L.Switch caseType x (L.MkVar "clause_def") alts'
--       ]
--       ++
--       snoc clauses defClause
--   where
--     alts'
--       | Just t <- isLitCase =
--         zipWith
--           (\(AltLit l _) -> L.Alt t $ litToLlvm l)
--           alts
--           clauseLabels
--       | isNodeCase =
--         zipWith
--           (\(AltNode t _) -> L.Alt L.I8 $ llvmLit $ tag t)
--           alts
--           clauseLabels
--       | otherwise = __IMPOSSIBLE__
--
--     caseType
--       | Just t <- isLitCase = t
--       | isNodeCase = L.I8
--       | otherwise  = __IMPOSSIBLE__
--
--     isLitCase :: Maybe L.Type
--     isLitCase =
--       mapM (fmap litToLlvmType . isLit) alts >>= \case
--         []   -> __IMPOSSIBLE__
--         x:xs -> foldr sameType (Just x) xs
--       where
--         sameType t1 (Just t2)
--           | t1 == t2  = Just t2
--           | otherwise = Nothing
--         sameType _ Nothing  = Nothing
--
--         isLit (AltLit lit _) = Just lit
--         isLit _              = Nothing
--
--     isNodeCase = flip all alts $ \case
--       AltNode{} -> True
--       _         -> False
--
--     clauseLabels = take (length alts) mkClauseLabels
--
--     altsToLlvm =
--         zipWithM (\s i -> L.Label s <$> altToLlvm i) clauseLabels alts
--
-- termToLlvm (Unit v) = do
--   v' <- valToLlvm v
--   pure [L.Ret L.Ptr v']
--
--
--
-- termToLlvm (App (Prim PSub64) vs) = do
--   pure [L.Comment "sub64"]
-- termToLlvm (App (Prim PAdd64) vs) = do
--   pure [L.Comment "add64."]
--
--
-- termToLlvm (App v vs) = do
--   v' <- valToLlvm v
--   -- FIXME
--   vs' <- zip (repeat L.Ptr) <$> mapM valToLlvm vs
--   singleton <$> setVar (L.Call L.Fastcc L.Ptr L.Global v' vs')
--
--
-- termToLlvm t = pure $ singleton $ L.Comment $ prettyShow t
--
-- -- termToLlvm t = error $ "not implemented: " ++ show t
--
--
-- valToLlvm :: Val -> M L.Val
-- valToLlvm (Lit lit)  = pure $ litToLlvm lit
-- valToLlvm (Var n)    = L.Var <$> getVar n
-- valToLlvm (Def q)    = pure $ L.Var $ L.MkVar $ prettyShow q
-- valToLlvm (Node _ _) = pure L.Null -- FIXME
-- valToLlvm v          = error $ "valToLlvm not implemented: " ++ show v
--
-- litToLlvm :: Literal -> L.Val
-- litToLlvm = L.Lit
--
-- nodeType :: L.Type
-- nodeType = L.Alias $ L.MkVar "Node"
--
-- setVar :: MonadState Env m => L.Instruction -> m L.Instruction
-- setVar i = do
--   modify $ \env -> env {varCount = 1 + env.varCount}
--   x <- gets $ L.MkVar . show . varCount
--   pure $ L.SetVar x i
--
-- mkClauseLabels :: [String]
-- mkClauseLabels = map (("clause_" ++) . show) [1 ..]
--
-- listType :: L.Type
-- listType = L.Structure [L.I8, L.Ptr, L.Ptr]
--
-- nodeType' :: Int -> L.Type
-- nodeType' n = L.Structure $ L.I8 : replicate n L.Ptr
--
-- -- TODO use local and increase vars
-- -- PROBLEM multiple variables for a term means that de bruijn indices are off
-- altToLlvm :: Alt -> M [L.Instruction]
-- altToLlvm = \case
--   -- TODO claues
--   AltNode tag t -> do
--     scrutVar <- getVar 0
--     let arity = tagArity tag
--
--     -- TODO do same for full arity
--     (newVars, is) <- fmap (second concat . unzip) $ forM [1 .. arity] $ \n -> do
--       getelementptr <- setVar $
--         L.Getelementptr (nodeType' arity)
--           [ (L.Ptr, L.Var scrutVar)
--           , (L.I32, llvmLit 0)
--           , (L.I32, llvmLit n)]
--       x <- L.Var <$> getLlvmVar
--       load <- setVar $ L.Load nodeType L.Ptr x
--       name <- gets $ show . varCount
--       pure (VarInfo {name=name}, [getelementptr, load])
--
--     rhs <- local (\cxt -> cxt { vars = newVars ++ cxt.vars }) $ termToLlvm t
--     pure $ is ++ rhs
--
--   AltLit _ t -> termToLlvm t
--
--
--   AltVar t -> do
--     -- Add the most recent LLVM variable to context. FIXME
--     n <- gets varCount
--     let newVar = VarInfo {name = show n}
--     local (\cxt -> cxt { vars = newVar : cxt.vars }) (termToLlvm t)
--
--   AltEmpty t -> termToLlvm t
--
--
-- typeToLlvm :: Type -> M L.Type
-- typeToLlvm t = asks $ flip go t where
--
--   go cxt (I.El _ t)
--     | Just prim <- mprim = prim
--     where
--       mprim = Map.lookup t cxt.builtins >>= \case
--         BuiltinNat -> Just L.I64
--         b          -> error $ "Not implemented: " ++ prettyShow b
--
--   go _ (I.El _ t) = case t of
--     I.Def q _-> L.Alias $ L.MkVar $ prettyShow q
--     _         -> L.Ptr -- FIXME
--
--
-- litToLlvmType :: Literal -> L.Type
-- litToLlvmType LitNat{} = L.I64
--
--
-- getLlvmVar :: MonadState Env m => m L.Var
-- getLlvmVar = gets $ L.MkVar . show . varCount
--
-- getVar :: MonadReader Cxt m => Int -> m L.Var
-- getVar n = do
--   vs <- asks vars
--   pure $ maybe __IMPOSSIBLE__ (L.MkVar . name) (vs !!! n)
--
-- llvmLit :: Integral a => a -> L.Val
-- llvmLit = L.Lit . LitNat . toInteger
--
-- returnType :: Type -> ([Type], Type)
-- returnType = go . ([], ) where
--   go (acc, I.El _ (I.Pi I.Dom{unDom} I.NoAbs{unAbs})) = go (snoc acc unDom , unAbs)
--   go p = p
--
--














































-----------------------------------------------------------------------
-- * Other
-----------------------------------------------------------------------

-- manual :: [GrinDefinition]
-- manual = [evalDef, downFromDef, sumDef, mainDef] where
--   callEval = App (Def "eval") . singleton
--   nilN = "[]"
--   consN = "_∷_"
--   sumN = "sum"
--   downFromN = "downFrom"
--   printfN = "printf"
--   natN = "nat"
--
--   evalDef = GrinDefinition {gType=Nothing, gTreeless=Nothing, gTerm=eval, gName="eval", gArity=1}
--   eval =
--     Bind
--       (Fetch $ Var 0)
--       (AltVar
--         (Case 0 unreachable
--           [ AltNode CTag {tTag=0, tCon=natN, tArity=1} (Unit $ Var 1)
--           , AltNode CTag {tTag=1, tCon=nilN, tArity=0} (Unit $ Var 0)
--           , AltNode CTag {tTag=2, tCon=consN, tArity=2} (Unit $ Var 2)
--           , AltNode FTag {tTag=3, tDef=downFromN, tArity=1}
--               (Bind
--                 (App (Def downFromN) [Var 0])
--                 (AltVar
--                   (Bind
--                     (Update (Var 2) (Var 0))
--                     (AltEmpty
--                       (Unit $ Var 0)
--                     )
--                   )
--                 )
--               )
--           , AltNode FTag {tTag=4, tDef=sumN, tArity=1}
--               (Bind
--                 (App (Def sumN) [Var 0])
--                 (AltVar
--                   (Bind
--                     (Update (Var 2) (Var 0))
--                     (AltEmpty
--                       (Unit $ Var 0)
--                     )
--                   )
--                 )
--               )
--           ]
--         )
--       )
--
--   downFromDef = GrinDefinition {gType=Nothing, gTreeless=Nothing, gTerm=downFrom, gName=downFromN, gArity=1}
--   downFrom =
--     Bind
--       (callEval $ Var 0)
--       (AltNode CTag{tTag=0, tCon=natN, tArity=1}
--         (Case 0
--           (Bind
--             (App (Prim PSub64) [Var 0, Lit $ LitNat 1])
--             (AltVar
--               (Bind
--                 (Store $ Node CTag{tTag=0, tCon=natN, tArity=1} [Var 0])
--                 (AltVar
--                   (Bind
--                     (Store $ Node FTag {tTag=3, tDef=downFromN, tArity=1} [Var 0])
--                     (AltVar
--                       (Bind
--                         (Store $ Node CTag{tTag=0, tCon=natN, tArity=1} [Var 2])
--                         (AltVar
--                           (Unit $ Node CTag {tTag=2, tCon=consN, tArity=2} [Var 0, Var 1])
--                         )
--                       )
--                     )
--                   )
--                 )
--               )
--             )
--           )
--           [ AltLit (LitNat 0) $ Unit $ Node CTag {tTag=1, tCon=nilN, tArity=0} []]
--         )
--       )
--
--
--   sumDef = GrinDefinition {gType=Nothing, gTreeless=Nothing, gTerm=sum, gName=sumN, gArity=1}
--   sum =
--     Bind
--       (callEval $ Var 0)
--       (AltVar
--         (Case 0 unreachable
--           [ AltNode
--               CTag {tTag=1, tCon=nilN, tArity=0}
--               (Unit $ Node CTag { tTag=0, tCon=natN, tArity=1} [Lit $ LitNat 0])
--           , AltNode
--               CTag {tTag=2, tCon=consN, tArity=2}
--               (Bind
--                 (callEval $ Var 1)
--                 (AltNode CTag{tTag=0, tCon=natN, tArity=1}
--                   (Bind
--                     (App (Def sumN) [Var 1])
--                     (AltNode CTag{tTag=0, tCon=natN, tArity=1}
--                       (Bind
--                         (App (Prim PAdd64) [Var 1, Var 0])
--                         (AltVar
--                           (Unit $ Node CTag {tTag=0, tCon=natN, tArity=1} [Var 0])
--                         )
--                       )
--                     )
--                   )
--                 )
--               )
--
--
--           ]
--         )
--       )
--
--
--   mainDef = GrinDefinition {gType=Nothing, gTreeless=Nothing, gTerm=main, gName="main", gArity=0}
--   main =
--     Bind
--       (Store $ Node CTag {tTag=0, tCon=natN, tArity=1} [Lit $ LitNat 4])
--       (AltVar
--         (Bind
--           (Store $ Node FTag {tTag=3, tDef=downFromN, tArity=1} [Var 0])
--           (AltVar
--             (Bind
--               (Store $ Node FTag {tTag=4, tDef=sumN, tArity=1} [Var 0])
--               (AltVar
--                 (Bind
--                   (callEval $ Var 0)
--                   (AltNode CTag {tTag=0, tCon=natN, tArity=1}
--                     (App (Def printfN) [Var 0])
--                   )
--                 )
--               )
--             )
--           )
--         )
--       )
--

