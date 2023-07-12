{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Agda.Llvm.Compiler (llvmBackend) where

import           Control.DeepSeq                       (NFData)
import           Control.Monad                         (forM_, liftM2,
                                                        replicateM, replicateM_,
                                                        unless, zipWithM,
                                                        zipWithM_)
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
                                                        (\\))
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Maybe                            (catMaybes, fromJust,
                                                        fromMaybe, isJust,
                                                        mapMaybe)
import           Data.Tuple.Extra                      (secondM)
import           Debug.Trace                           (trace)
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
import           Agda.Utils.Functor
import           Agda.Utils.Impossible
import           Agda.Utils.List
import           Agda.Utils.List1                      (List1)
import           Agda.Utils.Maybe                      (allJustM, caseMaybeM,
                                                        fromMaybeM, maybeM,
                                                        whenJust, whenJustM)
import           Agda.Utils.Monad                      (ifM)
import           Agda.Utils.Pretty
import           Agda.Utils.Tuple                      (swap)
import           Control.Applicative                   (Applicative (liftA2))
import           Data.Bifunctor                        (Bifunctor (bimap, first, second))
import           Data.Bitraversable                    (bimapM)
import           Data.Foldable                         (foldrM)
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

data LlvmOptions = LlvmOptions
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
llvmCompileDef env menv isMain def@Defn{defName, defType, theDef=Function{funCompiled}} = do

    -- let term = case prettyShow defName of
    --       "DownFrom.downFrom" -> env.terms !! 1
    --       "DownFrom.sum"      -> env.terms !! 2
    --       "DownFrom.main"     -> env.terms !! 3
    --       s                   -> error $ "ERROR UNREACHABLE " ++ show defType

    -- when (isMain == IsMain && isNamedMain) $ do
    --   pure ()
      -- liftIO $ putStrLn $ "IS MAIN: " ++ prettyShow defName

    treeless <- maybeM __IMPOSSIBLE__ normalizeNames $ toTreeless LazyEvaluation defName
    let (arity, treeless') = skipLambdas treeless

    let term' = treelessToGrin (isMain == IsMain && isNamedMain) (simplifyApp treeless') arity

    let gDef  = GrinDefinition
          {gType=Just defType
          ,gName=prettyShow defName
          ,gArity=arity
          ,gTerm=term'
          ,gTreeless=Just $ simplifyApp treeless
          }

    pure $ Just gDef
  where
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
    liftIO $ putStrLn "Variable table:"
    liftIO $ putStrLn (prettyShow $ genVariableTable defs)
    let (heap, env, share) = heapPointsTo defs
    liftIO $ putStrLn "\nAbstract heap: "
    liftIO $ putStrLn $ prettyShow heap
    liftIO $ putStrLn "\nAbstract env: "
    liftIO $ putStrLn $ prettyShow env

    let (heap', env') = solveEquations heap env
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
    mkEl unEl = I.El {unEl=unEl, _getSort=undefined}
    mkDom unDom = I.Dom {domTactic=undefined, domName=undefined, domIsFinite=undefined, domInfo=undefined, unDom=unDom}
    mkNoAbs unAbs = I.NoAbs {absName=undefined, unAbs=unAbs}

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
-- • Refactor
-- • Reuse evaluated variables
-- • Fill in rest of the patterns
-- • Assign an unique tag (@tTag@) instead of 0?

-- Preconditions:
-- • Separate applications
-- • Lambda lifted
-- • No polymorphic functions?
-- • Saturated constructors
treelessToGrin :: Bool -> TTerm -> Int -> Term
treelessToGrin isMain t arity = evalState (rScheme t) $ initGEnv arity isMain


rScheme :: TTerm -> G Term
rScheme (TCase n CaseInfo{caseType=CTNat} def alts) = do
  alts' <- mapM (aScheme . raise 1) alts
  def' <- rScheme (raise 1 def)
  pure $ Bind (eval n) $ AltNode natTag $ Case 0 def' alts'

rScheme (TCase n CaseInfo{caseType=CTData _} def alts) = do
  alts' <- mapM (aScheme . raise 1) alts
  def' <- rScheme (raise 1 def)
  pure $ Bind (eval n) $ AltVar $ Case 0 def' alts'

-- | 𝓡 [_[]_] = unit (C_[]_)
rScheme (TCon q) = pure $ Unit $ Node tag [] where
  tag = CTag{tTag = 0, tCon = prettyShow q, tArity = 0}
rScheme (TLit lit) = pure $ Unit $ Node natTag [Lit lit]
rScheme (TError TUnreachable) = pure $ Error TUnreachable



rScheme (TApp t as) = do
    isMain <- gets isMain
    let res t
          | isMain = Bind t $ AltVar $ Bind (eval 0) $ AltNode natTag $ printf 0
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
    --               unit (Cnat @1)
    appPrim res prim as = pure $ mkWithOffset (length evals) evals'
      where

        fin = Bind (App (Prim prim) vs) $ AltVar
            $ res $ Unit $ Node natTag [Var 0]

        evals' = foldr (\t ts -> Bind t $ AltVar ts) fin evals
        (evals, vs) =  foldr f ([], []) as

        f (TLit lit) (es, vs) = (es, Lit lit : vs)
        f (TVar n)   (es, vs) = (eval (on (-) length evals es + n - 1)  : es, Var (length es) : vs)
        f _          _        = __IMPOSSIBLE__



    -- | 𝓡 [foo x y] = foo x y
    appDef res q as = pure $ mkWithOffset (length stores) stores'
      where
        fin = res $ App (Def $ prettyShow q) vs

        stores' :: Term
        stores' = foldr (\t ts -> Bind t $ AltVar ts) fin stores
        (stores, vs) = foldr f ([], []) as

        f (TLit lit) (ss, vs) = (mkStore lit : ss, Var (length ss) : vs)
        f (TVar n)   (ss, vs) = (ss, Var (n + length stores) : vs)
        f _          _        = __IMPOSSIBLE__

        mkStore lit = Store $ Node natTag [Lit lit]

    -- | 𝓡 [_∷_ x xs] = unit (C_∷_ @1 @0)
    appCon res q as = pure $ mkWithOffset (length stores) stores'
      where
        tag = CTag{tTag = 0, tCon = prettyShow q, tArity = length as}

        fin = res $ Unit $ Node tag vs

        stores' :: Term
        stores' = foldr (\t ts -> Bind t $ AltVar ts) fin stores
        (stores, vs) = foldr f ([], []) as




        f (TLit lit) (ss, vs) = (mkStore lit : ss, Var (length ss) : vs)
        f (TVar n)   (ss, vs) = (ss, Var (n + length stores) : vs)
        f _          _        = __IMPOSSIBLE__

        mkStore lit = Store $ Node natTag [Lit lit]


-- | 𝓡 [let t1 in t2] = 𝓒 [t1] ; λ #1 → 𝓡 [t2]
rScheme (TLet t1 t2)
  | TApp t as <- t1 = do
    WithOffset{value=t1', offset} <- cSchemeApp t as
    t2' <- rScheme $ raiseFrom 1 offset t2
    pure $ t1' t2'

  | TLet t1 t2 <- t1 = do
    t1' <- cScheme t1
    t2' <- rScheme t2
    pure $ Bind t1'$ AltVar t2'

rScheme t = error $ "TODO rScheme " ++ show t

aScheme :: TAlt -> G Alt
aScheme TALit{aLit, aBody} = do
  aBody' <- rScheme aBody
  pure $ AltLit aLit aBody'

aScheme TACon{aCon, aArity, aBody} = do
    aBody' <- rScheme aBody
    pure $ AltNode tag aBody'
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
cSchemeApp t as = pure $ mkWithOffset (length stores) stores'
  where
  tag
    | TDef q <- t = FTag{tTag = 0, tDef = prettyShow q, tArity = length as}
    | TPrim prim <- t =  FTag {tTag=0, tDef=primStr prim, tArity=length as}
    | otherwise = __IMPOSSIBLE__

  fin = Bind (Store $ Node tag vs) . AltVar

  stores' = foldr (\t ts -> Bind t . AltVar . ts) fin stores
  (stores, vs) =  foldr f ([], []) as

  f (TLit lit) (ss, vs) = (mkStore lit : ss, Var (length ss) : vs)
  f (TVar n)   (ss, vs) = (ss, Var (n + length stores) : vs)
  f _          _        = __IMPOSSIBLE__

  mkStore lit = Store $ Node natTag [Lit lit]

natTag :: Tag
natTag = CTag{tTag=0, tCon="nat", tArity=1}

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

type G = State GEnv

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

heapPointsTo :: [GrinDefinition] -> (AbsHeap, AbsEnv, Map Location Bool)
heapPointsTo defs = (equationsHeap, equationsEnv, mempty)
  where
    equationsEnv = AbsEnv $ sortOn fst $ unAbsEnv equationsState.env
    equationsHeap = AbsHeap $ sortOn fst $ unAbsHeap equationsState.heap

    equationsState = flip execState initCxtState $ forM defs $ \def ->
      let cxtReader = CxtReader {gDef=def, variableTable=variableTable} in do
      unless (isSuffixOf "main" def.gName) $
        let var = fromMaybe __IMPOSSIBLE__ (variableTableLookup' variableTable def.gName) !! def.gArity in
        modify $ \cxtState -> cxtState{lastVariable = var}
      runReaderT (deriveEquations def.gTerm) cxtReader

    variableTable = genVariableTable defs

newtype AbsHeap = AbsHeap{unAbsHeap :: [(Location, Value)]} deriving Eq
newtype AbsEnv = AbsEnv{unAbsEnv :: [(Variable, Value)] } deriving Eq

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
  countVarsAlt (AltVar t)      = 1 + countVars t
  countVarsAlt (AltNode tag t) = tagArity tag + countVars t
  countVarsAlt (AltEmpty t)    = countVars t
  countVarsAlt (AltLit _ t)    = countVars t

isSuffixOf :: String -> String -> Bool
isSuffixOf s1 s2@(s:ss)
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

-- FIXME super ugly stateful code
-- | Derive equations
deriveEquations :: Term -> H ()
deriveEquations t = case t of

  Bind t1@Store{} (AltVar t2) -> do
    deriveEquations t1
    sucLastVariable
    cxt <- get
    envInsert (cxt.lastVariable) (Location cxt.lastLocation)
    deriveEquations t2

  Bind t1 (AltNode tag t2) | tag == natTag -> do
    deriveEquations t1
    sucLastVariable
    cxt <- get
    envInsert (cxt.lastVariable) Bas
    deriveEquations t2

  Bind t1@(App (Prim _) _) (AltVar t2) -> do
    deriveEquations t1
    sucLastVariable
    cxt <- get
    envInsert (cxt.lastVariable) Bas
    deriveEquations t2

  Bind (App (Def "eval") [Var n]) (AltVar t) -> do
    x <- Variable <$> deBruijnLookup n
    sucLastVariable
    cxt <- get
    envInsert (cxt.lastVariable) (EVAL $ FETCH x)
    deriveEquations t

  Bind t@(App Def{} _) alt        -> do
      deriveEquations t
      deriveEquationsAlt alt
    where
      deriveEquationsAlt (AltNode tag t) = do
        replicateM_ (tagArity tag) sucLastVariable
        deriveEquations t
      deriveEquationsAlt (AltLit _ t) = deriveEquations t
      deriveEquationsAlt (AltVar t) = do
        sucLastVariable
        lastVariable <- gets lastVariable
        name <- asks $ gName . gDef
        v <- Variable . head . fromMaybe __IMPOSSIBLE__ <$> variableTableLookup name
        envInsert lastVariable v
        deriveEquations t

  Case n t alts     -> do
      sc <- Variable <$> deBruijnLookup n
      let
        deriveEquationsAlt (AltNode tag t) = do
          forM_ [0 .. tagArity tag - 1] $ \i -> do
            sucLastVariable
            lastVariable <- gets lastVariable
            envInsert lastVariable (Pick sc tag i)
          deriveEquations t
        deriveEquationsAlt (AltLit _ t) = deriveEquations t
        deriveEquationsAlt (AltVar t) = sucLastVariable >> deriveEquations t

      mapM_ deriveEquationsAlt alts
      deriveEquations t

  App (Def "eval") _ -> pure ()
  App (Def "printf") _ -> pure ()
  App (Prim _) _ -> pure ()
  App (Def n) vs -> do
    xs <- tail . fromMaybe __IMPOSSIBLE__ <$> variableTableLookup n
    zipWithM_ (\x v -> envInsert x =<< valToValue v) xs vs

  Store (Lit _)     -> heapInsert Bas
  Store (Node tag vs) -> do
    vs' <- mapM valToValue vs
    variableTable <- asks variableTable

    case tag of
      FTag{tDef=n}
        | Just xs <- tail <$> variableTableLookup' variableTable n -> do
          zipWithM_ envInsert xs vs'
          let x_return = maybe __IMPOSSIBLE__ head
                       $ variableTableLookup' variableTable n
          heapInsert $ mkUnion (VNode tag vs') (Variable x_return)
      _ -> heapInsert $ VNode tag vs'

  Error _ -> pure ()

  Unit v  -> do
      variableTable <- asks variableTable
      x <- asks $ maybe __IMPOSSIBLE__ head
                . variableTableLookup' variableTable
                . gName
                . gDef

      envInsert x =<< valToValue v
    where
      valToValue (Var n)       = Variable <$> deBruijnLookup n
      valToValue (Lit _)       = pure Bas
      valToValue (Node tag vs) = VNode tag <$> mapM valToValue vs

  t -> error $ "missing " ++ show t

  where
    valToValue (Var n) = Variable <$> deBruijnLookup n
    valToValue (Lit _) = pure Bas
    valToValue _       = __IMPOSSIBLE__

variableTableLookup :: MonadReader CxtReader m => String -> m (Maybe [Variable])
variableTableLookup n = asks $ flip variableTableLookup' n . variableTable

variableTableLookup' :: VariableTable -> String -> Maybe [Variable]
variableTableLookup' vt n = lookup n $ unVariableTable vt

envInsert :: MonadState CxtState m => Variable -> Value -> m ()
envInsert x v = modify $ \cxt ->
  let env = unAbsEnv cxt.env
      env'
        | Nothing <- lookup x env = snoc env (x, v)
        | otherwise = [ if x' == x then (x',  mkUnion v' v) else (x', v') | (x', v') <- env ] in
  cxt{env = AbsEnv env'}

heapInsert :: MonadState CxtState m => Value -> m ()
heapInsert vs = modify $ \cxt ->
  let heap = unAbsHeap cxt.heap
      location = succ cxt.lastLocation in
  cxt
    { heap = AbsHeap $ snoc heap (location, vs)
    , lastLocation = location
    }

sucLastVariable :: MonadState CxtState m => m ()
sucLastVariable = modify $ \cxt -> cxt{lastVariable = succ cxt.lastVariable}

deBruijnLookup :: MonadState CxtState m => Int -> m Variable
deBruijnLookup n = do
  lastVariable <- gets $ unVariable . lastVariable
  pure $ MkVariable $ lastVariable - n


-- TODO

type F = Reader FixCxt

data FixCxt = FixCxt
  { fHeap :: AbsHeap
  , fEnv  :: AbsEnv
  } deriving Eq

initFixCxt :: FixCxt
initFixCxt = FixCxt
  { fHeap = AbsHeap []
  , fEnv  = AbsEnv []
  }

instance Semigroup FixCxt where
  cxt1 <> cxt2 = FixCxt
    { fHeap = AbsHeap $ on unionNub (unAbsHeap . fHeap) cxt1 cxt2
    , fEnv = AbsEnv $ on unionNub (unAbsEnv . fEnv) cxt1 cxt2
    }

unionNub :: Eq a => [a] -> [a] -> [a]
unionNub xs = union xs . filter (`notElem` xs)

differenceBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
differenceBy eq =  foldl (flip $ deleteBy eq)

-- | Solve equations
solveEquations :: AbsHeap -> AbsEnv -> (AbsHeap, AbsEnv)
solveEquations (AbsHeap heapEqs) (AbsEnv envEqs) = fix initFixCxt
  where
    fix :: FixCxt -> (AbsHeap, AbsEnv)
    fix cxt@FixCxt{fHeap, fEnv = AbsEnv es} =
        go es
      where
        go es
          | e:_ <- differenceBy (on (==) fst) envEqs es =
            fix $ fixFixCxt $ collectNewFixCxt $ envInsert' cxt e
          | otherwise = (fHeap, AbsEnv es)

    collectNewFixCxt :: FixCxt -> FixCxt
    collectNewFixCxt cxt
      | Nothing <- mcxt  = cxt
      | Just cxt' <- mcxt = collectNewFixCxt $ cxt <> cxt'
      where
        mcxt =
          flip runReader cxt $ liftA2 (<>)
          (go $ unAbsEnv $ cxt.fEnv)
          (go $ unAbsHeap $ cxt.fHeap)
        go xs = foldl (<>) Nothing <$> mapM (collectNew . snd) xs

    collectNew :: Value -> F (Maybe FixCxt)
    collectNew (Union a b) = on (<>) collectNew a b
    collectNew (Variable x) =
      maybe (Just cxt) (const Nothing) <$> envLookup x
     where
      v = fromMaybe __IMPOSSIBLE__ $ lookup x envEqs
      cxt = initFixCxt{fEnv = AbsEnv [(x, v)]}
    collectNew (Location l) =
      maybe (Just cxt) (const Nothing) <$> heapLookup l
     where
      v = fromMaybe __IMPOSSIBLE__ $ lookup l heapEqs
      cxt = initFixCxt{fHeap = AbsHeap [(l, v)]}
    collectNew (FETCH v) = collectNew v
    collectNew (EVAL v) = collectNew v
    collectNew (VNode _ vs) = foldl (<>) Nothing <$> mapM collectNew vs
    collectNew (Pick v _ _) = collectNew v
    collectNew Bas = pure Nothing

fixFixCxt :: FixCxt -> FixCxt
fixFixCxt cxt
  | cxt == cxt' = cxt
  | otherwise = fixFixCxt cxt'
  where
    cxt' =
      let f cxt (l, v) = heapUpdate cxt (l, runReader (sub v) cxt) in
      let cxt' = foldl f cxt $ unAbsHeap cxt.fHeap in
      let g cxt (x, v) = envUpdate cxt (x, runReader (sub v) cxt) in
      foldl g cxt' $ unAbsEnv cxt'.fEnv

sub :: Value -> F Value
sub (Union a b)
  -- {... tag [a₁,...,aᵢ,...],...} ∪ {... tag [b₁,...,bᵢ,...],...} =
  -- {... tag [a₁ ∪ b₁,...,aᵢ ∪ b₁,...],...}
  | (as, bs) <- on (,) valueToList a b
  , _:_ <- intersectBy sameTag as bs =
    pure $ listToValue $ for as $ \case
      VNode tag vs
        | (_, vss) <- unzipWith (fromMaybe __IMPOSSIBLE__ . vnodeView) $ filter (hasTag tag) bs ->
          VNode tag $ foldl (zipWith Union) vs vss
      v -> v
 | a == b = pure a
 | otherwise = on (liftM2 Union) sub a b
 -- | otherwise = pure $ Union a b

sub (Location l)   = pure $ Location l -- fix point
sub Bas            = pure Bas -- fix point
sub (Variable x)   = fromMaybeM __IMPOSSIBLE__ $ envLookup x
sub (VNode tag vs) = VNode tag <$> mapM sub vs

sub (FETCH v )
  | Just l <- isLocation v = fromMaybeM __IMPOSSIBLE__ $ heapLookup l
  | Union{} <- v
  , (xs, ys) <- partition (isJust . isLocation) $ valueToList v
  , Just ls <- mapM isLocation xs
  = do
    xs' <- mapM (fromMaybeM __IMPOSSIBLE__ . heapLookup) ls
    pure $ listToValue $ xs' ++ ys
  | Union a b <- v = pure $ on Union FETCH a b
  | otherwise = FETCH <$> sub v
  -- | otherwise = pure $ FETCH v

sub (EVAL v)
  | VNode CTag{} _ <- v = pure v

  -- solve correct values
  | Union{} <- v
  , all isCNode $ valueToList v = pure v
  | Union{} <- v
  , (xs, ys) <- partition isCNode $ valueToList v
  , on (&&) (not . null) xs ys =
    pure $ Union (listToValue xs) $ EVAL (listToValue ys)

  -- filter wrong tags
  | Union{} <- v
  , (xs, ys) <- partition (\v -> not $ isPNode v || isFNode v) $ valueToList v
  , on (&&) (not . null) xs ys =
    pure $ EVAL $ listToValue xs

  | otherwise = EVAL <$> sub v

  where
    isCNode (VNode CTag{} _) = True
    isCNode _                = False

    isPNode (VNode FTag{} _) = True
    isPNode _                = False

    isFNode (VNode FTag{} _) = True
    isFNode _                = False

sub (Pick v tag1 i)
  | VNode tag2 vs <- v
  , tag1 == tag2 = pure $ vs !! i

  -- filter wrong tags
  | Union{} <- v
  , (xs, ys) <- partition (hasWrongTag tag1) $ valueToList v
  , on (&&) (not . null) xs ys =
    pure $ Pick (listToValue ys) tag1 i

  -- solve correct values
  | Union{} <- v
  , all (hasTag tag1) $ valueToList v =
    pure $ listToValue $ for (valueToList v) $ \(VNode _ vs) -> vs !! i
  | Union{} <- v
  , (xs, ys) <- partition (hasTag tag1) $ valueToList v
  , on (&&) (not . null) xs ys =
    pure $ Union (listToValue xs) $ Pick (listToValue ys) tag1 i


  | otherwise = (\v' -> Pick v' tag1 i) <$> sub v
  -- | otherwise = pure $ Pick v tag1 i

envLookup :: MonadReader FixCxt m => Variable -> m (Maybe Value)
envLookup x = asks $ lookup x . unAbsEnv . fEnv

heapUpdate :: FixCxt -> (Location, Value) -> FixCxt
heapUpdate cxt (l, v) =
    cxt{fHeap = AbsHeap hs}
  where
    hs = [ if l == l' then (l, v) else (l', v')
         | (l', v') <- unAbsHeap cxt.fHeap
         ]

envUpdate :: FixCxt -> (Variable, Value) -> FixCxt
envUpdate cxt (x, v) =
    cxt{fEnv = AbsEnv es}
  where
    es = [ if x == x' then (x, v) else (x', v')
         | (x', v') <- unAbsEnv cxt.fEnv
         ]

envInsert' :: FixCxt -> (Variable, Value) -> FixCxt
envInsert' cxt e = cxt{fEnv = AbsEnv $ insert e $ unAbsEnv cxt.fEnv}

heapLookup :: MonadReader FixCxt m => Location -> m (Maybe Value)
heapLookup l = asks $ lookup l . unAbsHeap . fHeap

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
listToValue :: [Value] -> Value
listToValue = foldr1 Union

isLocation :: Value -> Maybe Location
isLocation (Location l) = Just l
isLocation _            = Nothing

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

data Cxt = Cxt
  { gDefs      :: [GrinDefinition]
  , builtins   :: Builtins
  , vars       :: [VarInfo]
    -- ^ Only variables from the GRIN program.
    --   The integer is the corresponding Llvm variable (e.g. %4).
  , currentDef :: GrinDefinition
  }

initCxt :: [GrinDefinition] -> Builtins -> Cxt
initCxt defs builtins = Cxt
  { gDefs = defs
  , builtins = builtins
  , vars = []
  , currentDef = head defs
  }

data VarInfo = VarInfo
  { name :: String
  }

data Env = Env
  { varCount :: Int
  }

initEnv :: Env
initEnv = Env {varCount = 0}



type M a = StateT Env (Reader Cxt) a

grinToLlvm :: [GrinDefinition] -> M [L.Instruction]
grinToLlvm defs =
  forM defs $ \def -> do
    put initEnv
    let vars = take def.gArity $ map (VarInfo . singleton) ['a' ..]
    local (\cxt -> cxt {vars=vars , currentDef=def}) $ defToLlvm def

defToLlvm :: GrinDefinition -> M L.Instruction
defToLlvm GrinDefinition{gName, gType=Just typ, gTerm, gArity} = do
  (ts, t) <- bimapM (mapM typeToLlvm) typeToLlvm $ returnType typ
  body <- termToLlvm gTerm

  pure $
    L.Define
      L.Fastcc
      L.Ptr
      (L.MkVar $ prettyShow gName)
      (zip (replicate gArity L.Ptr) $ map singleton ['a' ..])
      body

termToLlvm :: Term -> M [L.Instruction]
termToLlvm (Bind t alt) = liftA2 (++) (termToLlvm t) (altToLlvm alt)

termToLlvm (Fetch (Var n)) = do
  x <- getVar n
  getelementptr <- setVar $
    L.Getelementptr nodeType
      [ (L.Ptr, L.Var x)
      , (L.I32, llvmLit 0)
      , (L.I32, llvmLit 0)
      ]
  x <- L.Var <$> getLlvmVar
  load <- setVar $
    L.Load L.I8 L.Ptr x

  pure
    [ getelementptr
    , load
    ]


termToLlvm (Case n def alts) = do
    x <- getVar n
    clauses <- altsToLlvm
    defClause <- L.Label "clause_def" <$> termToLlvm def

    pure $
      [ L.Switch caseType x (L.MkVar "clause_def") alts'
      ]
      ++
      snoc clauses defClause
  where
    alts'
      | Just t <- isLitCase =
        zipWith
          (\(AltLit l _) -> L.Alt t $ litToLlvm l)
          alts
          clauseLabels
      | isNodeCase =
        zipWith
          (\(AltNode t _) -> L.Alt L.I8 $ llvmLit $ tag t)
          alts
          clauseLabels
      | otherwise = __IMPOSSIBLE__

    caseType
      | Just t <- isLitCase = t
      | isNodeCase = L.I8
      | otherwise  = __IMPOSSIBLE__

    isLitCase :: Maybe L.Type
    isLitCase =
      mapM (fmap litToLlvmType . isLit) alts >>= \case
        []   -> __IMPOSSIBLE__
        x:xs -> foldr sameType (Just x) xs
      where
        sameType t1 (Just t2)
          | t1 == t2  = Just t2
          | otherwise = Nothing
        sameType _ Nothing  = Nothing

        isLit (AltLit lit _) = Just lit
        isLit _              = Nothing

    isNodeCase = flip all alts $ \case
      AltNode{} -> True
      _         -> False

    clauseLabels = take (length alts) mkClauseLabels

    altsToLlvm =
        zipWithM (\s i -> L.Label s <$> altToLlvm i) clauseLabels alts

termToLlvm (Unit v) = do
  v' <- valToLlvm v
  pure [L.Ret L.Ptr v']



termToLlvm (App (Prim PSub64) vs) = do
  pure [L.Comment "sub64"]
termToLlvm (App (Prim PAdd64) vs) = do
  pure [L.Comment "add64."]


termToLlvm (App v vs) = do
  v' <- valToLlvm v
  -- FIXME
  vs' <- zip (repeat L.Ptr) <$> mapM valToLlvm vs
  singleton <$> setVar (L.Call L.Fastcc L.Ptr L.Global v' vs')


termToLlvm t = pure $ singleton $ L.Comment $ prettyShow t

-- termToLlvm t = error $ "not implemented: " ++ show t


valToLlvm :: Val -> M L.Val
valToLlvm (Lit lit)  = pure $ litToLlvm lit
valToLlvm (Var n)    = L.Var <$> getVar n
valToLlvm (Def q)    = pure $ L.Var $ L.MkVar $ prettyShow q
valToLlvm (Node _ _) = pure L.Null -- FIXME
valToLlvm v          = error $ "valToLlvm not implemented: " ++ show v

litToLlvm :: Literal -> L.Val
litToLlvm = L.Lit

nodeType :: L.Type
nodeType = L.Alias $ L.MkVar "Node"

setVar :: MonadState Env m => L.Instruction -> m L.Instruction
setVar i = do
  modify $ \env -> env {varCount = 1 + env.varCount}
  x <- gets $ L.MkVar . show . varCount
  pure $ L.SetVar x i

mkClauseLabels :: [String]
mkClauseLabels = map (("clause_" ++) . show) [1 ..]

listType :: L.Type
listType = L.Structure [L.I8, L.Ptr, L.Ptr]

nodeType' :: Int -> L.Type
nodeType' n = L.Structure $ L.I8 : replicate n L.Ptr

-- TODO use local and increase vars
-- PROBLEM multiple variables for a term means that de bruijn indices are off
altToLlvm :: Alt -> M [L.Instruction]
altToLlvm = \case
  -- TODO claues
  AltNode tag t -> do
    scrutVar <- getVar 0
    let arity = tagArity tag

    -- TODO do same for full arity
    (newVars, is) <- fmap (second concat . unzip) $ forM [1 .. arity] $ \n -> do
      getelementptr <- setVar $
        L.Getelementptr (nodeType' arity)
          [ (L.Ptr, L.Var scrutVar)
          , (L.I32, llvmLit 0)
          , (L.I32, llvmLit n)]
      x <- L.Var <$> getLlvmVar
      load <- setVar $ L.Load nodeType L.Ptr x
      name <- gets $ show . varCount
      pure (VarInfo {name=name}, [getelementptr, load])

    rhs <- local (\cxt -> cxt { vars = newVars ++ cxt.vars }) $ termToLlvm t
    pure $ is ++ rhs

  AltLit _ t -> termToLlvm t


  AltVar t -> do
    -- Add the most recent LLVM variable to context. FIXME
    n <- gets varCount
    let newVar = VarInfo {name = show n}
    local (\cxt -> cxt { vars = newVar : cxt.vars }) (termToLlvm t)

  AltEmpty t -> termToLlvm t


typeToLlvm :: Type -> M L.Type
typeToLlvm t = asks $ flip go t where

  go cxt (I.El _ t)
    | Just prim <- mprim = prim
    where
      mprim = Map.lookup t cxt.builtins >>= \case
        BuiltinNat -> Just L.I64
        b          -> error $ "Not implemented: " ++ prettyShow b

  go _ (I.El _ t) = case t of
    I.Def q _-> L.Alias $ L.MkVar $ prettyShow q
    _         -> L.Ptr -- FIXME


litToLlvmType :: Literal -> L.Type
litToLlvmType LitNat{} = L.I64


getLlvmVar :: MonadState Env m => m L.Var
getLlvmVar = gets $ L.MkVar . show . varCount

getVar :: MonadReader Cxt m => Int -> m L.Var
getVar n = do
  vs <- asks vars
  pure $ maybe __IMPOSSIBLE__ (L.MkVar . name) (vs !!! n)

llvmLit :: Integral a => a -> L.Val
llvmLit = L.Lit . LitNat . toInteger

returnType :: Type -> ([Type], Type)
returnType = go . ([], ) where
  go (acc, I.El _ (I.Pi I.Dom{unDom} I.NoAbs{unAbs})) = go (snoc acc unDom , unAbs)
  go p = p
















































-----------------------------------------------------------------------
-- * Other
-----------------------------------------------------------------------

manual :: [GrinDefinition]
manual = [evalDef, downFromDef, sumDef, mainDef] where
  callEval = App (Def "eval") . singleton
  nilN = "[]"
  consN = "_∷_"
  sumN = "sum"
  downFromN = "downFrom"
  printfN = "printf"
  natN = "nat"

  evalDef = GrinDefinition {gType=Nothing, gTreeless=Nothing, gTerm=eval, gName="eval", gArity=1}
  eval =
    Bind
      (Fetch $ Var 0)
      (AltVar
        (Case 0 unreachable
          [ AltNode CTag {tTag=0, tCon=natN, tArity=1} (Unit $ Var 1)
          , AltNode CTag {tTag=1, tCon=nilN, tArity=0} (Unit $ Var 0)
          , AltNode CTag {tTag=2, tCon=consN, tArity=2} (Unit $ Var 2)
          , AltNode FTag {tTag=3, tDef=downFromN, tArity=1}
              (Bind
                (App (Def downFromN) [Var 0])
                (AltVar
                  (Bind
                    (Update (Var 2) (Var 0))
                    (AltEmpty
                      (Unit $ Var 0)
                    )
                  )
                )
              )
          , AltNode FTag {tTag=4, tDef=sumN, tArity=1}
              (Bind
                (App (Def sumN) [Var 0])
                (AltVar
                  (Bind
                    (Update (Var 2) (Var 0))
                    (AltEmpty
                      (Unit $ Var 0)
                    )
                  )
                )
              )
          ]
        )
      )

  downFromDef = GrinDefinition {gType=Nothing, gTreeless=Nothing, gTerm=downFrom, gName=downFromN, gArity=1}
  downFrom =
    Bind
      (callEval $ Var 0)
      (AltNode CTag{tTag=0, tCon=natN, tArity=1}
        (Case 0
          (Bind
            (App (Prim PSub64) [Var 0, Lit $ LitNat 1])
            (AltVar
              (Bind
                (Store $ Node CTag{tTag=0, tCon=natN, tArity=1} [Var 0])
                (AltVar
                  (Bind
                    (Store $ Node FTag {tTag=3, tDef=downFromN, tArity=1} [Var 0])
                    (AltVar
                      (Bind
                        (Store $ Node CTag{tTag=0, tCon=natN, tArity=1} [Var 2])
                        (AltVar
                          (Unit $ Node CTag {tTag=2, tCon=consN, tArity=2} [Var 0, Var 1])
                        )
                      )
                    )
                  )
                )
              )
            )
          )
          [ AltLit (LitNat 0) $ Unit $ Node CTag {tTag=1, tCon=nilN, tArity=0} []]
        )
      )


  sumDef = GrinDefinition {gType=Nothing, gTreeless=Nothing, gTerm=sum, gName=sumN, gArity=1}
  sum =
    Bind
      (callEval $ Var 0)
      (AltVar
        (Case 0 unreachable
          [ AltNode
              CTag {tTag=1, tCon=nilN, tArity=0}
              (Unit $ Node CTag { tTag=0, tCon=natN, tArity=1} [Lit $ LitNat 0])
          , AltNode
              CTag {tTag=2, tCon=consN, tArity=2}
              (Bind
                (callEval $ Var 1)
                (AltNode CTag{tTag=0, tCon=natN, tArity=1}
                  (Bind
                    (App (Def sumN) [Var 1])
                    (AltNode CTag{tTag=0, tCon=natN, tArity=1}
                      (Bind
                        (App (Prim PAdd64) [Var 1, Var 0])
                        (AltVar
                          (Unit $ Node CTag {tTag=0, tCon=natN, tArity=1} [Var 0])
                        )
                      )
                    )
                  )
                )
              )


          ]
        )
      )


  mainDef = GrinDefinition {gType=Nothing, gTreeless=Nothing, gTerm=main, gName="main", gArity=0}
  main =
    Bind
      (Store $ Node CTag {tTag=0, tCon=natN, tArity=1} [Lit $ LitNat 4])
      (AltVar
        (Bind
          (Store $ Node FTag {tTag=3, tDef=downFromN, tArity=1} [Var 0])
          (AltVar
            (Bind
              (Store $ Node FTag {tTag=4, tDef=sumN, tArity=1} [Var 0])
              (AltVar
                (Bind
                  (callEval $ Var 0)
                  (AltNode CTag {tTag=0, tCon=natN, tArity=1}
                    (App (Def printfN) [Var 0])
                  )
                )
              )
            )
          )
        )
      )


