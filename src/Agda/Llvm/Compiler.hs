{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Agda.Llvm.Compiler (llvmBackend) where

import           Control.DeepSeq                       (NFData)
import           Control.Monad                         (replicateM, zipWithM)
import           Control.Monad.Reader                  (MonadIO (liftIO),
                                                        MonadReader (ask, local),
                                                        Reader, asks, runReader,
                                                        when)
import           Control.Monad.State                   (MonadState (put), State,
                                                        StateT, evalState,
                                                        evalStateT, forM, get,
                                                        gets, modify, runState)
import           Data.Function                         (on)
import           Data.List                             (mapAccumL, nub)
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Maybe                            (catMaybes, fromJust,
                                                        fromMaybe)
import           Debug.Trace                           (trace)
import           GHC.Generics                          (Generic)
import           GHC.OldList                           (intercalate, singleton)
import           Prelude                               hiding ((!!))

import           Agda.Compiler.Backend                 hiding (Prim, initEnv)
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
import           Agda.TypeChecking.Substitute          (Subst (applySubst),
                                                        raise, raiseFrom,
                                                        raiseFromS, raiseS, wkS)
import           Agda.Utils.Functor
import           Agda.Utils.Impossible
import           Agda.Utils.List
import           Agda.Utils.List1                      (List1)
import           Agda.Utils.Maybe                      (allJustM, maybeM)
import           Agda.Utils.Pretty
import           Agda.Utils.Tuple                      (swap)
import           Control.Applicative                   (Applicative (liftA2))
import           Data.Bifunctor                        (Bifunctor (bimap, first, second))
import           Data.Bitraversable                    (bimapM)
import           Data.Foldable                         (foldrM)

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

    let term' = treelessToGrin (simplifyApp treeless') arity
    let gDef  = GrinDefinition
          {gType=Just defType
          ,gName=prettyShow defName
          ,gArity=arity
          ,gTerm=term'
          ,gTreeless=Just $ simplifyApp treeless
          }

    pure $ Just gDef
  -- where
  --   isNamedMain = "main" == (prettyShow . nameConcrete . qnameName) defName


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

    builtinThings <- getsTC $ foldr1 Map.union .
                     (\m -> map (iBuiltin . maybe __IMPOSSIBLE__ miInterface . flip Map.lookup m) $ Map.keys mods)
                     . stDecodedModules . stPersistentState

    let builtins = Map.fromList $ catMaybes $ flip map (Map.toList builtinThings) $ \case
          (BuiltinName n, Builtin t) -> Just (t, n)
          _                          -> Nothing

    -- liftIO $ putStrLn $ render $ vcat [text "BUILTINS:", nest 2 $ pretty builtins]

    -- let llvmInstructions = runReader (evalStateT (grinToLlvm defs) initEnv)
    --                      $ initCxt defs builtins
    -- liftIO $ putStrLn "\n------------------------------------------------------------------------"
    -- liftIO $ putStrLn "-- * LLVM"
    -- liftIO $ putStrLn "------------------------------------------------------------------------\n"
    -- liftIO $ putStrLn $ intercalate "\n\n" $ map prettyShow llvmInstructions

    liftIO $ putStrLn "\n------------------------------------------------------------------------"
    liftIO $ putStrLn "-- * Manual GRIN"
    liftIO $ putStrLn "------------------------------------------------------------------------\n"
    liftIO $ putStrLn $ intercalate "\n\n" $ map prettyShow manual



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
-- • Reuse evaluated variables
-- • Fix de Bruijn indices

-- Preconditions:
-- • Separate applications
-- • Lambda lifted
-- • No polymorphic functions?
-- • Saturated constructors
treelessToGrin :: TTerm -> Int -> Term
treelessToGrin t arity = evalState (rScheme t) $ initGEnv arity

data GVarInfo = GVarInfo
  { isEvaluated      :: Bool
  , evaluationOffset :: Maybe Int -- negative
  }

mkVar :: GVarInfo
mkVar = GVarInfo{isEvaluated=False, evaluationOffset=Nothing}

data GEnv = GEnv
  { gVars :: [GVarInfo]
  }

initGEnv :: Int -> GEnv
initGEnv arity = GEnv {gVars = replicate arity mkVar}

type G = State GEnv

rScheme :: TTerm -> G Term
rScheme (TCase n CaseInfo{caseType=CTNat} def alts) = do
  alts' <- mapM (aScheme . raise 1) alts
  def' <- rScheme $ raise 1 def
  pure $ Bind (eval n) $ AltNode natTag $ Case 0 def' alts'

rScheme (TCase n CaseInfo{caseType=CTData _} def alts) = do
  alts' <- mapM (aScheme . raise 1) alts
  def' <- rScheme $ raise 1 def
  pure $ Bind (eval n) $ AltNode natTag $ Case 0 def' alts'

-- | 𝓡 [_[]_] = unit (C_[]_)
rScheme (TCon q) = pure $ Unit $ Node tag [] where
  tag = CTag{tTag = 0, tCon = prettyShow q, tArity = 0}

-- | 𝓡 [_∷_ x xs] = unit (C_∷_ @1 @0)
rScheme (TApp (TCon q) as) =
   pure
     $ stores'
     $ Unit $ Node tag vs
  where
    tag = CTag{tTag = 0, tCon = prettyShow q, tArity = length as}

    stores' = foldr (.) id stores
    (stores, vs) =  foldr f ([], []) as

    f (TLit lit) (stores, vs) = (mkStore lit : stores, Var (length stores) : vs)
    f (TVar n)   (stores, vs) = (stores, Var n : vs)
    f _          _            = __IMPOSSIBLE__

    mkStore lit = Bind (Store $ Node natTag [Lit lit]) . AltVar


-- | 𝓡 [x + y] = eval @1 ; λ Cnat #1 →
--               eval @1 ; λ Cnat #1 →
--               add @1 @0 ; λ #1 →
--               unit (Cnat @1)
rScheme (TApp (TPrim prim) as) =
  pure
    $ evals'
    $ Bind (App (Prim prim) vs) $ AltVar
    $ Unit $ Node natTag [Var 0]

  where
    evals' = foldr (.) id evals
    (evals, vs) =  foldr f ([], []) as

    f (TLit lit) (evals, vs) = (evals, Lit lit : vs)
    f (TVar n)   (evals, vs) = (mkEval n : evals, Var (length evals) : vs)
    f _          _           = __IMPOSSIBLE__

    mkEval n = Bind (eval n) . AltNode natTag

-- | 𝓡 [foo x y] = foo x y
rScheme (TApp (TDef q) as) =
    pure
      $ stores'
      $ App (Def $ prettyShow q) vs
  where
    stores' = foldr (.) id stores
    (stores, vs) =  foldr f ([], []) as

    f (TLit lit) (stores, vs) = (mkStore lit : stores, Var (length stores) : vs)
    f (TVar n)   (stores, vs) = (stores, Var n : vs)
    f _          _            = __IMPOSSIBLE__

    mkStore lit = Bind (Store $ Node natTag [Lit lit]) . AltVar


-- | 𝓡 [let t1 in t2] = 𝓒 [t1] ; λ #1 → 𝓡 [t2]
rScheme (TLet t1 t2) = Bind <$> cScheme t1 <*> (AltVar <$> rScheme t2)
rScheme (TLit lit) = pure $ Unit $ Lit lit
rScheme (TError TUnreachable) = pure $ Error TUnreachable
rScheme t                     = error $ "TODO rScheme " ++ show t

aScheme :: TAlt -> G Alt
aScheme TALit{aLit, aBody} = AltLit aLit <$> rScheme aBody
aScheme TACon{aCon, aArity, aBody} = AltNode tag <$> rScheme aBody where
  tag = CTag{tTag = 0, tCon = prettyShow aCon, tArity = aArity}
aScheme alt                = error $ "TODO aScheme " ++ show alt


eScheme :: TTerm -> G Term
eScheme t = error $ "TODO eScheme " ++ show t


cScheme :: TTerm -> G Term

-- | 𝓒 [foo x y] = store (Ffoo @1 @0)
cScheme (TApp (TDef q) as) =
    pure
      $ stores'
      $ Store $ Node tag vs
  where
    tag = FTag{tTag = 0, tDef = prettyShow q, tArity = length as}

    stores' = foldr (.) id stores
    (stores, vs) =  foldr f ([], []) as

    f (TLit lit) (stores, vs) = (mkStore lit : stores, Var (length stores) : vs)
    f (TVar n)   (stores, vs) = (stores, Var n : vs)
    f _          _            = __IMPOSSIBLE__

    mkStore lit = Bind (Store $ Node natTag [Lit lit]) . AltVar


-- | 𝓒 [a + 4] = store (Cnat 4) λ #1 →
--               store (Prim.add @1 @0)
cScheme (TApp (TPrim prim) as) =
    pure
      $ stores'
      $ Store $ Node tag vs
  where
    tag = FTag{tTag = 0, tDef = primStr prim, tArity = length as}

    stores' = foldr (.) id stores
    (stores, vs) =  foldr f ([], []) as

    f (TLit lit) (stores, vs) = (mkStore lit : stores, Var (length stores) : vs)
    f (TVar n)   (stores, vs) = (stores, Var n : vs)
    f _          _            = __IMPOSSIBLE__

    mkStore lit = Bind (Store $ Node natTag [Lit lit]) . AltVar




cScheme t = error $ "TODO cScheme " ++ show t


natTag :: Tag
natTag = CTag{tTag=0, tCon="nat", tArity=1}

eval :: Int -> Term
eval = App (Def "eval") . singleton . Var

primStr :: TPrim -> String
primStr PAdd64 = "Prim.add"
primStr PAdd   = "Prim.add"
primStr PSub64 = "Prim.sub"
primStr PSub   = "Prim.sub"
primStr p      = error $ "TODO primStr " ++ show p



-- | Primitive operations need the arguments to be evaluated, unboxed,
--   and then boxed again.
--
--   𝓡 [let b = a - 1 in foo b]  =
--   𝓔 [a - 1] ; λ b → 𝓡 [foo b] =
--   eval a ; λ Cnat #1 →
--   Sub64 @0 1 ; λ #1 →
--   store (Cnat @0) ; λ #1 →
--   foo @0
--
-- • TODO Should primitives always be forced?
{-
rScheme (TLet (TApp (TPrim prim) as) t2) = do
    t2' <- rScheme (raiseFrom 1 indexOffset t2)
    pure
      $ evals'
      $ Bind (App (Prim prim) vs) $ AltNode natTag
      $ Bind (Store $ Node natTag [Var 0]) $ AltVar t2'
  where
    indexOffset = length evals + 1

    evals' = foldr (.) id evals
    (evals, vs) =  foldr f ([], []) as

    f (TLit lit) (evals, vs) = (evals, Lit lit : vs)
    f (TVar n)   (evals, vs) = (mkEval n : evals, Var (length evals) : vs)
    f _          _           = __IMPOSSIBLE__

    mkEval n = Bind (eval n) . AltNode natTag
-}


-----------------------------------------------------------------------
-- * GRIN heap points-to analysis
-----------------------------------------------------------------------


--
-- "determine for each call to eval, a safe approximation to what different node
-- values (or rather tags) that eval might find when it fetches a node from the
-- heap via its argument pointer." (s. 67)
--
-- • abstract locations: {1, 2, ...,maxloc} where maxloc is total number of store
--   operations
-- • the abstract store contains a set of...
--
-- Abstract store maps heap locations ({1, 2, ...,maxloc}) to a set of nodes:
-- 1 → {Cnat [{BAS}]}
-- 2 → {FdownFrom {[{1}]}}
-- 3 → {Cnat [{BAS}]}
--
-- Abstract enviroment maps variables to a set of values:
heapPointsTo = undefined -- TODO





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


