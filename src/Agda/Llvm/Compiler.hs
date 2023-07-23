{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Agda.Llvm.Compiler (llvmBackend) where

import           Control.DeepSeq                       (NFData)
import           Control.Monad                         (forM, replicateM)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.State                   (MonadTrans (lift),
                                                        StateT, evalStateT,
                                                        gets)
import           Data.Foldable                         (foldrM, toList)
import           Data.Function                         (on)
import           Data.List                             (intercalate, singleton)
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           GHC.Generics                          (Generic)
import           Prelude                               hiding ((!!))

import           Agda.Compiler.Backend                 hiding (Prim, initEnv)
import           Agda.Compiler.Treeless.NormalizeNames (normalizeNames)
import           Agda.Interaction.Options
import           Agda.Syntax.TopLevelModuleName
import           Agda.TypeChecking.Substitute
import           Agda.Utils.Functor
import           Agda.Utils.Impossible
import           Agda.Utils.List
import           Agda.Utils.List1                      (List1, pattern (:|))
import qualified Agda.Utils.List1                      as List1
import           Agda.Utils.Maybe
import           Agda.Utils.Pretty

import           Agda.Llvm.Grin
import           Agda.Llvm.HeapPointsTo
import           Agda.Llvm.Utils
import           Agda.TypeChecking.SizedTypes.Utils    (trace)
import           Control.Applicative                   (Applicative (liftA2))
import           Control.Monad.Reader                  (MonadReader (local),
                                                        ReaderT (runReaderT),
                                                        ask, asks)


llvmBackend :: Backend
llvmBackend = Backend llvmBackend'

llvmBackend' :: Backend' LlvmOptions LlvmEnv LlvmModuleEnv LlvmModule (Maybe GrinDefinition)
llvmBackend' = Backend'
  { backendName           = "LLVM"
  , backendVersion        = Nothing
  , options               = defaultLlvmOptions
  , commandLineFlags      = llvmCommandLineFlags
  , isEnabled             = flagLlvmCompile
  , preCompile            = llvmPreCompile
  , postCompile           = llvmPostCompile
  , preModule             = \_ _ _ -> pure $ pure $ Recompile LlvmModuleEnv
  , postModule            = llvmPostModule
  , compileDef            = llvmCompileDef
  , scopeCheckingSuffices = False
  , mayEraseType          = const $ pure True
  }

newtype LlvmOptions = LlvmOptions
  { flagLlvmCompile :: Bool
  } deriving (Generic, NFData)

data LlvmEnv = LlvmEnv {}

data LlvmModuleEnv = LlvmModuleEnv
newtype LlvmModule = LlvmModule [GrinDefinition]

defaultLlvmOptions :: LlvmOptions
defaultLlvmOptions = LlvmOptions
  { flagLlvmCompile = False
  }

llvmCommandLineFlags :: [OptDescr (Flag LlvmOptions)]
llvmCommandLineFlags =
    [ Option []  ["llvm"] (NoArg enable)
      "compile program using the LLVM backend"
    ]
  where
    enable o = pure o{flagLlvmCompile = True}

llvmPreCompile :: LlvmOptions -> TCM LlvmEnv
llvmPreCompile _ = pure LlvmEnv

-- TODO need to filter unreachable functions
llvmCompileDef :: LlvmEnv
               -> LlvmModuleEnv
               -> IsMain
               -> Definition
               -> TCM (Maybe GrinDefinition)
llvmCompileDef env menv isMainModule Defn{defName, defType, theDef=Function{}} = do
    treeless <- maybeM __IMPOSSIBLE__ normalizeNames $ toTreeless LazyEvaluation defName
    let (arity, treeless') = skipLambdas treeless

    term' <- treelessToGrin isMain (simplifyApp treeless') arity
    gArgs <- replicateM arity freshAbs
    gReturn <- boolToMaybe (not isMain) <$> freshAbs

    let gDef  =
          GrinDefinition
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
    liftIO $ putStrLn $ intercalate "\n\n" (map prettyShow defs)

    liftIO $ putStrLn "\n------------------------------------------------------------------------"
    liftIO $ putStrLn "-- * Heap points-to analysis"
    liftIO $ putStrLn "------------------------------------------------------------------------\n"

    let (absCxt, share) = heapPointsTo defs
    liftIO $ putStrLn "\nAbstract heap: "
    liftIO $ putStrLn $ prettyShow absCxt.fHeap
    liftIO $ putStrLn "\nAbstract env: "
    liftIO $ putStrLn $ prettyShow absCxt.fEnv
    liftIO $ putStrLn $ "\nShared: " ++ prettyShow share


    defs' <- inlineEval defs absCxt
    liftIO $ putStrLn "\n------------------------------------------------------------------------"
    liftIO $ putStrLn "-- * Inlining eval"
    liftIO $ putStrLn "------------------------------------------------------------------------\n"
    liftIO $ putStrLn $ intercalate "\n\n" (map prettyShow defs')

  where
    defs = concatMap (\(LlvmModule xs) -> xs) (Map.elems mods)

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
-- • Use bind combinators
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
    alt <- altNode natTag (printf 0)
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

eval :: Int -> Term
eval = App (Def "eval") . singleton . Var

fetch :: Int -> Term
fetch = Fetch . Var

update :: Int -> Int -> Term
update = on Update Var

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

data WithOffset a = WithOffset
  { offset :: Int
  , value  :: a
  }

mkWithOffset :: Int -> a -> WithOffset a
mkWithOffset n a = WithOffset{offset = n, value = a}

-----------------------------------------------------------------------
-- * GRIN transformations
-----------------------------------------------------------------------


type E = ReaderT [Abs] TCM

-- | Specialize and inline calls to eval
--
-- eval 3 ; λ x14 →
-- >>>
-- (fetch 3 ; λ x42 →
--  (case 0 of
--     FPrim.sub x38 x39 → Prim.sub 0
--     Cnat x40 → unit 0
--  ) ; λ x41 →
--  update 2 0 ; λ () →
--  unit 0
-- ) ; λ x14 →
inlineEval :: [GrinDefinition] -> AbstractContext -> TCM [GrinDefinition]
inlineEval defs absCxt =
    forM defs $ \def -> do
      t <- runReaderT (go def.gTerm) (returnAbs ++ def.gArgs)
      pure def{gTerm = t}

  where
    returnAbs = mapMaybe gReturn defs

    localAbs :: Abs -> E a -> E a
    localAbs abs = local (abs :)

    localAbss :: [Abs] -> E a -> E a
    localAbss = foldl (.: localAbs) id

    heapLookup :: Loc -> Maybe Value
    heapLookup loc = lookup loc $ unAbsHeap absCxt.fHeap

    envLookup :: Abs -> Maybe Value
    envLookup abs = lookup abs $ unAbsEnv absCxt.fEnv

    go :: Term -> E Term
    go term = case term of
      App (Def "eval") [Var n] -> genEval n =<< deBruijnLookup n
      Case i t alts            -> liftA2 (Case i) (go t) (mapM goAlt alts)
      Bind t alt               -> liftA2 Bind (go t) (goAlt alt)
      App _ _                  -> pure term
      Store _ _                -> pure term
      Unit _                   -> pure term
      Fetch _                  -> pure term
      Update _ _               -> pure term
      Error _                  -> pure term

    goAlt :: Alt -> E Alt
    goAlt (AltEmpty t)         = AltEmpty <$> go t
    goAlt (AltVar abs t)       = AltVar abs <$> localAbs abs (go t)
    goAlt (AltNode tag abss t) = AltNode tag abss <$> localAbss abss (go t)
    goAlt (AltLit lit t)       = AltLit lit <$> go t

    genEval :: Int -> Abs -> E Term
    genEval n abs =
        fetch n    `bindVarR`
        caseOf     `bindVarL`
        update 2 0 `bindEmpty`
        Unit (Var 0)
      where
        caseOf :: E Term
        caseOf =
          Case 0 unreachable . toList <$>
          forM (collectTags abs) (\tag -> altNode tag $ genBody tag)

        genBody :: Tag -> Term
        genBody FTag{tDef, tArity} =
          App (Def tDef) $ map Var $ downFrom tArity
        genBody CTag{} = Unit $ Var 0
        genBody PTag{} = error "TODO"

    collectTags :: Abs -> List1 Tag
    collectTags abs =
        go $ fromMaybe __IMPOSSIBLE__ $ envLookup abs
      where
        go (Loc loc)     = go $ fromMaybe __IMPOSSIBLE__ $ heapLookup loc
        go (Union v1 v2) = List1.nub $ on (<>) go v1 v2
        go (VNode tag _) = List1.singleton tag
        go _             = __IMPOSSIBLE__

    deBruijnLookup :: Int -> E Abs
    deBruijnLookup n = asks $ fromMaybe __IMPOSSIBLE__ . (!!! n)

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

