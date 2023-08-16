-- {-# OPTIONS_GHC -Wincomplete-patterns -Woverlapping-patterns #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ViewPatterns             #-}

module Agda.Llvm.Compiler (module Agda.Llvm.Compiler) where

import           Control.Applicative            (Applicative (liftA2))
import           Control.DeepSeq                (NFData)
import           Control.Monad                  (forM, replicateM)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Reader           (MonadReader (local),
                                                 ReaderT (runReaderT), asks)
import           Control.Monad.State            (StateT (runStateT), evalStateT,
                                                 gets, modify)
import           Data.Foldable                  (find, foldrM, toList)
import           Data.Function                  (on)
import           Data.List                      (intercalate, singleton)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           GHC.Generics                   (Generic)
import           Prelude                        hiding ((!!))

import           Agda.Compiler.Backend          hiding (Prim, initEnv)
import           Agda.Interaction.Options
import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.TopLevelModuleName
import           Agda.TypeChecking.Substitute
import           Agda.Utils.Functor
import           Agda.Utils.Impossible
import           Agda.Utils.List
import           Agda.Utils.List1               (List1, pattern (:|), (<|))
import qualified Agda.Utils.List1               as List1
import           Agda.Utils.Maybe

import           Agda.Llvm.Grin
import           Agda.Llvm.GrinInterpreter      (interpretGrin)
import           Agda.Llvm.HeapPointsTo
import           Agda.Llvm.TreelessTransform
import           Agda.Llvm.Utils
import           Agda.Utils.Lens
import           GHC.IO                         (unsafePerformIO)


llvmBackend :: Backend
llvmBackend = Backend llvmBackend'

llvmBackend' :: Backend' LlvmOptions LlvmEnv LlvmModuleEnv LlvmModule (Maybe TreelessDefinition)
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
newtype LlvmModule = LlvmModule [TreelessDefinition]

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
               -> TCM (Maybe TreelessDefinition)

llvmCompileDef _ _ = definitionToTreeless

llvmPostModule :: LlvmEnv
               -> LlvmModuleEnv
               -> IsMain
               -> TopLevelModuleName
               -> [Maybe TreelessDefinition]
               -> TCM LlvmModule
llvmPostModule _ _ _ _ defs =
  pure $ LlvmModule $ catMaybes defs

llvmPostCompile :: LlvmEnv
                -> IsMain
                -> Map TopLevelModuleName LlvmModule
                -> TCM ()
llvmPostCompile _ _ mods = do

  let tl_defs = concatMap (\(LlvmModule xs) -> xs) (Map.elems mods)
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Treeless"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow tl_defs

  gr_defs <- treelessToGrin tl_defs
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * GRIN"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow gr_defs

  let (absCxtEqs, absCxt, share) = heapPointsTo gr_defs
  let tagInfo_pointsTo = initTagInfo absCxt
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Heap points-to analysis"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn "Equations"
    putStrLn $ prettyShow absCxtEqs
    putStrLn ""
    putStrLn $ prettyShow absCxt
    putStrLn $ "\nSharing Heap: " ++ prettyShow share
    putStrLn $ "\nTag Info:\n" ++ prettyShow tagInfo_pointsTo

  (defs_evalInlined, tagInfo_inlineEval) <- inlineEval gr_defs absCxt tagInfo_pointsTo
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Inlining Eval"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_evalInlined
    putStrLn $ "\nTag Info:\n" ++ prettyShow tagInfo_inlineEval

  res_evalInlined <- interpretGrin defs_evalInlined
  liftIO $ putStrLn $ "\nResult: " ++ show res_evalInlined

  let defs_normalised = map (updateGrTerm normalise) defs_evalInlined
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Normalise"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_normalised

  res_normalised <- interpretGrin defs_normalised
  liftIO $ putStrLn $ "\nResult: " ++ show res_normalised

  let defs_removeUnit = map (updateGrTerm removeUnitBind) defs_normalised
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Remove unit binds"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_removeUnit

  res_removeUnit <- interpretGrin defs_removeUnit
  liftIO $ putStrLn $ "\nResult: " ++ show res_removeUnit

  let defs_updateSpecialized = map (updateGrTerm specializeUpdate) defs_removeUnit
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Specialize Update"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_updateSpecialized

  res_updateSpecialized <- interpretGrin defs_updateSpecialized
  liftIO $ putStrLn $ "\nResult: " ++ show res_updateSpecialized

  defs_vectorized <- mapM (lensGrTerm $ vectorize tagInfo_inlineEval) defs_updateSpecialized
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Vectorization"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_vectorized

  res_vectorized <- interpretGrin defs_vectorized
  liftIO $ putStrLn $ "\nResult: " ++ show res_vectorized

  let defs_caseSimplified = map (updateGrTerm simplifyCase) defs_vectorized
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Simplify case"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_caseSimplified

  res_caseSimplified <- interpretGrin defs_caseSimplified
  liftIO $ putStrLn $ "\nResult: " ++ show res_caseSimplified

  let defs_fetchSplit = map (updateGrTerm splitFetch) defs_caseSimplified
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Split fetch"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_fetchSplit

  res_fetchSplit <- interpretGrin defs_fetchSplit
  liftIO $ putStrLn $ "\nResult: " ++ show res_fetchSplit

  let defs_removeUnit = map (updateGrTerm removeUnitBind) defs_fetchSplit
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Remove unit binds"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_removeUnit

  res_removeUnit <- interpretGrin defs_removeUnit
  liftIO $ putStrLn $ "\nResult: " ++ show res_removeUnit

  defs_rightHoistedFetch <- mapM (lensGrTerm rightHoistFetch) defs_removeUnit
  liftIO $ do
    putStrLn "\n------------------------------------------------------------------------"
    putStrLn "-- * Right hoist fetch"
    putStrLn "------------------------------------------------------------------------\n"
    putStrLn $ intercalate "\n\n" $ map prettyShow defs_rightHoistedFetch

  res_rightHoistedFetch <- interpretGrin defs_rightHoistedFetch
  liftIO $ putStrLn $ "\nResult: " ++ show res_rightHoistedFetch

-----------------------------------------------------------------------
-- * GRIN code generation
-----------------------------------------------------------------------

-- TODO
-- • Refactor (use Reader instead of State)
-- • Use bind combinators
-- • Reuse evaluated variables
-- • Fill in rest of the patterns

-- Preconditions:
-- • Separate applications
-- • Lambda lifted
-- • No polymorphic functions?
-- • Saturated constructors
treelessToGrin :: [TreelessDefinition] -> TCM [GrinDefinition]
treelessToGrin defs = forM defs $ \def -> do
  gr_term <- evalStateT (rScheme def.tl_term) $
           initGEnv def.tl_arity def.tl_isMain
  gr_args <- replicateM def.tl_arity freshAbs
  gr_return <- boolToMaybe (not def.tl_isMain) <$> freshAbs
  pure $ GrinDefinition
    { gr_name = def.tl_name
    , gr_isMain = def.tl_isMain
    , gr_arity = def.tl_arity
    , gr_type = Just def.tl_type
    , gr_term = gr_term
    , gr_args = gr_args
    , gr_return = gr_return
    }

rScheme :: TTerm -> G Term
rScheme (TCase n CaseInfo{caseType=CTNat} def alts) = do
  alts' <- mapM (aScheme . raise 1) alts
  def' <- rScheme (raise 1 def)
  Bind (eval n) <$> laltConstantNode natTag (Case (Var 0) def' alts')

rScheme (TCase n CaseInfo{caseType=CTData _} def alts) = do
  alts' <- mapM (aScheme . raise 1) alts
  def' <- rScheme (raise 1 def)
  Bind (eval n) <$> laltVar (Case (Var 0) def' alts')

-- | 𝓡 [_[]_] = unit (C_[]_)
rScheme (TCon q) = pure $ Unit $ ConstantNode tag [] where
  tag = CTag{tCon = prettyShow q, tArity = 0}
rScheme (TLit lit) = pure $ Unit $ ConstantNode natTag [Lit lit]
rScheme (TError TUnreachable) = pure $ Error TUnreachable

rScheme (TApp t as) = do
    isMain <- gets isMain
    alt <- laltConstantNode natTag (printf 0)
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
        fin <- App (Prim prim) vs `bindVar` res (Unit $ ConstantNode natTag [Var 0])
        evals' <- foldrM (\t ts -> Bind t <$> laltConstantNode natTag ts) fin evals
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
            s <- store $ ConstantNode natTag [Lit lit]
            pure (s : ss, Var (length ss) : vs)
        f (TVar n)   (ss, vs) = pure (ss, Var (n + nLits) : vs)
        f _          _        = __IMPOSSIBLE__

      (stores, vs) <- foldrM f ([], []) as
      let fin = res $ App (Def $ prettyShow q) vs
      stores' <- foldrM (\t ts -> Bind t <$> laltVar ts) fin stores

      pure $ mkWithOffset nLits stores'

    -- | 𝓡 [_∷_ x xs] = unit (C_∷_ @1 @0)
    appCon res q as = do
        let
          nLits = foldl (\n -> \case{TLit _ -> succ n ; _ -> n}) 0 as

          f (TLit lit) (ss, vs) = do
            s <- store $ ConstantNode natTag [Lit lit]
            pure (s : ss, Var (length ss) : vs)
          f (TVar n)   (ss, vs) = pure (ss, Var (n + nLits) : vs)
          f _          _        = __IMPOSSIBLE__

        (stores, vs) <- foldrM f ([], []) as
        let fin = res $ Unit $ ConstantNode tag vs
        stores' <- foldrM (\t ts -> Bind t <$> laltVar ts) fin stores

        pure $ mkWithOffset (length stores) stores'
      where
        tag = CTag{tCon = prettyShow q, tArity = length as}

-- | 𝓡 [let t1 in t2] = 𝓒 [t1] ; λ #1 → 𝓡 [t2]
rScheme (TLet t1 t2)
  | TApp t as <- t1 = do
    WithOffset{value=t1', offset} <- cSchemeApp t as
    t2' <- rScheme $ raiseFrom 1 offset t2
    pure $ t1' t2'

  | TLet t1 t2 <- t1 = do
    t1' <- cScheme t1
    t2' <- rScheme t2
    Bind t1' <$> laltVar t2'

rScheme t = error $ "TODO rScheme " ++ show t

aScheme :: TAlt -> G CAlt
aScheme TALit{aLit, aBody} = do
  aBody' <- rScheme aBody
  pure $ CAltLit aLit aBody'

aScheme TACon{aCon, aArity, aBody} = do
    aBody' <- rScheme aBody
    caltConstantNode tag aBody'
  where
    tag = CTag{tCon = prettyShow aCon, tArity = aArity}

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
          s <- store $ ConstantNode natTag [Lit lit]
          pure (s : ss, Var (length ss) : vs)
        f (TVar n)   (ss, vs) = pure (ss, Var (n + nLits) : vs)
        f _          _        = __IMPOSSIBLE__

    (stores, vs) <-  foldrM f ([], []) as

    -- let fin' = res $ Unit $ Node tag vs
    t <- store $ ConstantNode tag vs
    abs <- freshAbs
    let fin = Bind t . LAltVar abs
    stores' <- foldrM (\t ts -> (\abs -> Bind t . LAltVar abs . ts) <$> freshAbs) fin stores
    pure $ mkWithOffset nLits stores'
  where
  tag
    | TDef q <- t = FTag{tDef = prettyShow q, tArity = length as}
    | TPrim prim <- t =  FTag {tDef=primStr prim, tArity=length as}
    | otherwise = __IMPOSSIBLE__

natTag :: Tag
natTag = CTag{tCon = "nat" , tArity = 1}

eval :: Int -> Term
eval = App (Def "eval") . singleton . Var

printf :: Int -> Term
printf = App (Def "printf") . singleton . Var

-- FIXME
primStr :: TPrim -> String
-- primStr PAdd64 = "Prim.add"
primStr PAdd = "Prim.add"
-- primStr PSub64 = "Prim.sub"
primStr PSub = "Prim.sub"
primStr p    = error $ "TODO primStr " ++ show p

strPrim :: String -> TPrim
-- strPrim "Prim.add" = PAdd64
strPrim "Prim.add" = PAdd
-- strPrim "Prim.sub" = PSub64
strPrim "Prim.sub" = PSub
strPrim p          = error $ "TODO primStr " ++ show p

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


newtype TagInfo = TagInfo{unTagInfo :: Map Abs (Set Tag)}

lensTagInfo :: Lens' TagInfo (Map Abs (Set Tag))
lensTagInfo f tagInfo = TagInfo <$> f tagInfo.unTagInfo

tagInfoInsert :: Abs -> Set Tag -> TagInfo -> TagInfo
tagInfoInsert = over lensTagInfo .: Map.insert

lookupMaxArity :: Abs -> TagInfo -> Maybe Int
lookupMaxArity abs tagInfo =
  maximum . map tArity . Set.toList <$> Map.lookup abs tagInfo.unTagInfo

singletonTag :: Abs -> TagInfo -> Maybe Tag
singletonTag abs tagInfo =
  Map.lookup abs tagInfo.unTagInfo >>= \set ->
    boolToMaybe (Set.size set == 1) (Set.elemAt 0 set)

instance Pretty TagInfo where
  pretty (TagInfo tagInfo) =
      vcat $ map prettyEntry $ Map.toList tagInfo
    where
      prettyEntry :: (Abs, Set Tag) -> Doc
      prettyEntry (x, tags) =
            pretty x
        <+> text "→"
        <+> pretty tags

initTagInfo :: AbstractContext -> TagInfo
initTagInfo absCxt =
  TagInfo $ Map.fromList $ forMaybe (absCxt ^. lensAbsEnv) $ \(abs, v) ->
    case v of
      VNode tag _ -> Just (abs, Set.singleton tag)
      v@Union{} ->
        let tags = flip List1.mapMaybe (valueToList v) $ \case
                   VNode tag _ -> Just tag
                   _           -> Nothing in
        caseList tags Nothing $ \t ts -> Just (abs, Set.fromList $ t : ts)
      Loc _ -> Nothing
      Bas -> Nothing
      _ -> __IMPOSSIBLE__

valueToTags :: Value -> Maybe (Set Tag)
valueToTags = \case
      VNode tag _ -> Just $ Set.singleton tag
      v@Union{} ->
        let tags = flip List1.mapMaybe (valueToList v) $ \case
                   VNode tag _ -> Just tag
                   _           -> Nothing in
        caseList tags Nothing $ \t ts -> Just $ Set.fromList $ t : ts
      Loc _ -> Nothing
      Bas -> Nothing
      _ -> __IMPOSSIBLE__

type E m = ReaderT [Abs] (StateT TagInfo m)

-- | Specialize and inline calls to eval
--
-- eval 3 ; λ x14 →
-- >>>
-- (fetch 3 ; λ x42 →
--  (case 0 of
--     FPrim.sub x38 x39 → Prim.sub 1 0
--     Cnat x40 → unit 0
--  ) ; λ x41 →
--  update 2 0 ; λ () →
--  unit 0
-- ) ; λ x14 →
inlineEval :: forall mf. MonadFresh Int mf
           => [GrinDefinition]
           -> AbstractContext
           -> TagInfo
           -> mf ([GrinDefinition], TagInfo)
inlineEval defs absCxt tagInfo =
    flip runStateT tagInfo $ forM defs $ \def -> do
      t <- runReaderT (go def.gr_term) (def.gr_args ++ returnAbs)
      pure def{gr_term = t}
  where
    returnAbs = mapMaybe gr_return defs

    localAbs :: Abs -> E mf a -> E mf a
    localAbs abs = local (abs :)

    localAbss :: [Abs] -> E mf a -> E mf a
    localAbss = foldl (.: localAbs) id

    heapLookup :: Loc -> Maybe Value
    heapLookup loc = lookup loc $ unAbsHeap absCxt.fHeap

    envLookup :: Abs -> Maybe Value
    envLookup abs = lookup abs $ unAbsEnv absCxt.fEnv

    -- ugly
    mkTagSet :: Abs -> Set Tag
    mkTagSet x = flip foldMap (collectTags x) $ \case
      FTag{tDef} ->
        let xs =
              forMaybe defs $ \def ->
                boolToMaybe (tDef == def.gr_name) =<< def.gr_return in
        caseList xs (Set.singleton natTag) $ \x _ ->
          fromMaybe __IMPOSSIBLE__ $ valueToTags =<< envLookup x
      PTag{} -> error "TODO"
      ctag -> Set.singleton ctag


    go :: Term -> E mf Term
    go term = case term of
      App (Def "eval") [Var n] `Bind` LAltVar x1 t1 -> do
          x2 <- deBruijnLookup n
          let tagSet = mkTagSet x2
          modify (tagInfoInsert x1 tagSet)
          t2 <- genEval n x2 tagSet
          t1' <- localAbs x1 $ go t1
          pure $ t2 `Bind` LAltVar x1 t1'
      App (Def "eval") [Var n] -> do
        x <- deBruijnLookup n
        genEval n x $ mkTagSet x
      Case i t alts            -> liftA2 (Case i) (go t) (mapM goCalt alts)
      Bind t alt               -> liftA2 Bind (go t) (goLalt alt)
      _ -> pure term

    goLalt :: LAlt -> E mf LAlt
    goLalt (LAltEmpty t)         = LAltEmpty <$> go t
    goLalt (LAltVar abs t)       = LAltVar abs <$> localAbs abs (go t) -- FIXME need to assign this abs
    goLalt (LAltConstantNode tag abss t) = LAltConstantNode tag abss <$> localAbss abss (go t)

    goCalt :: CAlt -> E mf CAlt
    goCalt (CAltConstantNode tag abss t) = CAltConstantNode tag abss <$> localAbss abss (go t)
    goCalt (CAltLit lit t)       = CAltLit lit <$> go t


    genEval :: Int -> Abs -> Set Tag -> E mf Term
    genEval n x1 tagSet
      | tag :| [] <- collectTags x1 = do

        x2 <- freshAbs
        modify (tagInfoInsert x2 tagSet)

        body <- genBody tag

        let infixr 2 `bindConstantNode`, `bindVar'`
            bindConstantNode t1 t2 = Bind t1 <$> laltConstantNode tag t2
            bindVar' t1 t2 = t1 `Bind` LAltVar x2 t2 in

          -- Generated term
          FetchNode n                    `bindConstantNode`
          body                           `bindVar'`
          Update Nothing (n + 2) (Var 0) `BindEmpty`
          Unit (Var 0)

      | otherwise = do

        let tags = collectTags x1
        x2 <- freshAbs
        modify (tagInfoInsert x2 $ Set.fromList $ toList tags)

        x3 <- freshAbs
        modify (tagInfoInsert x3 tagSet)

        let infixr 2 `bindVarL'`, `bindVarR'`
            bindVarR' t1 t2 = t2 <&> \t2' ->  t1 `Bind` LAltVar x2 t2'
            bindVarL' t1 t2 = t1 <&> \t1' ->  t1' `Bind` LAltVar x3 t2
            caseOf =
              Case (Var 0) unreachable . toList <$>
              forM tags (\tag -> caltConstantNode tag =<< genBody tag) in

          -- Generated term
          FetchNode n                    `bindVarR'`
          caseOf                         `bindVarL'`
          Update Nothing (n + 2) (Var 0) `BindEmpty`
          Unit (Var 0)

      where

        genBody :: Tag -> E mf Term
        genBody FTag{tDef, tArity}
          | isJust $ find ((tDef==). gr_name) defs =
            pure $ App (Def tDef) $ map Var $ downFrom tArity
          -- Wrap primitive values
          | otherwise =
            let prim = Prim $ strPrim tDef
                ts = (map Var $ downFrom tArity) in
            App prim ts `bindVar`
            Unit (ConstantNode natTag [Var 0])

        genBody tag@CTag{tArity}
          | tArity == 0 = pure $ Unit $ Tag tag
          | otherwise = pure $ Unit $ ConstantNode tag $ map Var $ downFrom tArity

        genBody PTag{} = error "TODO"

    collectTags :: Abs -> List1 Tag
    collectTags abs =
        go $ fromMaybe __IMPOSSIBLE__ $ envLookup abs
      where
        go (Loc loc)     = go $ fromMaybe __IMPOSSIBLE__ $ heapLookup loc
        go (Union v1 v2) = List1.nub $ on (<>) go v1 v2
        go (VNode tag _) =
          List1.singleton tag
        go _             = __IMPOSSIBLE__

    deBruijnLookup :: Int -> E mf Abs
    deBruijnLookup n = asks $ fromMaybe __IMPOSSIBLE__ . (!!! n)


removeUnitBind :: Term -> Term
-- unit n ; λ xₘ →
-- 〈t 〉
-- >>>
-- 〈t 〉[n / m]
removeUnitBind (Unit (Var n) `Bind` LAltVar abs t) =
    applySubst rho $ removeUnitBind t
  where
    rho = strengthenS impossible 1 `composeS` singletonS 0 (Var $ n + 1)

-- unit (tag v₁ v₂) ; λ tag x₁ x₂ →
-- 〈t 〉
-- >>>
-- 〈t 〉[v₁ / 1, v₂ / 0]
removeUnitBind (Unit (ConstantNode tag1 vs) `Bind` LAltConstantNode tag2 xs t)
  | tag1 == tag2 =
    let
      arity = tagArity tag1
      vs' = raise arity $ reverse vs
      rho =
        strengthenS impossible arity `composeS`
        foldr composeS IdS (zipWith inplaceS [0 .. arity - 1] vs')
    in
    applySubst rho $ removeUnitBind t

  | otherwise = __IMPOSSIBLE__

removeUnitBind term = case term of
  Bind t alt    -> Bind (removeUnitBind t) (removeUnitBindLalt alt)
  Case n t alts -> Case n (removeUnitBind t) (map removeUnitBindCalt alts)
  _             -> term

removeUnitBindLalt :: LAlt -> LAlt
removeUnitBindLalt (LAltVar abs t) = LAltVar abs $ removeUnitBind t
removeUnitBindLalt (LAltConstantNode tag abss t) = LAltConstantNode tag abss $ removeUnitBind t
removeUnitBindLalt (LAltVariableNode x abss t) = LAltVariableNode x abss $ removeUnitBind t
removeUnitBindLalt (LAltEmpty t) = LAltEmpty $ removeUnitBind t

removeUnitBindCalt :: CAlt -> CAlt
removeUnitBindCalt (CAltConstantNode tag abss t) = CAltConstantNode tag abss $ removeUnitBind t
removeUnitBindCalt (CAltLit lit t) = CAltLit lit $ removeUnitBind t
removeUnitBindCalt (CAltTag tag t) = CAltTag tag $ removeUnitBind t

-- | Normalise the GRIN expression by making the expression right-skewed.
normalise :: Term -> Term
normalise (Bind t1 alt1)
  | Bind t2 alt2 <- normalise t1 =
    let alt1' = raise (countBinders alt2) $ normaliseLalt alt1
        (mkAlt2, t_alt2) = splitLalt alt2 in
    normalise $ Bind t2 (mkAlt2 (Bind t_alt2 alt1'))

normalise term = case term of
  Bind t alt    -> Bind (normalise t) (normaliseLalt alt)
  Case n t alts -> Case n (normalise t) (map normaliseCalt alts)
  _             -> term

normaliseLalt :: LAlt -> LAlt
normaliseLalt (LAltVar abs t)       = LAltVar abs $ normalise t
normaliseLalt (LAltConstantNode tag abss t) = LAltConstantNode tag abss $ normalise t
normaliseLalt (LAltEmpty t)         = LAltEmpty $ normalise t


normaliseCalt :: CAlt -> CAlt
normaliseCalt (CAltConstantNode tag abss t) = CAltConstantNode tag abss $ normalise t
normaliseCalt (CAltLit lit t)       = CAltLit lit $ normalise t

countBinders :: LAlt -> Int
countBinders (LAltConstantNode _ abss _) = length abss
countBinders LAltVar{}                   = 1
countBinders LAltEmpty{}                 = 0

-- TODO Fix returning eval [Boquist 1999, p. 95]
specializeUpdate :: Term -> Term

    -- update n₁ n₂ ; λ () →
    -- 〈m₁ 〉
    -- case n₂ of
    --   CNil       → 〈m₂ 〉
    --   CCons x xs → 〈m₃ 〉
    -- >>>
    -- 〈m₁ 〉
    -- case n₂ of
    --   CNil       →
    --     updateᶜᴺⁱˡ n₁' n₂' ; λ () →
    --     〈m₂ 〉
    --   CCons x xs →
    --     updateᶜᶜᵒⁿˢ n₁' n₂' ; λ () →
    --     〈m₃ 〉
specializeUpdate (caseUpdateView -> Just (mkUpdate, m, Case t1 t2 alts)) =
    specializeUpdate $ m $ Case t1 t2 alts'
  where
    alts' =
      for alts $ \case
        CAltConstantNode tag abss t3 ->
          let update = raise tag.tArity $ mkUpdate tag in
          CAltConstantNode tag abss $ update `BindEmpty` t3
        _ -> __IMPOSSIBLE__

-- update n₁ n₂ ; λ () →
-- unit n₂ ; λ CNat x →
-- 〈m 〉
-- >>>
-- updateᶜᴺᵃᵗ n₁ n₂ ; λ () →
-- unit n₂ ; λ CNat x →
-- 〈m 〉
specializeUpdate (
  Update Nothing n1 n2 `BindEmpty`
  Unit (Var n2') `Bind` LAltConstantNode tag abss t) =
  -- >>>
  Update (Just tag) n1 n2 `BindEmpty`
  Unit (Var n2') `Bind` LAltConstantNode tag abss (specializeUpdate t)

specializeUpdate (Bind t alt) =
  specializeUpdate t `Bind` specializeUpdateLalt alt
specializeUpdate (Case n t alts) =
  Case n (specializeUpdate t) (map specializeUpdateCalt alts)
specializeUpdate (Update mtag n1 n2)
  | Nothing <- mtag = __IMPOSSIBLE__
  | otherwise = Update mtag n1 n2
specializeUpdate t = t

specializeUpdateCalt :: CAlt -> CAlt
specializeUpdateCalt (CAltConstantNode tag abss t) = CAltConstantNode tag abss $ specializeUpdate t
specializeUpdateCalt (CAltLit lit t)       = CAltLit lit $ specializeUpdate t


specializeUpdateLalt :: LAlt -> LAlt
specializeUpdateLalt (LAltVar abs t)       = LAltVar abs $ specializeUpdate t
specializeUpdateLalt (LAltConstantNode tag abss t) = LAltConstantNode tag abss $ specializeUpdate t
specializeUpdateLalt (LAltEmpty t)         = LAltEmpty $ specializeUpdate t

-- Preconditions: Normalised
--
-- update n₁ n₂ ; λ () →
-- 〈m 〉where n₁ ∉ m
-- case n₂ of alts
--
-- Returns: (update n₁' n₂', 〈m 〉, case n₂ of alts)
caseUpdateView :: Term -> Maybe (Tag -> Term, Term -> Term, Term)
caseUpdateView (Update Nothing n t1 `BindEmpty` t) = go IdS id t where
  go :: Substitution' Val
     -> (Term -> Term)
     -> Term
     -> Maybe (Tag -> Term,Term -> Term, Term)
  go rho m (Case t2 t3 alts)
    | t1' <- applySubst rho t1
    , t1' == t2 =
      let n' = case lookupS rho n of
                 Var n -> n
                 _     -> __IMPOSSIBLE__ in
      Just (\tag -> Update (Just tag) n' t1', m , Case t2 t3 alts)
    | otherwise = Nothing
  go rho m (Bind t alt) = goLalt rho (m . Bind t) alt
  go _ _ _ = Nothing

  goLalt :: Substitution' Val
        -> (LAlt -> Term)
        -> LAlt
        -> Maybe (Tag -> Term, Term -> Term, Term)
  goLalt rho m (LAltVar abs t) = go (raiseS 1 `composeS` rho) (m . LAltVar abs) t
  goLalt rho m (LAltConstantNode tag abss t) =
    go (raiseS tag.tArity `composeS` rho) (m . LAltConstantNode tag abss) t
  goLalt rho m (LAltEmpty t) = go rho (m . LAltEmpty) t


caseUpdateView _ = Nothing

-- | Replace all node variables by explicit nodes.
--
-- <t1> ; λ x₁ →
-- <t2>
-- >>>
-- <t1> ; λ tag x₂ x₃ →
-- <t2> [tag x₂ x₃ / x₁]
-- TODO update taginfo
vectorize :: forall mf. MonadFresh Int mf => TagInfo -> Term -> mf Term
vectorize tagInfo (t1 `Bind` LAltVar x1 t2)
  | Just tag <- singletonTag x1 tagInfo = do
    let
      arity = tagArity tag
      rho =
        inplaceS 0 (ConstantNode tag $ map Var $ downFrom arity) `composeS`
        raiseFromS 1 (arity - 1)
    t2' <- applySubst rho <$> vectorize tagInfo t2
    Bind t1 <$> laltConstantNode tag t2'
  | Just arity <- lookupMaxArity x1 tagInfo = do
    let rho =
          inplaceS 0 (VariableNode arity $ map Var $ downFrom arity) `composeS`
          raiseFromS 1 arity
    t2' <- applySubst rho <$> vectorize tagInfo t2
    Bind t1 <$> laltVariableNode arity t2'

vectorize absCxt (t1 `Bind` alt) =
  let (mkAlt, t2) = splitLalt alt in do
  t1' <- vectorize absCxt t1
  t2' <- vectorize absCxt t2
  pure $ t1' `Bind` mkAlt t2'

vectorize absCxt (Case v t alts) = do
  t' <- vectorize absCxt t
  alts' <- forM alts $ \alt ->
    let (mkAlt, t) = splitCalt alt in
    mkAlt <$> vectorize absCxt t
  pure $ Case v t' alts'

vectorize _ t = pure t

--
-- <t1>
-- case tag x₁ x₂ of
--   Cnil        → <t2>
--   Ccons x₃ x₄ → <t3>
-- >>>
-- <t1>
-- case tag of
--   Cnil  → <t2>
--   Ccons → <t3> [x₁ / x₃, x₂ / x₄]
simplifyCase :: Term -> Term
simplifyCase (Case v t alts)
  | ConstantNode tag vs <- v = Case (Tag tag) t' $ map (mkAlt vs) alts
  | VariableNode n vs <- v = Case (Var n) t' $ map (mkAlt vs) alts
  | otherwise =
    let go = (\(mkAlt, t) -> mkAlt $ simplifyCase t) . splitCalt in
    Case v t' $ map go alts
  where
    t' = simplifyCase t

    mkAlt vs (CAltConstantNode tag xs t)
      | arity <- tagArity tag
      , arity > 0 =
        let
          vs' = raise arity $ reverse $ take arity vs

          -- Substitute pattern variables by node variables
          rho =
            strengthenS impossible arity `composeS`
            foldr composeS IdS (zipWith inplaceS [0 .. arity - 1] vs')
         in
         CAltTag tag $ applySubst rho $ simplifyCase t
      | otherwise = CAltTag tag t
    mkAlt _ alt = alt

simplifyCase (t1 `Bind` alt) =
    simplifyCase t1 `Bind` mkAlt (simplifyCase t2)
  where
    (mkAlt, t2) = splitLalt alt

simplifyCase term = term




-- | Split fetch operations using offsets
-- <t1>
-- fetch p ; λ tag x₁ x₂ →
-- <t2>
-- >>>
-- <t1>
-- fetch p [0]; λ tag →
-- fetch p [1]; λ x₁ →
-- fetch p [2]; λ x₂ →
-- <t2>
-- TODO returning fetch [Boquist 1999, p. 105]
splitFetch :: Term -> Term
splitFetch (FetchNode n `Bind` alt)
  | LAltVariableNode x xs t <- alt =
    let mkFetch x m t = FetchOffset (n + m) m `Bind` LAltVar x t in
    FetchOffset n 0 `Bind` LAltVar x
    (mkFetchs xs (splitFetch t) mkFetch)
  | LAltConstantNode tag xs t <- alt =
    let mkFetch x m t = FetchOffset (n + m - 1) m `Bind` LAltVar x t in
    mkFetchs xs (splitFetch t) mkFetch
  where
    mkFetchs xs t f = foldr (uncurry f) t $ zip xs [1 ..]

splitFetch (t1 `Bind` alt) =
  splitFetch t1 `Bind` mkAlt (splitFetch t2)
  where
    (mkAlt, t2) = splitLalt alt

splitFetch (Case v t alts) =
    Case v (splitFetch t) $ map go alts
  where
    go = (\(mkAlt, t) -> mkAlt $ splitFetch t) . splitCalt

splitFetch term = term

-- fetch1 :: Term
-- fetch1 = FetchOffset 1 1
-- fetch2 :: Term
-- fetch2 = FetchOffset 2 2
--
-- term :: Term
-- term =
--   fetch1 `Bind` LAltVar (MkAbs $ Gid 10)
--   (fetch2 `Bind` LAltVar (MkAbs $ Gid 11)
--   (Case (Var 3) unreachable
--     [ CAltTag natTag (Unit (ConstantNode natTag [Var 1]))
--
--     ]))

-- Preconditions: Normalised
--
-- fetch p [0] ; λ tag →
-- fetch p [1] ; λ x₁ →
-- fetch p [2] ; λ x₂ →
-- <m1>
-- case tag of
--   Cnil  → <m2>
--   Ccons → <m3>
-- <m2>
--
-- >>>
--
-- fetch p [0] ; λ tag →
-- <m1>
-- case tag of
--   Cnil  → <m2>
--   Ccons → fetch p [1] ; λ y₁ →
--           fetch p [2] ; λ y₂ →
--           <m3> [y₁ / x₁, y₂ / x₂]
--

rightHoistFetch :: forall mf. MonadFresh Int mf => Term -> mf Term
rightHoistFetch term@(FetchOffset n1 0       `Bind` LAltVar x1
                     (FetchOffset n1' offset `Bind` LAltVar x2 t2))
  | n1' - 1 == n1 = do
    t3 <- hoistFetch x1 n1' x2 t2 offset
    rightHoistFetch $ FetchOffset n1 0 `Bind` LAltVar x1 t3

rightHoistFetch (Bind t1 (splitLaltWithAbss -> (mkAlt, t2, xs))) =
  Bind t1 . mkAlt <$> rightHoistFetch t2

rightHoistFetch (Case v t alts) =
   (Case v <$> rightHoistFetch t) <*> mapM go alts
  where
    go (splitCalt -> (mkAlt, t)) = mkAlt <$> rightHoistFetch t

rightHoistFetch term = pure term

hoistFetch :: forall mf. MonadFresh Int mf => Abs -> Int -> Abs -> Term -> Int -> mf Term
hoistFetch x1 n x2 t offset =
    go (x2 <| x1 :| []) t
  where
    go :: List1 Abs -> Term -> mf Term
    go xs (Case (Var n) t alts)
      | x1 `elem` (toList xs !!! n) =
        let v = strengthen impossible $ Var n in
        Case v t <$> updateAlts xs alts

    go xs (Case (Var n) t alts `Bind` alt)
      | x1 `elem` (toList xs !!! n) = do
        let v = strengthen impossible $ Var n
        (Case v t <$> updateAlts xs alts) <&> (`Bind` strengthen impossible alt)


    go (x1 :| xs) (t1 `Bind` (splitLaltWithAbss -> (mkAlt, t2, [x2]))) = do
      -- Swap indices 0 and 1
      t2' <- go (x1 <| x2 :| xs) (swap01' t2)
      pure $ strengthen impossible t1 `Bind` mkAlt t2'

    -- TODO
    go xs1 (t1 `Bind` (splitLaltWithAbss -> (mkAlt, t2, xs2))) = error "TODO need swap0n"
    go _ _ = __IMPOSSIBLE__

    updateAlts  :: List1 Abs -> [CAlt] -> mf [CAlt]
    updateAlts xs alts =
      forM alts $ \(splitCalt -> (mkAlt, t)) -> mkAlt <$>
        caseMaybe (prependFetchOnUse xs t)
          -- Strengthen alternatives which didn't prepend fetch
          (pure $ strengthen impossible t)
          id

    prependFetchOnUse :: List1 Abs -> Term -> Maybe (mf Term)
    prependFetchOnUse (x1 :| xs) (Bind t1 (splitLaltWithAbss -> (mkAlt, t2, [x2])))
      | usesX2 (x1 :| xs) t1 =
        Just $ prependFetch (x1 :| xs) (t1 `Bind` mkAlt t2)
      | otherwise =
        ifJust (prependFetchOnUse (x1 :| xs) t1)
          -- Keep t2 unchanged
          (Just . fmap (`Bind` mkAlt t2)) $

          (fmap . fmap)
             -- Strengthen t1 if fetch is prepended to t2
            (\t2' -> strengthen impossible t1 `Bind` mkAlt t2')
            -- Swap indices 0 and 1
            (prependFetchOnUse (x1 <| x2 :| xs) (swap01' t2))

    prependFetchOnUse xs1 (Bind t1 (splitLaltWithAbss -> (mkAlt, t2, xs2))) = error "TODO need swap0n"
    prependFetchOnUse xs t = boolToMaybe (usesX2 xs t) (prependFetch xs t)

    prependFetch :: List1 Abs -> Term -> mf Term
    prependFetch xs t =  FetchOffset (n + length xs - 2) offset `bindVar` t

    usesX2 :: List1 Abs -> Term -> Bool
    usesX2 xs = \case
        App v vs        -> any (usesX2Val xs) (v : vs)
        Store _ v       -> usesX2Val xs v
        Unit v          -> usesX2Val xs v
        Update _ n v    -> isX2 xs n || usesX2Val xs v
        Error _         -> False
        FetchOffset n _ -> isX2 xs n
        Bind{}          -> __IMPOSSIBLE__
        Case{}          -> __IMPOSSIBLE__
        Fetch{}         -> __IMPOSSIBLE__
      where
        usesX2Val :: List1 Abs -> Val -> Bool
        usesX2Val xs = \case
          ConstantNode _ vs -> any (usesX2Val xs) vs
          VariableNode n vs -> isX2 xs n || any (usesX2Val xs) vs
          Var n             -> isX2 xs n
          _                 -> False

        isX2 xs n = caseMaybe (toList xs !!! n) False (== x2)

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
--     let vars = take def.gr_arity $ map (VarInfo . singleton) ['a' ..]
--     local (\cxt -> cxt {vars=vars , currentDef=def}) $ defToLlvm def
--
-- defToLlvm :: GrinDefinition -> M L.Instruction
-- defToLlvm GrinDefinition{gr_name, gr_type=Just typ, gr_term, gr_arity} = do
--   (ts, t) <- bimapM (mapM typeToLlvm) typeToLlvm $ returnType typ
--   body <- termToLlvm gr_term
--
--   pure $
--     L.Define
--       L.Fastcc
--       L.Ptr
--       (L.MkVar $ prettyShow gr_name)
--       (zip (replicate gr_arity L.Ptr) $ map singleton ['a' ..])
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
--     let arity = tagr_arity tag
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
--   evalDef = GrinDefinition {gr_type=Nothing, gTreeless=Nothing, gr_term=eval, gr_name="eval", gr_arity=1}
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
--   downFromDef = GrinDefinition {gr_type=Nothing, gTreeless=Nothing, gr_term=downFrom, gr_name=downFromN, gr_arity=1}
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
--   sumDef = GrinDefinition {gr_type=Nothing, gTreeless=Nothing, gr_term=sum, gr_name=sumN, gr_arity=1}
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
--   mainDef = GrinDefinition {gr_type=Nothing, gTreeless=Nothing, gr_term=main, gr_name="main", gr_arity=0}
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

-- llvmCompileDef env menv isMainModule Defn{defName, defType, theDef=Function{}} = do
--     treeless <- maybeM __IMPOSSIBLE__ normalizeNames $ toTreeless LazyEvaluation defName
--     let (arity, treeless') = skipLambdas treeless
--
--     term' <- treelessToGrin isMain (simplifyApp treeless') arity
--     gr_args <- replicateM arity freshAbs
--     gr_return <- boolToMaybe (not isMain) <$> freshAbs
--
--     let gDef  =
--           GrinDefinition
--             { gr_type = Just defType
--             , gr_name = prettyShow defName
--             , gr_arity = arity
--             , gr_term = term'
--             , gTreeless = Just $ simplifyApp treeless
--             , gr_args = gr_args
--             , gr_return = gr_return
--             }
--     pure $ Just gDef
--   where
--     isMain = isNamedMain && isMainModule == IsMain
--     isNamedMain = "main" == (prettyShow . nameConcrete . qnameName) defName
--
-- llvmCompileDef env menv isMain def@Defn{defName, theDef=Primitive{primName}} = do
--   -- liftIO $ putStrLn $ render $ sep [text "Primitive:", pretty defName, pretty primName]
--   pure Nothing
--
-- llvmCompileDef env menv isMain def@Defn{defName, theDef=Datatype{dataCons}} = do
--   -- liftIO $ putStrLn $ render $ sep [text "Datatype: ", pretty defName, pretty dataCons]
--   pure Nothing
--
-- llvmCompileDef env menv isMain def@Defn{defName, defType, theDef=Constructor{conArity, conData}} = do
--   -- liftIO $ putStrLn $ render $ sep [text "Constructor:", pretty defName, pretty conData, pretty conArity]
--   pure Nothing
--
-- llvmCompileDef env menv isMain def = pure Nothing
--


trace' :: String -> a -> a
trace' s = unsafePerformIO . (appendFile "trace.log" (s ++ "\n") $>)
