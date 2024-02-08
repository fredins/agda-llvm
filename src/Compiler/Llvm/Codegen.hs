module Compiler.Llvm.Codegen (grinToLlvm) where

import           Control.Applicative        (liftA2)
import           Control.Monad              (replicateM, (<=<))
import           Control.Monad.Reader       (ReaderT, asks, runReaderT)
import           Control.Monad.Reader.Class (MonadReader (ask, local, reader))
import           Control.Monad.State        (MonadState, State, gets, modify,
                                             runState)
import           Control.Monad.State.Class  (state)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Writer       (WriterT, runWriterT, tell)
import           Control.Monad.Writer.Class (MonadWriter (writer), censor)
import           Data.Bifunctor             (bimap)
import           Data.Foldable              (foldlM)
import           Data.List                  (singleton)
import           Data.List.Extra            (list, snoc, zipWith4)
import qualified Data.List.NonEmpty.Extra   as List1
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Prelude                    hiding ((!!))

import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.Literal        (Literal (LitNat))
import           Agda.Utils.Function        (applyUnless)
import           Agda.Utils.Functor         (for)
import           Agda.Utils.Impossible      (__IMPOSSIBLE__)
import           Agda.Utils.List            (caseList, (!!))
import           Agda.Utils.List1           (pattern (:|))
import           Agda.Utils.Maybe           (fromJust, fromMaybe, fromMaybeM)
import           Agda.Utils.Monad           (tell1)

import           Compiler.Grin.Grin         (GrinDefinition (..), Tag, Term)
import qualified Compiler.Grin.Grin         as G
import           Compiler.Llvm.Llvm
import           Utils.Utils

typeOfDef :: String -> Int -> ([Type], Type)
typeOfDef "free"    1     = ([Ptr], Void)
typeOfDef "drop"    1     = ([I64], Void)
typeOfDef "printf"  2     = ([Ptr, I64], Void)
typeOfDef "main"    0     = ([], Void)
typeOfDef _         arity = (replicate arity I64, nodeTySyn)

type MonadCodegen m = (MonadReader Cxt m,  MonadWriter [Instruction] m, MonadState Env m)
type Codegen = ReaderT Cxt (WriterT [Instruction] (State Env))
type Continuation a = Env -> ((a, [Instruction]), Env)

data Assignment
  = AEmpty
  | AVar LocalId -- ^ Need to be lazy.
  | AConstantNode [LocalId] -- FIXME List1
  | AVariableNode LocalId [LocalId] -- FIXME List1
    deriving Show

data Finally
  = FFallthrough (Continuation ())
  | FReturn
  | FBranch LocalId
    deriving Show

-- | Embed a continuation into the monad
continuation :: Continuation a -> Codegen a
continuation = (writer <=< state <=< reader) . const

runCodegen :: Codegen a -> Cxt -> Continuation a
runCodegen f cxt env = flip runState env $ runWriterT $ runReaderT f cxt

-- FIXME name
run :: Codegen a -> Codegen (a, [Instruction])
run m = lift . lift . runWriterT . runReaderT m =<< ask

newtype Cxt = MkCxt{cxt_vars :: [LocalId]}

lookupVar :: MonadReader Cxt m => Int -> m LocalId
lookupVar n = asks $ (!! n) . cxt_vars

data Env = MkEnv
  { env_fresh_unnamed   :: !Int
  , env_fresh_tag       :: !Int
  , env_fresh_label_num :: !Int
  , env_tags            :: Map Tag Int
  , env_recent_label    :: Maybe LocalId
  }

freshUnnamed :: MonadState Env m => m LocalId
freshUnnamed = do
  unnamed <- gets $ MkLocalId . prettyShow . env_fresh_unnamed
  modify \ env -> env{env_fresh_unnamed = env.env_fresh_unnamed + 1}
  pure unnamed

freshLabelNum :: MonadState Env m => m Int
freshLabelNum = do
  alt_num <- gets env_fresh_label_num
  modify \ env -> env{env_fresh_label_num = env.env_fresh_label_num + 1}
  pure alt_num

setVar :: (MonadState Env m, MonadWriter [Instruction] m) => Instruction -> m LocalId
setVar instruction = do
  unnamed <- freshUnnamed
  tell1 (SetVar unnamed instruction)
  pure unnamed

lookupTag :: MonadState Env m => Tag -> m Int
lookupTag tag = fromMaybeM (freshTag tag) (gets $ Map.lookup tag . env_tags)
  where
  freshTag tag = do
    n <- gets env_fresh_tag
    modify \ env -> env { env_fresh_tag = env.env_fresh_tag + 1
                        , env_tags      = Map.insert tag n env.env_tags
                        }
    pure n

recentLabel :: MonadState Env m => LocalId -> m ()
recentLabel label = modify \ env -> env{env_recent_label = Just label}

emitVal :: MonadCodegen m => G.Val -> m Val
emitVal (G.Var n)               = LocalId <$> lookupVar n
emitVal (G.Def f)               = pure $ GlobalId (mkGlobalId f)
emitVal G.Prim{}                = __IMPOSSIBLE__
emitVal (G.Lit lit)             = pure (Lit lit)
emitVal (G.Tag tag)             = mkLit <$> lookupTag tag
emitVal (G.ConstantNode tag vs) = do
  vs' <- (:) <$> (mkLit <$> lookupTag tag) <*> mapM emitVal vs
  foldlM step Undef $ zip vs' [1 .. length vs' + 1]
  where
  step unnamed (v, index) = LocalId <$> setVar (insertvalue unnamed v index)
emitVal (G.VariableNode n vs)   = do
  vs' <- liftA2 (:) (LocalId <$> lookupVar n) (mapM emitVal vs)
  foldlM step Undef $ zip vs' [1 .. length vs' + 1]
  where
  step unnamed (v, index) = LocalId <$> setVar (insertvalue unnamed v index)
emitVal G.Empty                 = __IMPOSSIBLE__

--------------------------------------------------------------------------------
-- * Emitting terms
--------------------------------------------------------------------------------

mkUnnamedNode :: (MonadWriter [Instruction] m, MonadState Env m) => Val -> [Val] -> m LocalId
mkUnnamedNode v vs = do
  unnameds <- replicateM (length vs) freshUnnamed
  unnamed <- freshUnnamed
  let ins  = Undef : map LocalId unnameds
      outs = unnameds `snoc` unnamed
  tell $ zipWith4 (\ out -> SetVar out .:. insertvalue) outs ins (v : vs) [1 ..]
  pure unnamed

mkNode :: (MonadWriter [Instruction] m, MonadState Env m) => LocalId -> Val -> [Val] -> m ()
mkNode x v vs = do
  unnameds <- replicateM (length vs) freshUnnamed
  let ins  = Undef : map LocalId unnameds
      outs = unnameds `snoc` x
  tell $ zipWith4 (\ out -> SetVar out .:. insertvalue) outs ins (v : vs) [1 ..]
  pure ()

emitUnit :: Assignment -> Finally -> G.Val -> Codegen ()
emitUnit AEmpty FReturn (G.Var n) = tell1 . RetNode . LocalId =<< lookupVar n
emitUnit AEmpty FReturn G.Empty   = tell1 RetVoid
emitUnit AEmpty FReturn (G.ConstantNode tag vs) = do
  v <- mkLit <$> lookupTag tag
  vs' <- mapM emitVal vs
  x <- mkUnnamedNode v vs'
  tell1 $ RetNode (LocalId x)
emitUnit (AVar x) (FBranch label) (G.ConstantNode tag vs) = do
  v <- mkLit <$> lookupTag tag
  vs' <- mapM emitVal vs
  mkNode x v vs'
  tell1 (Br label)
emitUnit AEmpty FReturn (G.VariableNode n vs) = do
  v <- LocalId <$> lookupVar n
  vs' <- mapM emitVal vs
  x <- mkUnnamedNode v vs'
  tell1 $ RetNode (LocalId x)
emitUnit (AVar x) (FBranch label) (G.VariableNode n vs) = do
  v <- LocalId <$> lookupVar n
  vs' <- mapM emitVal vs
  mkNode x v vs'
  tell1 (Br label)
emitUnit ass fin v = error $ render $ text "emitUnit: Missing pattern" <+> pshow ass <+>  pshow fin <+> pshow v

localVars :: MonadReader Cxt m => [G.Abs] -> m a -> m a
localVars xs = local \ cxt -> cxt{cxt_vars = reverse (map (MkLocalId . prettyShow) xs) ++ cxt.cxt_vars}

localVar :: MonadReader Cxt m => G.Abs -> m a -> m a
localVar = localVars . singleton

emitBind :: Assignment -> Finally -> G.Term -> G.LAlt -> Codegen ()
emitBind ass fin t1 (G.LAltEmpty t2) = do
  fin' <- FFallthrough . runCodegen (emitTerm ass fin t2) <$> ask
  emitTerm AEmpty fin' t1
emitBind ass fin t1 (G.LAltVar x t2) = do
  fin' <- FFallthrough . runCodegen m <$> ask
  emitTerm (AVar $ MkLocalId $ prettyShow x) fin' t1
  where
  m = localVar x (emitTerm ass fin t2)
emitBind ass fin t1 (G.LAltConstantNode _ xs t2) = do
  fin' <- FFallthrough . runCodegen m <$> ask
  emitTerm ass' fin' t1
  where
  ass' = AConstantNode $ map (MkLocalId . prettyShow) xs
  m = localVars xs (emitTerm ass fin t2)
emitBind ass fin t1 (G.LAltVariableNode x xs t2) = do
  fin' <- FFallthrough . runCodegen m <$> ask
  emitTerm ass' fin' t1
  where
  ass' = AVariableNode (MkLocalId $ prettyShow x) $ map (MkLocalId . prettyShow) xs
  m = localVars (x : xs) (emitTerm ass fin t2)

-- FIXME Horrible copy paste programming. Abstract patterns once everything works.
emitCase :: Assignment -> Finally -> G.Val -> G.Term -> [G.CAlt] -> Codegen ()
emitCase AEmpty (FFallthrough c) (G.Var n) t alts = do
  -- Continue label
  continue_label_num <- freshLabelNum
  let continue_label = surroundWithQuotes $ "L" ++ show continue_label_num ++ "-continue"
  let fin = FBranch (MkLocalId continue_label)

  let
    go malt_num desc t = run do
      alt_label_num <- freshLabelNum
      let alt_first_label = MkLocalId $ surroundWithQuotes $  "L" ++ show alt_label_num ++ "-" ++ desc
      censor do singleton . Label alt_first_label.unLocalId
             do recentLabel alt_first_label
                emitTerm AEmpty fin t
      alt_recent_label <- gets $ fromMaybe alt_first_label . env_recent_label
      pure MkAltInfo
            { alt_num          = malt_num
            , alt_first_label  = alt_first_label
            , alt_recent_label = alt_recent_label
            , alt_result       = Nothing
            }

    emitCAlt (G.CAltTag tag t) = do
      malt_num <- Just <$> lookupTag tag
      let desc = prettyShow tag
      go malt_num desc t
    emitCAlt (G.CAltLit (LitNat n) t) = do
      let malt_num = Just (fromInteger n)
      let desc = show n
      go malt_num desc t
    emitCAlt _ = __IMPOSSIBLE__

  -- Alternatives
  (altInfo, instructions) <- go Nothing "default" t
  (instructions, altInfos) <-
    forAccumM instructions alts \ instructions1  alt -> do
      (altInfo, instructions2) <- emitCAlt alt
      pure (instructions1 ++ instructions2, altInfo)

  -- Switch
  scrutinee <- lookupVar n
  tell $ switch scrutinee altInfo altInfos : instructions

  -- Continuation
  censor (singleton . Label continue_label) do
    recentLabel (MkLocalId continue_label)
    continuation c
emitCase AEmpty (FBranch label) (G.Var n) t alts = do
  -- Alternatives
  (altInfo, instructions) <- go Nothing "default" t
  (instructions, altInfos) <- forAccumM instructions alts \ instructions1 alt -> do
        (altInfo, instructions2) <- emitCAlt alt
        pure (instructions1 ++ instructions2, altInfo)

  -- Switch
  scrutinee <- lookupVar n
  tell $ switch scrutinee altInfo altInfos : instructions
  where
  go malt_num desc t = run do
    alt_label_num <- freshLabelNum
    let alt_first_label = MkLocalId $ surroundWithQuotes $  "L" ++ show alt_label_num ++ "-" ++ desc
    censor do singleton . Label alt_first_label.unLocalId
           do recentLabel alt_first_label
              emitTerm AEmpty (FBranch label) t
    alt_recent_label <- gets $ fromMaybe alt_first_label . env_recent_label
    pure MkAltInfo
          { alt_num          = malt_num
          , alt_first_label  = alt_first_label
          , alt_recent_label = alt_recent_label
          , alt_result       = Nothing
          }

  emitCAlt (G.CAltTag tag t) = do
    malt_num <- Just <$> lookupTag tag
    let desc = prettyShow tag
    go malt_num desc t
  emitCAlt (G.CAltLit (LitNat n) t) = do
    let malt_num = Just (fromInteger n)
    let desc = show n
    go malt_num desc t
  emitCAlt _ = __IMPOSSIBLE__
emitCase (AVar x) (FFallthrough c) (G.Var n) t alts = do
  -- Continue label
  continue_label_num <- freshLabelNum
  let continue_label = surroundWithQuotes $  "L" ++ show continue_label_num ++ "-continue"
  let fin = FBranch (MkLocalId continue_label)

  let
    go malt_num desc t = run do
      alt_label_num <- freshLabelNum
      let alt_first_label = MkLocalId $ surroundWithQuotes $  "L" ++ show alt_label_num ++ "-" ++ desc
      let alt_result = MkLocalId $ surroundWithQuotes $ "L" ++ show alt_label_num ++ "-result-" ++ desc
      censor do singleton . Label alt_first_label.unLocalId
             do recentLabel alt_first_label
                emitTerm (AVar alt_result) fin t
      alt_recent_label <- gets $ fromMaybe alt_first_label . env_recent_label
      pure MkAltInfo
            { alt_num          = malt_num
            , alt_first_label  = alt_first_label
            , alt_recent_label = alt_recent_label
            , alt_result       = Just alt_result
            }

    emitCAlt (G.CAltTag tag t) = do
      malt_num <- Just <$> lookupTag tag
      let desc = prettyShow tag
      go malt_num desc t
    emitCAlt (G.CAltLit (LitNat n) t) = do
      let malt_num = Just (fromInteger n)
      let desc = show n
      go malt_num desc t
    emitCAlt _ = __IMPOSSIBLE__

  -- Alternatives
  (altInfo, instructions) <- go Nothing "default" t
  (instructions, altInfos) <- forAccumM instructions alts \ instructions1 alt -> do
    (altInfo, instructions2) <- emitCAlt alt
    pure (instructions1 ++ instructions2, altInfo)

  -- Switch
  scrutinee <- lookupVar n
  tell $ switch scrutinee altInfo altInfos : instructions

  -- Continuation
  censor (singleton . Label continue_label) do
    recentLabel (MkLocalId continue_label)
    -- Phi node
    let altInfos' = list __IMPOSSIBLE__ (:|) $ applyUnless (t == G.Unreachable) (altInfo :) altInfos
    let pairs = for altInfos' \ altInfo -> (fromMaybe __IMPOSSIBLE__ altInfo.alt_result, altInfo.alt_recent_label)
    tell1 $ SetVar x (phi nodeTySyn pairs)
    continuation c
emitCase (AVar x) (FBranch label) (G.Var n) t alts = do
  -- Continue label
  continue_label_num <- freshLabelNum
  let continue_label = surroundWithQuotes $  "L" ++ show continue_label_num ++ "-continue"
  let fin = FBranch (MkLocalId continue_label)

  let
    go malt_num desc t = run do
      alt_label_num <- freshLabelNum
      let alt_first_label = MkLocalId $ surroundWithQuotes $  "L" ++ show alt_label_num ++ "-" ++ desc
      let alt_result = MkLocalId $ surroundWithQuotes $ "L" ++ show alt_label_num ++ "-result-" ++ desc
      censor do singleton . Label alt_first_label.unLocalId
             do recentLabel alt_first_label
                emitTerm (AVar alt_result) fin t
      alt_recent_label <- gets $ fromMaybe alt_first_label . env_recent_label
      pure MkAltInfo
            { alt_num          = malt_num
            , alt_first_label  = alt_first_label
            , alt_recent_label = alt_recent_label
            , alt_result       = Just alt_result
            }

    emitCAlt (G.CAltTag tag t) = do
      malt_num <- Just <$> lookupTag tag
      let desc = prettyShow tag
      go malt_num desc t
    emitCAlt (G.CAltLit (LitNat n) t) = do
      let malt_num = Just (fromInteger n)
      let desc = show n
      go malt_num desc t
    emitCAlt _ = __IMPOSSIBLE__

  -- Alternatives
  (altInfo, instructions) <- go Nothing "default" t
  (instructions, altInfos) <- forAccumM instructions alts \ instructions1 alt -> do
    (altInfo, instructions2) <- emitCAlt alt
    pure (instructions1 ++ instructions2, altInfo)

  -- Switch
  scrutinee <- lookupVar n
  tell $ switch scrutinee altInfo altInfos : instructions

  -- Continuation
  censor (singleton . Label continue_label) do
    recentLabel (MkLocalId continue_label)
    -- Phi node
    let altInfos' = list __IMPOSSIBLE__ (:|) $ applyUnless (t == G.Unreachable) (altInfo :) altInfos
    let pairs = for altInfos' \ altInfo -> (fromMaybe __IMPOSSIBLE__ altInfo.alt_result, altInfo.alt_recent_label)
    tell [SetVar x (phi nodeTySyn pairs), Br label]
emitCase (AConstantNode xs) (FFallthrough c) (G.Var n) t alts = do
  -- Continue label
  contine_label_num <- freshLabelNum
  let continue_label = surroundWithQuotes $  "L" ++ show contine_label_num ++ "-continue"
  let fin = FBranch (MkLocalId continue_label)

  let
    go malt_num desc t = run do
      label_num <- freshLabelNum
      let alt_first_label = MkLocalId $ surroundWithQuotes $  "L" ++ show label_num ++ "-" ++ desc
      let alt_result = MkLocalId $ surroundWithQuotes $ "L" ++ show label_num ++ "-result-" ++ desc
      censor do singleton . Label alt_first_label.unLocalId
             do recentLabel alt_first_label
                emitTerm (AVar alt_result) fin t
      alt_recent_label <- gets $ fromMaybe alt_first_label . env_recent_label
      pure MkAltInfo
            { alt_num          = malt_num
            , alt_first_label  = alt_first_label
            , alt_recent_label = alt_recent_label
            , alt_result       = Just alt_result
            }

    emitCAlt (G.CAltTag tag t) = do
      malt_num <- Just <$> lookupTag tag
      let desc = prettyShow tag
      go malt_num desc t
    emitCAlt (G.CAltLit (LitNat n) t) = do
      let malt_num = Just (fromInteger n)
      let desc = show n
      go malt_num desc t
    emitCAlt _ = __IMPOSSIBLE__

  -- Alternatives
  (altInfo, instructions) <- go Nothing "default" t
  (instructions, altInfos) <- forAccumM instructions alts \ instructions1 alt -> do
    (altInfo, instructions2) <- emitCAlt alt
    pure (instructions1 ++ instructions2, altInfo)

  -- Switch
  scrutinee <- lookupVar n
  tell $ switch scrutinee altInfo altInfos : instructions

  -- Continuation
  censor (singleton . Label continue_label) do
    recentLabel (MkLocalId continue_label)
    -- Phi node
    unnamed <- freshUnnamed
    let altInfos' = list __IMPOSSIBLE__ (:|) $ applyUnless (t == G.Unreachable) (altInfo :) altInfos
    let pairs = for altInfos' \ altInfo -> (fromMaybe __IMPOSSIBLE__ altInfo.alt_result, altInfo.alt_recent_label)
    tell $ SetVar unnamed (phi nodeTySyn pairs)
         : zipWith (\ x -> SetVar x . extractvalue unnamed) xs [2 ..]
    continuation c
emitCase (AVariableNode x xs) (FFallthrough c) (G.Var n) t alts = do
  -- Continue label
  contine_label_num <- freshLabelNum
  let continue_label = surroundWithQuotes $  "L" ++ show contine_label_num ++ "-continue"
  let fin = FBranch (MkLocalId continue_label)

  let
    go malt_num desc t = run do
      label_num <- freshLabelNum
      let alt_first_label = MkLocalId $ surroundWithQuotes $  "L" ++ show label_num ++ "-" ++ desc
      let alt_result = MkLocalId $ surroundWithQuotes $ "L" ++ show label_num ++ "-result-" ++ desc
      censor do singleton . Label alt_first_label.unLocalId
             do recentLabel alt_first_label
                emitTerm (AVar alt_result) fin t
      alt_recent_label <- gets $ fromMaybe alt_first_label . env_recent_label
      pure MkAltInfo
            { alt_num          = malt_num
            , alt_first_label  = alt_first_label
            , alt_recent_label = alt_recent_label
            , alt_result       = Just alt_result
            }

    emitCAlt (G.CAltTag tag t) = do
      malt_num <- Just <$> lookupTag tag
      let desc = prettyShow tag
      go malt_num desc t
    emitCAlt (G.CAltLit (LitNat n) t) = do
      let malt_num = Just (fromInteger n)
      let desc = show n
      go malt_num desc t
    emitCAlt _ = __IMPOSSIBLE__

  -- Alternatives
  (altInfo, instructions) <- go Nothing "default" t
  (instructions, altInfos) <- forAccumM instructions alts \ instructions1 alt -> do
    (altInfo, instructions2) <- emitCAlt alt
    pure (instructions1 ++ instructions2, altInfo)

  -- Switch
  scrutinee <- lookupVar n
  tell $ switch scrutinee altInfo altInfos : instructions

  -- Continuation
  censor (singleton . Label continue_label) do
    recentLabel (MkLocalId continue_label)
    -- Phi node
    unnamed <- freshUnnamed
    let altInfos' = list __IMPOSSIBLE__ (:|) $ applyUnless (t == G.Unreachable) (altInfo :) altInfos
    let pairs = for altInfos' \ altInfo -> (fromMaybe __IMPOSSIBLE__ altInfo.alt_result, altInfo.alt_recent_label)
    tell $ SetVar unnamed (phi nodeTySyn pairs)
         : zipWith (\ x -> SetVar x . extractvalue unnamed) (x : xs) [1 ..]
    continuation c
emitCase ass fin v t _ = error $ render $ text "emitCase: Missing pattern" <+> pshow ass <+>  pshow fin <+> pshow v <+> pshow t

mkCall :: Maybe Tail -> String -> [G.Val] -> Codegen Instruction
mkCall tail "free" [G.Var n] = do
  x <- lookupVar n
  unnamed <- freshUnnamed
  tell1 $ SetVar unnamed (inttoptr x)
  pure $ Call tail Fastcc Void (mkGlobalId "free") [(Ptr, LocalId unnamed)]
mkCall tail f vs = do
  vs' <- mapM emitVal vs
  pure $ Call tail Fastcc returnTy (mkGlobalId $ surroundWithQuotes f) (zip argsTys vs')
  where
  (argsTys, returnTy) = typeOfDef f (length vs)

emitApp :: Assignment -> Finally -> G.Val -> [G.Val] -> Codegen ()
emitApp AEmpty FReturn (G.Def "printf") [v] = do
  v' <- emitVal v
  tell [ Call Nothing Fastcc Void (mkGlobalId "printf") [(Ptr, fmt), (I64, v')]
       , RetVoid
       ]
  where
  fmt = GlobalId (mkGlobalId "\"%d\"")
emitApp AEmpty (FFallthrough c) (G.Def f) vs = do
  tell1 =<< mkCall Nothing f vs
  continuation c
emitApp AEmpty FReturn (G.Def f) vs =
  tell1 =<< mkCall (Just Tail) f vs
emitApp AEmpty (FBranch label) (G.Def f) vs = do
  call <- mkCall Nothing f vs
  tell [call, Br label]
emitApp (AVar x) (FBranch label) (G.Def f) vs = do
  call <- mkCall Nothing f vs
  tell [SetVar x call, Br label]
emitApp (AConstantNode xs) (FFallthrough c) (G.Def f) vs = do
  unnamed <- freshUnnamed
  call <- mkCall Nothing f vs
  tell $ SetVar unnamed call : zipWith (\ x -> SetVar x . extractvalue unnamed) xs [2 ..]
  continuation c
emitApp (AVariableNode x xs) (FFallthrough c) (G.Def f) vs = do
  unnamed <- freshUnnamed
  call <- mkCall Nothing f vs
  tell $ SetVar unnamed call : zipWith (\ x -> SetVar x . extractvalue unnamed) (x : xs) [1 ..]
  continuation c
emitApp (AVar x) (FFallthrough c) (G.Prim prim) [v1, v2] =do
  operation <- operand <$> emitVal v1 <*> emitVal v2
  tell1 (SetVar x operation)
  continuation c
  where
  operand = case prim of
              G.PAdd -> add64
              G.PSub -> sub64
              _TODO  -> __IMPOSSIBLE__
emitApp ass fin f v = error $ render $ text "emitApp: Missing pattern" <+> pshow ass <+>  pshow fin <+> pshow f <+> pshow v

emitFetchOpaqueOffset :: Assignment -> Finally -> Int -> Int -> Codegen ()
emitFetchOpaqueOffset (AVar x) (FFallthrough c) n offset | offset `elem` [0, 1] = do
  unnamed <- setVar . inttoptr =<< lookupVar n
  unnamed' <- setVar (getelementptr unnamed offset)
  tell1 (SetVar x $ load I64 unnamed')
  continuation c
emitFetchOpaqueOffset ass fin n offset = error $ render $ text "emitFetchOpaqueOffset: Missing pattern" <+> pshow ass <+>  pshow fin <+> pshow n <+> pshow offset

-- TODO need to lookup tag to get node structure
emitFetchOffset :: Assignment -> Finally -> Tag -> Int -> Int -> Codegen ()
emitFetchOffset (AVar x) (FFallthrough c) _ n offset = do
  unnamed <- setVar . inttoptr =<< lookupVar n
  unnamed' <- setVar (getelementptr unnamed offset)
  tell1 (SetVar x $ load I64 unnamed')
  continuation c
emitFetchOffset ass fin tag n offset = error $ render $ text "emitFetchOpaqueOffset: Missing pattern" <+> pshow ass <+>  pshow fin <+> pretty tag <+> pshow n <+> pshow offset

-- TODO need to lookup tag to get node structure
emitUpdate' :: Int -> G.Tag -> [G.Val] ->Codegen ()
emitUpdate' n tag vs = do 
  x <- lookupVar n
  x_ptr <- setVar (inttoptr x)
  rc_ptr <- setVar (getelementptr x_ptr 0)
  rc <- setVar (load I64 rc_ptr)
  v <- mkLit <$> lookupTag tag
  vs' <- mapM emitVal vs
  node <- mkUnnamedNode v vs'
  node' <- setVar (insertvalue (LocalId node) (LocalId rc) 0)
  tell1 (store nodeTySyn (LocalId node') x_ptr)

emitUpdate :: Assignment -> Finally -> Tag -> Tag -> Int -> G.Val -> Codegen ()
emitUpdate AEmpty (FBranch label) _ _ n (G.ConstantNode tag vs) = do
  emitUpdate' n tag vs
  tell1 (Br label)
emitUpdate AEmpty (FFallthrough c) _ _ n (G.ConstantNode tag vs) = do
  emitUpdate' n tag vs
  continuation c
emitUpdate ass fin tag' tag n v = error $ render $ text "emitUpdate: Missing pattern" <+> pshow ass <+>  pshow fin <+> pretty tag' <+> pretty tag <+> pshow n <+> pshow v

emitStore :: Assignment -> Finally -> G.Val -> Codegen ()
emitStore (AVar x) (FFallthrough c) (G.ConstantNode tag vs) = do
  v <- mkLit <$> lookupTag tag
  vs' <- mapM emitVal vs
  node <- mkUnnamedNode v vs'
  node' <- setVar $ insertvalue (LocalId node) (mkLit 1) 0
  ptr <- setVar (malloc nodeSize)
  tell [ store nodeTySyn (LocalId node') ptr
       , SetVar x (ptrtoint ptr)
       ]
  continuation c
emitStore ass fin v = error $ render $ text "emitStore: Missing pattern" <+> pshow ass <+>  pshow fin <+> pshow v

emitDup :: Assignment -> Finally -> Int -> Codegen ()
emitDup AEmpty (FFallthrough c) n = do
  x <- lookupVar n
  x_ptr <- setVar (inttoptr x)
  rc_ptr <- setVar (getelementptr x_ptr 0)
  rc <- setVar (load I64 rc_ptr)
  rc' <- setVar $ add64 (LocalId rc) (mkLit 1)
  tell1 (store I64 (LocalId rc') rc_ptr)
  continuation c
emitDup ass fin v = error $ render $ text "emitDup: Missing pattern" <+> pshow ass <+>  pshow fin <+> pshow v

emitDecref :: Assignment -> Finally -> Int -> Codegen ()
emitDecref AEmpty FReturn n = do
  x <- lookupVar n
  x_ptr <- setVar (inttoptr x)
  rc_ptr <- setVar (getelementptr x_ptr 0)
  rc <- setVar (load I64 rc_ptr)
  rc' <- setVar $ sub64 (LocalId rc) (mkLit 1)
  tell [store I64 (LocalId rc') rc_ptr, RetVoid]
emitDecref AEmpty (FBranch label) n = do
  x <- lookupVar n
  x_ptr <- setVar (inttoptr x)
  rc_ptr <- setVar (getelementptr x_ptr 0)
  rc <- setVar (load I64 rc_ptr)
  rc' <- setVar $ sub64 (LocalId rc) (mkLit 1)
  tell [store I64 (LocalId rc') rc_ptr, Br label]
emitDecref ass fin v = error $ render $ text "emitDecref: Missing pattern" <+> pshow ass <+>  pshow fin <+> pshow v

emitTerm :: Assignment -> Finally -> Term -> Codegen ()
emitTerm ass fin (t1 `G.Bind` alt)              = emitBind ass fin t1 alt
emitTerm ass fin (G.Case v t alts)              = emitCase ass fin v t alts
emitTerm ass fin (G.App f vs)                   = emitApp ass fin f vs
emitTerm ass fin (G.Unit v)                     = emitUnit ass fin v
emitTerm ass fin (G.FetchOpaqueOffset n offset) = emitFetchOpaqueOffset ass fin n offset
emitTerm ass fin (G.FetchOffset tag n offset)   = emitFetchOffset ass fin tag n offset
emitTerm ass fin (G.Update tag' tag n v)        = emitUpdate ass fin tag' tag n v
emitTerm ass fin (G.Store _ v)                  = emitStore ass fin v
emitTerm ass fin (G.Dup n)                      = emitDup ass fin n
emitTerm ass fin (G.Decref n)                   = emitDecref ass fin n
emitTerm _   _   G.Unreachable                  = tell1 Unreachable
emitTerm _   _   G.Error{}                      = __IMPOSSIBLE__
emitTerm _   _   G.Fetch'{}                     = __IMPOSSIBLE__

--------------------------------------------------------------------------------
-- * Interface
--------------------------------------------------------------------------------

initCxt :: GrinDefinition -> Cxt
initCxt def = MkCxt {cxt_vars = map (MkLocalId . prettyShow) (reverse def.gr_args)}

initEnv :: Map Tag Int -> Env
initEnv tags = MkEnv
  { env_fresh_unnamed   = 1
  , env_fresh_tag       = caseList (Map.elems tags) 0  $ (+1) . List1.maximum1 .: (:|)
  , env_fresh_label_num = 0
  , env_tags            = tags
  , env_recent_label    = Nothing
  }

grinToLlvm :: Map Tag Int -> GrinDefinition -> (Map Tag Int, Instruction)
grinToLlvm tags def = (tags', definition)
  where
  (argsTys, returnTy) = typeOfDef def.gr_name def.gr_arity
  args = zip argsTys (map (MkLocalId . prettyShow) def.gr_args)

  (instructions, tags') = bimap snd env_tags
                        $ runCodegen do emitTerm AEmpty FReturn def.gr_term
                                     do initCxt def
                                     do initEnv tags

  globalId = mkGlobalId $ applyUnless def.gr_isMain surroundWithQuotes def.gr_name
  definition = Define Fastcc returnTy globalId args instructions
