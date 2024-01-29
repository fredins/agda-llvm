{-# LANGUAGE UndecidableInstances #-}

module Compiler.Llvm.Codegen (grinToLlvm) where

import           Control.Monad.Reader.Class (MonadReader (local))
import           Control.Monad.State.Class  (MonadState (get, state))
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Writer.Class ()


import           Control.Applicative        (liftA2)
import           Control.Arrow              (first)
import           Control.Monad              (liftM2, zipWithM, zipWithM_)
import           Control.Monad.Reader       (MonadReader, ReaderT (runReaderT),
                                             asks, runReader)
import           Control.Monad.State        (MonadState, State, StateT,
                                             evalStateT, execStateT, gets,
                                             modify, runState, runStateT)
import           Control.Monad.Writer       (MonadWriter (tell), Writer,
                                             WriterT (runWriterT), runWriter)
import           Data.Bifunctor             (bimap)
import           Data.Foldable              (foldl', foldlM)
import           Data.Functor               (($>))
import           Data.List.Extra            (list)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Prelude                    hiding ((!!))

import           Agda.Utils.Impossible      (__IMPOSSIBLE__)
import           Agda.Utils.List            (caseList, (!!))
import           Agda.Utils.List1           (List1, pattern (:|), (<|))
import qualified Agda.Utils.List1           as List1

import           Agda.Utils.Maybe           (boolToMaybe, fromMaybe)
import           Agda.Utils.Monad           ((==<<), tell1)

import           Agda.Syntax.Literal        (Literal (LitNat))
import           Compiler.Grin.Grin         (CAlt, GrinDefinition (..), LAlt,
                                             Tag, Term)
import qualified Compiler.Grin.Grin         as G
import           Compiler.Llvm.Llvm
import           Control.Monad.Identity     (Identity)
import qualified Utils.List1                as List1
import           Utils.Utils




typeOfDef :: String -> Int -> ([Type], Type)
typeOfDef "free"    1     = ([Ptr], Void)
typeOfDef "drop"    1     = ([I64], Void)
typeOfDef "dup"     1     = ([I64], Void)
typeOfDef "printf"  2     = ([Ptr, I64], Void)
typeOfDef _         arity = (replicate arity I64, nodeTySyn)



data Continuation
  = Return
  | Branch LocalId
  | Assign -- (Instruction -> Env -> (Instruction, Env))
  | AssignAndBranch (Instruction -> Env -> (Instruction, Env)) LocalId

contLocal :: MonadReader Cxt m => Continuation -> m a -> m a
contLocal cont = local \ cxt -> cxt{cxt_cont = cont}

data Cxt = MkCxt
  { cxt_vars :: [LocalId]
  , cxt_cont :: Continuation
  }

initCxt :: GrinDefinition -> Cxt
initCxt def = MkCxt
  { cxt_vars = map mkLocalId (reverse def.gr_args)
  , cxt_cont = Return
  }

varLookup :: MonadReader Cxt m => Int -> m LocalId
varLookup n = asks $ (!! n) . cxt_vars

type Instructions = [Instruction]

data Env = MkEnv
  { env_fresh_unnamed :: Int
  , env_fresh_tag     :: Int
  , env_fresh_alt     :: Int
  , env_tags          :: Map Tag Int
  }

initEnv :: Env
initEnv = MkEnv
  { env_fresh_unnamed = 1
  , env_fresh_tag     = 0
  , env_fresh_alt     = 0
  , env_tags          = mempty
  }

tagLookup :: MonadState Env m => Tag -> m Int
tagLookup tag = gets $ fromMaybe __IMPOSSIBLE__ . Map.lookup tag . env_tags

freshUnnamed :: MonadState Env m => m LocalId
freshUnnamed  = do
  unnamed <- gets $ mkLocalId . env_fresh_unnamed
  modify \ env -> env{env_fresh_unnamed = env.env_fresh_unnamed + 1}
  pure unnamed

setVar :: (MonadState Env m, MonadWriter Instructions m) => Instruction -> m LocalId
setVar instruction = do
  unnamed <- freshUnnamed
  tell1 (SetVar unnamed instruction)
  pure unnamed

type MonadCodegen m = (MonadReader Cxt m, MonadState Env m, MonadWriter Instructions m)

emitVal :: MonadCodegen m => G.Val -> m Val
emitVal (G.Var n)               = LocalId <$> varLookup n
emitVal (G.Def f)               = pure $ GlobalId (mkGlobalId f)
emitVal G.Prim{}                = __IMPOSSIBLE__
emitVal (G.Lit lit)             = pure (Lit lit)
emitVal (G.Tag tag)             = mkLit <$> tagLookup tag
emitVal (G.ConstantNode tag vs) = do
  vs' <- liftA2 (:) (mkLit <$> tagLookup tag) (mapM emitVal vs)
  foldlM step Undef $ zip vs' [1 .. length vs' + 1]
  where
  step unnamed (v, index) = LocalId <$> setVar (insertvalue unnamed v index)
emitVal (G.VariableNode n vs)   = do
  vs' <- liftA2 (:) (LocalId <$> varLookup n) (mapM emitVal vs)
  foldlM step Undef $ zip vs' [1 .. length vs' + 1]
  where
  step unnamed (v, index) = LocalId <$> setVar (insertvalue unnamed v index)
emitVal G.Empty                 = __IMPOSSIBLE__


emitCalt :: MonadCodegen m => Term -> m (String, String, Instruction)
emitCalt = undefined

emitLalt :: MonadCodegen m => Instruction -> G.LAlt -> m Instruction
emitLalt instruction (G.LAltEmpty t) = do
  tell1 instruction
  emitTerm t
emitLalt instruction (G.LAltVar x t) = do
  tell1 (SetVar (mkLocalId x) instruction)
  emitTerm t
emitLalt instruction (G.LAltConstantNode _ xs t) = do
  unnamed <- freshUnnamed
  tell1 (SetVar unnamed instruction)
  let step x i = SetVar (mkLocalId x) $ extractvalue (LocalId unnamed) i
  tell $ zipWith step xs [2 ..]
  emitTerm t
emitLalt instruction (G.LAltVariableNode x xs t) = do
  unnamed <- freshUnnamed
  tell1 (SetVar unnamed instruction)
  let step x i = SetVar (mkLocalId x) $ extractvalue (LocalId unnamed) i
  tell $ zipWith step (x : xs) [1 ..]
  emitTerm t

emitUnitNode :: MonadCodegen m => Val -> List1 Val -> m Instruction
emitUnitNode v vs = do
  let (vs', v') = first (v :|) (List1.initLast vs)
      indices = 1 :| [2 .. length vs']
      index = length vs' + 1
  unnamed <- List1.foldlM step step' $ List1.zip vs' indices
  asks cxt_cont >>= \case
    Return -> do
      unnamed' <- freshUnnamed
      tell [SetVar unnamed' $ insertvalue (LocalId unnamed) v' index]
      pure $ RetNode (LocalId unnamed')
    Branch{} -> __IMPOSSIBLE__
    Assign -> pure $ insertvalue (LocalId unnamed) v' index
    AssignAndBranch setVar label -> do
      tell1 =<< state (setVar $ insertvalue (LocalId unnamed) v' index)
      pure (Br label)
  where
  step unnamed (v, index) = setVar (Insertvalue nodeTySyn (LocalId unnamed) I64 v index)
  step' (v, index) = setVar (Insertvalue nodeTySyn Undef I64 v index)

emitApp :: MonadCodegen m => G.Val -> [G.Val] -> m Instruction
emitApp (G.Def f) vs = do
  vs' <- mapM emitVal vs
  let mkCall tail = Call tail Fastcc returnTy (mkGlobalId f) (zip argsTys vs')
  asks cxt_cont >>= \case
    Return -> pure $ mkCall (Just Tail)
    Branch label -> tell1 (mkCall Nothing) $> Br label
    Assign -> pure (mkCall Nothing)
    AssignAndBranch setVar label -> do
      tell1 =<< state (setVar $ mkCall Nothing)
      pure (Br label)
  where
  (argsTys, returnTy) = typeOfDef f (length vs)
emitApp (G.Prim prim) vs = undefined
emitApp _ _ = __IMPOSSIBLE__

emitTerm :: MonadCodegen m => Term -> m Instruction
emitTerm (G.App f vs) = emitApp f vs
emitTerm (G.Unit (G.ConstantNode tag vs)) = do
  let vs1 = caseList vs __IMPOSSIBLE__ (:|) -- FIXME ConstantNode should use List1
  emitUnitNode ==<< (mkLit <$> tagLookup tag, mapM emitVal vs1)
emitTerm (G.Unit (G.VariableNode n vs)) = do
  let vs1 = caseList vs __IMPOSSIBLE__ (:|) -- FIXME VariableNode should use List1
  emitUnitNode ==<< (LocalId <$> varLookup n, mapM emitVal vs1)
emitTerm (G.Store _ v `G.Bind` G.LAltVar x t) = do
  v' <- emitVal v
  tell [malloc nodeSize, store nodeTySyn v' (mkLocalId x)]
  emitTerm t
emitTerm (t1 `G.Bind` alt) = do
  instruction <- contLocal Assign (emitTerm t1)
  emitLalt instruction alt
emitTerm (G.FetchOpaqueOffset n offset) | offset `elem` [0, 1] = do
  x <- varLookup n
  unnamed <- setVar (inttoptr x)
  unnamed' <- setVar (getelementptr unnamed offset)
  pure (load I64 unnamed')
emitTerm (G.FetchOffset tag n offset) = do
  -- TODO need to lookup tag to get node structure
  x <- varLookup n
  unnamed <- setVar (inttoptr x)
  unnamed' <- setVar (getelementptr unnamed offset)
  pure (load I64 unnamed')
emitTerm (G.Update tag' tag n offset) = undefined
emitTerm (G.UpdateOffset n 0 v) = do
  -- TODO replace UpdateOffset by Dup and Decref
  undefined
emitTerm (G.Case v t alts) = undefined
emitTerm G.Unreachable = pure Unreachable

emitTerm G.Error{} = __IMPOSSIBLE__
emitTerm G.Unit{} = __IMPOSSIBLE__
emitTerm G.Fetch'{} = __IMPOSSIBLE__
emitTerm G.UpdateOffset{} = __IMPOSSIBLE__
emitTerm G.Store{} = __IMPOSSIBLE__


grinToLlvm :: GrinDefinition -> (Map Tag Int, Instruction)
grinToLlvm def = (tags, definition)
  where
  (argsTys, returnTy) = typeOfDef def.gr_name def.gr_arity
  args = zip argsTys (map mkLocalId def.gr_args)
  (instructions, tags) = bimap (uncurry (:|)) env_tags
                       $ flip runState initEnv
                       $ runWriterT
                       $ flip runReaderT (initCxt def)
                       $ emitTerm def.gr_term


  definition = Define Fastcc returnTy (mkGlobalId def.gr_name) args instructions



