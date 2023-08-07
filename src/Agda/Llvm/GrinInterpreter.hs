{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Agda.Llvm.GrinInterpreter (module Agda.Llvm.GrinInterpreter) where

import           Control.Monad             ((<=<))
import           Control.Monad.Reader      (MonadReader, Reader,
                                            ReaderT (runReaderT), local,
                                            runReader)
import           Control.Monad.State       (MonadState, StateT, evalStateT,
                                            gets, modify)
import           Data.Foldable             (find)
import           Data.List                 (intercalate, singleton)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           GHC.IO                    (unsafePerformIO)

import           Agda.Compiler.Backend     hiding (Prim, initEnv)
import           Agda.Llvm.Grin
import           Agda.Syntax.Literal       (Literal (LitNat))
import           Agda.Utils.Functor
import           Agda.Utils.Impossible
import           Agda.Utils.Lens
import           Agda.Utils.List
-- import           Agda.Utils.List1                   (List1, pattern (:|), (<|))
-- import qualified Agda.Utils.List1                   as List1
import           Agda.Syntax.Common.Pretty
import           Agda.Utils.Maybe


data Value = BasNat Integer
           | VTag Tag
           | VNode Tag [Value]
           | Loc Loc
           | VEmpty
           | Undefined
             deriving (Show)

newtype Heap = Heap{unHeap :: Map Loc Value}

newtype StackFrame = StackFrame{unStackFrame :: [(Abs, Value)]}

type Stack = [StackFrame]

newtype Env = Env
  { heap     :: Heap
  }

initEnv :: Env
initEnv = Env {heap = Heap mempty}

lensHeap :: Lens' Env (Map Loc Value)
lensHeap f env = f (unHeap env.heap) <&> \heap -> env{heap = Heap heap}

lensStackFrame :: Lens' Stack [(Abs,Value)]
lensStackFrame f stack =
  f (unStackFrame $ headWithDefault __IMPOSSIBLE__ stack) <&>
  \sf -> StackFrame sf : tail stack


type Eval m = StateT Env (ReaderT Stack m)

interpretGrin :: forall mf. MonadFresh Int mf => [GrinDefinition] -> mf Value
interpretGrin defs =
    runReaderT (evalStateT (eval main.gr_term) initEnv) [StackFrame []]
  where
    main = fromMaybe __IMPOSSIBLE__ $ find gr_isMain defs

    eval :: Term -> Eval mf Value
    eval (App (Def "printf") [t]) = evalVal t

    -- Use an new (unused) memory location each time
    eval (Store _ t1 `Bind` AltVar abs t2) = do
      v <- evalVal t1 <&> \case
             BasNat i -> cnat i
             vn       -> vn
      loc <- heapInsert v
      stackFrameLocal abs (Loc loc) $ eval t2

    eval (Fetch n `Bind` AltVar abs t) = do
      heap <- use lensHeap
      v <- fromMaybeM (error $ "here is heap:\n" ++ prettyShow heap) . heapLookup =<<
           fromMaybeM __IMPOSSIBLE__ (stackFrameLookupLoc n)
      stackFrameLocal abs v $ eval t

    eval (Case t1 t2 alts `Bind` AltVar abs t3) = do
      v <- evalCase t1 t2 alts
      stackFrameLocal abs v $ eval t3

    eval (App t1 ts `Bind` AltVar abs t2) = do
      v <- evalApp t1 ts
      stackFrameLocal abs v $ eval t2

    eval (App t1 ts `Bind` AltConstantNode tag1 abss t2) =
      evalApp t1 ts >>= \case
        VNode tag2 vs | tag2 == tag1 ->
          stackFrameLocals abss vs $ eval t2
        _ -> __IMPOSSIBLE__

    eval (Update _ n t1 `BindEmpty` t2) = do
      loc <- fromMaybeM __IMPOSSIBLE__ $ stackFrameLookupLoc n
      evalVal t1 >>= \v -> case v of
        VNode{} -> do
          heapUpdate loc v
          eval t2
        _ -> __IMPOSSIBLE__

    eval (Unit t1 `Bind` AltVar abs t2) =
      evalVal t1 >>= \v ->
        stackFrameLocal abs v $
        eval t2

    eval (Unit v1 `Bind` AltConstantNode tag1 abss t2) = do
      evalVal v1 >>= \case
        VNode tag2 vs | tag2 == tag1 -> stackFrameLocals abss vs $ eval t2
        _                            -> __IMPOSSIBLE__

    eval (t1 `Bind` AltVar abs t2) = do
      v <- eval t1
      stackFrameLocal abs v $ eval t2

    eval (t1 `Bind` AltConstantNode tag1 abss t2) =
      eval t1 >>= \case
        VNode tag2 vs  | tag2 == tag1 -> stackFrameLocals abss vs $ eval t2
        _                             -> __IMPOSSIBLE__

    eval (App t ts) = evalApp t ts
    eval (Case v t alts) = evalCase v t alts
    eval (Unit v) = evalVal v




    eval Store{} = __IMPOSSIBLE__
    eval Fetch{} = __IMPOSSIBLE__
    eval Update{} = __IMPOSSIBLE__
    eval Error{} = __IMPOSSIBLE__
    eval t@Bind{} = error $ "MISSING: " ++ show t

    evalCase :: Val -> Term -> [Alt] -> Eval mf Value
    evalCase v1 t2 alts = do
      v <- evalVal v1
      case v of
        VNode _ vs ->
          let (abss, t3) = selAlt v alts t2 in
          stackFrameLocals abss vs $
          eval t3
        BasNat _ ->
          let t3 = snd $ selAlt v alts t2 in
          eval t3
        _ -> __IMPOSSIBLE__

    evalApp :: Val -> [Val] -> Eval mf Value
    evalApp v1 vs
      | Prim prim <- v1   =
        let
          evalNat = \case
              BasNat i -> pure i
              VNode tag [BasNat i] | tag == natTag -> pure i
              Loc loc  -> evalNat . fromMaybe __IMPOSSIBLE__ =<< heapLookup loc
              _        -> __IMPOSSIBLE__ in
        BasNat . runPrim prim <$> mapM (evalNat <=< evalVal) vs

      | Def name <- v1 =
        let (abss, t2) = fromMaybe (error $ "can't find " ++ name ++ "\n" ++ prettyShow (App v1 vs)) $ getGlobal name in
        mapM evalVal vs >>= \vs ->
          stackCons (StackFrame $ zip abss vs) $
          eval t2
      | otherwise = __IMPOSSIBLE__


    evalVal :: Val -> Eval mf Value
    evalVal (ConstantNode tag vs) = VNode tag <$> mapM evalVal vs
    evalVal (VariableNode n vs) = do
      stackFrameLookup n >>= \mv -> case fromMaybe __IMPOSSIBLE__ mv of
        VTag tag -> VNode tag <$> mapM evalVal vs
        _        -> __IMPOSSIBLE__
    evalVal (Var n) = fromMaybeM __IMPOSSIBLE__ $ stackFrameLookup n
    evalVal Empty = pure VEmpty
    evalVal (Lit (LitNat i)) = pure $ BasNat i
    evalVal Lit{} = __IMPOSSIBLE__
    evalVal Prim{} = __IMPOSSIBLE__
    evalVal Def{} = __IMPOSSIBLE__
    evalVal Tag{} = __IMPOSSIBLE__

    -- Not used right now (Don't remove)
    -- sel :: Int -> Value -> Value
    -- sel 0 (VNode tag _) = Tag tag
    -- sel i (VNode _ vs) | Just v <- vs !!! i = v
    -- sel _ _ = __IMPOSSIBLE__

    selAlt :: Value -> [Alt] -> Term -> ([Abs], Term)
    selAlt v alts t
      | VNode tag _ <- v = go tag
      | VTag tag <- v = go tag
      where
        go tag1 =
          headWithDefault ([], t) $
          forMaybe alts $ \case
            AltConstantNode tag2 abss t -> boolToMaybe (tag2 == tag1) (abss, t)
            t                   -> error $ "ALT " ++ show t ++ "  v:  " ++ show v

    selAlt (BasNat n1) alts t =
      ([], ) $ headWithDefault t $
      forMaybe alts $ \case
        AltLit lit t
          | LitNat n2 <- lit
          , n2 == n1 -> Just t
          | otherwise -> Nothing
        _                   -> __IMPOSSIBLE__

    selAlt _ _ _ = __IMPOSSIBLE__

    runPrim :: TPrim -> [Integer] -> Integer
    runPrim PAdd [n1, n2] = n1 + n2
    runPrim PSub [n1, n2] = n1 - n2
    runPrim p vs          = error $ "TODO " ++ show p ++ " " ++ show vs

    getGlobal :: String -> Maybe ([Abs], Term)
    getGlobal name =
      find (\def -> def.gr_name == name) defs <&> \def ->
        (def.gr_args, def.gr_term)

    stackCons :: MonadReader Stack m => StackFrame -> m a -> m a
    stackCons sf = local (sf :)

    stackFrameLookup :: MonadReader Stack m => Int -> m (Maybe Value)
    stackFrameLookup n = do
      abs <- deBruijnLookup n
      lookup abs <$> view lensStackFrame

    stackFrameLookupLoc :: MonadReader Stack m => Int -> m (Maybe Loc)
    stackFrameLookupLoc n =
      for (stackFrameLookup n) $ fmap $ \case
        Loc loc -> loc
        _       -> __IMPOSSIBLE__

    stackFrameLocal :: MonadReader Stack m => Abs -> Value -> m a -> m a
    stackFrameLocal abs v = locally lensStackFrame ((abs, v) :)

    stackFrameLocals :: MonadReader Stack m => [Abs] -> [Value] -> m a -> m a
    stackFrameLocals abss vs = foldl (.) id $ zipWith stackFrameLocal abss vs

    heapLookup :: MonadState Env m => Loc -> m (Maybe Value)
    heapLookup loc = Map.lookup loc <$> use lensHeap

    heapInsert :: (MonadFresh Int m, MonadState Env m) => Value -> m Loc
    heapInsert v = do
      loc <- freshLoc
      lensHeap %= Map.insert loc v
      pure loc

    heapUpdate :: MonadState Env m => Loc -> Value -> m ()
    heapUpdate loc v = lensHeap %= Map.insert loc v

    deBruijnLookup :: MonadReader Stack m => Int -> m Abs
    deBruijnLookup n =
      fst . fromMaybe __IMPOSSIBLE__ . (!!! n) <$> view lensStackFrame

trace' :: String -> a -> a
trace' s = unsafePerformIO . (putStrLn s $>)

natTag :: Tag
natTag = CTag{tCon = "nat" , tArity = 1}

cnat :: Integer -> Value
cnat = VNode natTag . singleton  . BasNat

-- Instances

instance Pretty Heap where
  pretty (Heap heap) =
      vcat $ map prettyEntry $ Map.toList heap
    where
      prettyEntry :: (Loc, Value) -> Doc
      prettyEntry (x, v) =
            pretty x
        <+> text "→"
        <+> pretty v

instance Pretty StackFrame where
  pretty (StackFrame stackFrame) =
      vcat $ map prettyEntry stackFrame
    where
      prettyEntry :: (Abs, Value) -> Doc
      prettyEntry (x, v) =
            pretty x
        <+> text "→"
        <+> pretty v

instance Pretty Value where
  pretty (BasNat n) = pretty n
  pretty (VTag tag) = pretty tag
  pretty (VNode tag vs) =
    pretty tag <+> text ("[" ++ intercalate ", " (map prettyShow vs) ++ "]")
  pretty VEmpty = text "()"
  pretty Undefined = text "⊥"
  pretty (Loc loc) = pretty loc
