{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Agda.Llvm.GrinInterpreter (module Agda.Llvm.GrinInterpreter) where

import           Control.Monad             ((<=<))
import           Control.Monad.Reader      (MonadReader, Reader, local,
                                            runReader)
import           Control.Monad.State       (MonadState, StateT, evalStateT,
                                            gets, modify)
import           Data.Foldable             (find)
import           Data.List                 (intercalate, singleton)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           GHC.IO                    (unsafePerformIO)

import           Agda.Compiler.Backend     hiding (Prim, initEnv)
import           Agda.Llvm.Grin            hiding (freshLoc)
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
           | Tag Tag
           | VNode Tag [Value]
           | Loc Loc
           | VEmpty
           | Undefined
             deriving (Show)

newtype Heap = Heap{unHeap :: Map Loc Value}

newtype StackFrame = StackFrame{unStackFrame :: [(Abs, Value)]}

type Stack = [StackFrame]

data Env = Env
  { heap     :: Heap
  , freshLoc :: Loc
  }

initEnv :: Env
initEnv = Env {heap = Heap mempty, freshLoc = MkLoc $ Gid 0}

lensHeap :: Lens' Env (Map Loc Value)
lensHeap f env = f (unHeap env.heap) <&> \heap -> env{heap = Heap heap}

lensStackFrame :: Lens' Stack [(Abs,Value)]
lensStackFrame f stack =
  f (unStackFrame $ headWithDefault __IMPOSSIBLE__ stack) <&>
  \sf -> StackFrame sf : tail stack

newtype Eval a = Eval{runEval :: StateT Env (Reader Stack) a}
  deriving (Functor, Applicative, Monad, MonadState Env, MonadReader Stack)

interpretGrin :: [GrinDefinition] -> Value
interpretGrin defs =
    runReader (evalStateT (runEval $ eval main.gr_term) initEnv) [StackFrame []]
  where
    main = fromMaybe __IMPOSSIBLE__ $ find gr_isMain defs

    eval :: Term -> Eval Value
    eval (App (Def "printf") [t]) = eval t

    -- Use an new (unused) memory location each time
    eval (Store _ t1 `Bind` AltVar abs t2) = do
      v <- eval t1 <&> \case
             BasNat i -> cnat i
             vn       -> vn
      loc <- heapInsert v
      stackFrameLocal abs (Loc loc) $ eval t2

    eval (Fetch n `Bind` AltVar abs t) = do
      heap <- use lensHeap
      v <- fromMaybeM (error $ "here is heap:\n" ++ prettyShow heap) . heapLookup =<<
           fromMaybeM __IMPOSSIBLE__ (stackFrameLookupLoc n)
      stackFrameLocal abs v $ eval t

    eval (Case n t1 alts `Bind` AltVar abs t2) = do
      v <- evalCase n t1 alts
      stackFrameLocal abs v $ eval t2

    eval (App t1 ts `Bind` AltVar abs t2) = do
      v <- evalApp t1 ts
      stackFrameLocal abs v $ eval t2

    eval (App t1 ts `Bind` AltNode tag1 abss t2) =
      evalApp t1 ts >>= \case
        VNode tag2 vs | tag2 == tag1 ->
          stackFrameLocals abss vs $ eval t2
        _ -> __IMPOSSIBLE__

    eval (Update _ n1 n2 `BindEmpty` t) = do
      loc <- fromMaybeM __IMPOSSIBLE__ $ stackFrameLookupLoc n1
      v <- fromMaybeM __IMPOSSIBLE__ $ stackFrameLookup n2
      case v of
        VNode{} -> do
          heapUpdate loc v
          eval t
        _ -> __IMPOSSIBLE__

    eval (Unit t1 `Bind` AltVar abs t2) =
      eval t1 >>= \v ->
        stackFrameLocal abs v $
        eval t2

    eval (Unit t1 `Bind` AltNode tag1 abss t2) = do
      eval t1 >>= \case
        VNode tag2 vs | tag2 == tag1 -> stackFrameLocals abss vs $ eval t2
        _                            -> __IMPOSSIBLE__

    eval (t1 `Bind` AltVar abs t2) = do
      v <- eval t1
      stackFrameLocal abs v $ eval t2

    eval (t1 `Bind` AltNode tag1 abss t2) =
      eval t1 >>= \case
        VNode tag2 vs  | tag2 == tag1 -> stackFrameLocals abss vs $ eval t2
        _                             -> __IMPOSSIBLE__

    eval (App t ts) = evalApp t ts
    eval (Case n t alts) = evalCase n t alts
    eval (Unit t) = eval t

    -- Values
    eval (Node tag ts) = VNode tag <$> mapM eval ts
    eval (Var n) = fromMaybeM __IMPOSSIBLE__ $ stackFrameLookup n

    eval (Lit (LitNat i)) = pure $ BasNat i
    eval Empty = pure VEmpty

    eval t@Prim{} = error $ "TODO " ++ show t

    eval Store{} = __IMPOSSIBLE__
    eval Fetch{} = __IMPOSSIBLE__
    eval Update{} = __IMPOSSIBLE__
    eval Def{} = __IMPOSSIBLE__
    eval Error{} = __IMPOSSIBLE__
    eval Lit{} = __IMPOSSIBLE__
    eval t@Bind{} = error $ "MISSING: " ++ show t

    evalCase :: Int -> Term -> [Alt] -> Eval Value
    evalCase n t1 alts = do
      v <- fromMaybeM __IMPOSSIBLE__ $ stackFrameLookup n
      case v of
        VNode _ vs ->
          let (abss, t2) = selAlt v alts t1 in
          stackFrameLocals abss vs $
          eval t2
        BasNat _ ->
          let t2 = snd $ selAlt v alts t1 in
          eval t2
        _ -> __IMPOSSIBLE__

    evalApp :: Term -> [Term] -> Eval Value
    evalApp t1 ts
      | Prim prim <- t1   =
        let evalNat = \case
              BasNat i -> pure i
              VNode tag [BasNat i] | tag == natTag -> pure i
              Loc loc  -> evalNat =<< fromMaybeM __IMPOSSIBLE__ (heapLookup loc)
              _        -> __IMPOSSIBLE__ in
        BasNat . runPrim prim <$> mapM (evalNat <=< eval) ts

      | Def name <- t1 =
        let (abss, t2) = fromMaybe (error $ "can't find " ++ name ++ "\n" ++ prettyShow (App t1 ts)) $ getGlobal name in
        mapM eval ts >>= \vs ->
          stackCons (StackFrame $ zip abss vs) $
          eval t2
      | otherwise = __IMPOSSIBLE__

    -- Not used right now (Don't remove)
    -- sel :: Int -> Value -> Value
    -- sel 0 (VNode tag _) = Tag tag
    -- sel i (VNode _ vs) | Just v <- vs !!! i = v
    -- sel _ _ = __IMPOSSIBLE__

    selAlt :: Value -> [Alt] -> Term -> ([Abs], Term)
    selAlt v alts t
      | VNode tag _ <- v = go tag
      | Tag tag <- v = go tag
      where
        go tag1 =
          headWithDefault ([], t) $
          forMaybe alts $ \case
            AltNode tag2 abss t -> boolToMaybe (tag2 == tag1) (abss, t)
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

    heapInsert :: (MonadFresh Loc m, MonadState Env m) => Value -> m Loc
    heapInsert v = do
      loc <- fresh
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
  pretty (Tag tag) = pretty tag
  pretty (VNode tag vs) =
    pretty tag <+> text ("[" ++ intercalate ", " (map prettyShow vs) ++ "]")
  pretty VEmpty = text "()"
  pretty Undefined = text "⊥"
  pretty (Loc loc) = pretty loc

instance MonadFresh Loc Eval where
  fresh = do
    loc <- gets freshLoc
    modify $ \env -> env{freshLoc = succ env.freshLoc}
    pure loc
