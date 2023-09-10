{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Agda.Llvm.GrinInterpreter (module Agda.Llvm.GrinInterpreter) where

import           Control.Monad             ((>=>))
import           Control.Monad.Reader      (MonadReader, ReaderT (runReaderT),
                                            local, MonadIO (liftIO))
import           Control.Monad.State       (StateT (runStateT), evalStateT, gets)
import           Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import           Data.Bifunctor            (Bifunctor (bimap))
import           Data.Foldable             (find, toList)
import           Data.List                 (intercalate, singleton, intersperse)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Tuple.Extra          (dupe, second)

import           Agda.Compiler.Backend     hiding (Prim, initEnv)
import           Agda.Llvm.Grin            
import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.Literal       (Literal (LitNat))
import           Agda.Utils.Functor
import           Agda.Utils.Impossible
import           Agda.Utils.Lens
import           Agda.Utils.List
import           Agda.Utils.Maybe
import Agda.Llvm.Utils (trace', logIO)


data Value = BasNat Integer
           | VTag Tag
           | VNode Tag Integer [Value]
           | Loc Loc
           | VEmpty
           | Undefined
             deriving (Show)

newtype Heap = Heap{unHeap :: Map Loc Value}

newtype StackFrame = StackFrame{unStackFrame :: [(Abs, Value)]}

type Stack = [StackFrame]

newtype Env = Env
  { heap :: Heap
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

-- FIXME remove

traceHeap s = do
  heap <- use lensHeap
  trace' (render s ++ ":\n" ++ prettyShow (Heap heap) ++ "\n") pure ()


printInterpretGrin :: (MonadIO m, MonadFresh Int m) => [GrinDefinition] -> m ()
printInterpretGrin defs = do 
  (val, heap) <- interpretGrin defs
  liftIO $ putStrLn $ render $ vcat 
    [ text "Result:" <+> pretty val 
    , text "Heap:" <+> pretty heap ]

interpretGrin :: forall mf. MonadFresh Int mf => [GrinDefinition] -> mf (Value, Heap)
interpretGrin defs = 
    second heap <$> runReaderT (runStateT (eval main.gr_term) initEnv) [StackFrame []]
  where
    main = fromMaybe __IMPOSSIBLE__ $ find gr_isMain defs

    eval :: Term -> Eval mf Value
    eval (App (Def "printf") [v]) = evalVal v

    -- Use an new (unused) memory location each time
    eval (Store _ v `Bind` LAltVar x t2) = do
      v' <- evalVal v
      loc <- freshLoc
      lensHeap %= Map.insert loc v'
      traceHeap (text "store" <+> pretty loc <+> pretty v')
      stackFrameLocal x (Loc loc) (eval t2)

    eval fetch
      | Fetch _ n <- fetch = heapLookupLoc n
      | FetchOpaque n <- fetch = heapLookupLoc n
      | FetchOpaqueOffset n offset <- fetch = sel offset <$> heapLookupLoc n
      | FetchOffset _ n offset <- fetch = sel offset <$> heapLookupLoc n
      where
      sel 0 (VTag tag)    = VTag tag
      sel 0 (VNode tag _ _) = VTag tag
      sel 1 (VNode _ i _) = BasNat i
      sel i (VNode _ _ vs)
        | Just v <- vs !!! (i - 2) = v
        | 2 <= i && i <= 3 = Undefined
      sel _ _ = __IMPOSSIBLE__

      heapLookupLoc n = 
        fromMaybeM __IMPOSSIBLE__ . runMaybeT $ do
          loc <- stackFrameLookupLoc n
          MaybeT (Map.lookup loc <$> use lensHeap)

    eval (Update _ n t1 `BindEmpty` t2) = do
      loc <- fromMaybe __IMPOSSIBLE__ <$> runMaybeT (stackFrameLookupLoc n)
      v <- evalVal t1
      lensHeap %= update loc v
      traceHeap (text "update" <+> pretty loc <+> pretty v)
      eval t2
      where
      update loc (VNode tag _ vs) = Map.update (\(VNode _ i _) -> Just $ VNode tag i vs) loc  
      update _ _ = __IMPOSSIBLE__

    eval (t1 `Bind` LAltVar x t2) = do
      v <- eval t1
      stackFrameLocal x v (eval t2)

    eval (t1 `Bind` LAltConstantNode tag1 x xs t2) = do
      v <- eval t1
      case v of
        VNode tag2 i vs | tag2 == tag1 -> stackFrameLocals (x : xs) (BasNat i : vs) (eval t2)
        _                            -> __IMPOSSIBLE__

    eval (t1 `Bind` LAltVariableNode x1 x2 xs t2) = do
      v <- eval t1
      case v of
        VNode tag i vs -> stackFrameLocals (x1 : x2 : xs) (VTag tag : BasNat i : vs) (eval t2)
        _            -> __IMPOSSIBLE__

    eval (t1 `BindEmpty` t2) = eval t1 *> eval t2

    eval (App (Prim prim) vs) = do
      vs' <- mapM evalVal vs
      let ns = map toBasUnsafe vs'
      pure $ BasNat $ runPrim prim ns
      where
      toBasUnsafe (BasNat i) = i
      toBasUnsafe _ = __IMPOSSIBLE__

      runPrim :: TPrim -> [Integer] -> Integer
      runPrim PAdd [n1, n2] = n1 + n2
      runPrim PSub [n1, n2] = n1 - n2
      runPrim p vs          = error $ "TODO " ++ show p ++ " " ++ show vs


    eval (App (Def "free") [Var n]) = do
      loc <- fromMaybe __IMPOSSIBLE__ <$> runMaybeT (stackFrameLookupLoc n)
      lensHeap %= Map.delete loc
      traceHeap (text "free" <+> pretty loc)
      pure VEmpty

    eval (Drop n) = do 
      (x, v) <- fromMaybe __IMPOSSIBLE__ <$> runMaybeT (stackFrameLookup' n)
      sf <- view lensStackFrame     

      let loc = case v of 
            Loc loc -> loc
            _ -> error $ render $ vcat 
              [ text "DROP" <+> pretty (Drop n)
              , pretty x
              , text "SF:"
              , pretty sf]
      trace' (render $ text "drop" <+> pretty x <+> text "→" <+> pretty loc <+> text "\n") pure ()
      let sf = StackFrame [(head def.gr_args, v)]
      local (sf :) (eval def.gr_term)
      where
      def = fromMaybe __IMPOSSIBLE__  (find (("drop" ==) . gr_name) defs)


    eval (Dup n) = do 
      (x, v) <- fromMaybe __IMPOSSIBLE__ <$> runMaybeT (stackFrameLookup' n)
      let loc = case v of 
            Loc loc -> loc
            _ -> __IMPOSSIBLE__
      trace' (render $ text "dup" <+> pretty x <+> text "→" <+> pretty loc <+> text "\n") pure ()
      let sf = StackFrame [(head def.gr_args, v)]
      local (sf :) (eval def.gr_term)
      where
      def = fromMaybe __IMPOSSIBLE__  (find (("dup" ==) . gr_name) defs)

    eval (App (Def name) vs) = do
      vs' <- mapM evalVal vs
      let sf = StackFrame (reverse $ zip def.gr_args vs')
      local (sf :) (eval def.gr_term)
      where
      def = fromMaybe (error $ "Can't find " ++ name ++ " in " ++ show (map gr_name defs))  (find ((name ==) . gr_name) defs)

    eval (Case v def alts) = do
      v' <- evalVal v
      let (xs, t) = selAlt v' alts def
          f = case v' of
            VNode _ i vs -> stackFrameLocals xs (BasNat i : vs)
            BasNat _   -> id
            VTag _     -> id
            _          -> __IMPOSSIBLE__
      f (eval t)

      where
      vTagView (VNode tag _ _) = Just tag
      vTagView (VTag tag)      = Just tag
      vTagView _               = Nothing

      selAlt :: Value -> [CAlt] -> Term -> ([Abs], Term)
      selAlt (vTagView -> Just tag1) alts t =
        case matching of
          []    -> ([], t)
          [alt] -> alt
          _     -> __IMPOSSIBLE__
        where
        matching = forMaybe alts $ \case
          CAltConstantNode tag2 x xs t -> boolToMaybe (tag2 == tag1) (x : xs, t)
          CAltTag tag2 t             -> boolToMaybe (tag2 == tag1) ([], t)
          _                          -> __IMPOSSIBLE__

      selAlt (BasNat n1) alts t =
        case matching of
          []  -> ([], t)
          [t] -> ([], t)
          _   -> __IMPOSSIBLE__
        where
        matching = forMaybe alts $ \case
          CAltLit (LitNat n2) t -> boolToMaybe (n2 == n1) t
          _                     -> __IMPOSSIBLE__
      selAlt _ _ _ = __IMPOSSIBLE__

    eval (UpdateOffset n 1 v) = do
      v' <- evalVal v
      loc <- fromMaybe __IMPOSSIBLE__ <$> runMaybeT (stackFrameLookupLoc n)
      v_node <- fromMaybe __IMPOSSIBLE__ . Map.lookup loc <$> use lensHeap
      let v_node' = case (v_node, v') of
            (VNode tag _ vs , BasNat i) -> VNode tag i vs
            _ -> __IMPOSSIBLE__
      lensHeap %= Map.insert loc v_node'
      traceHeap (text "updateOffset" <+> pretty loc <+> pretty v_node <+> text "→" <+> pretty v_node')
      pure VEmpty
    
    eval UpdateOffset{} = error "TODO"

    eval (Unit v) = evalVal v
    eval Store{} = __IMPOSSIBLE__
    eval Fetch'{} = __IMPOSSIBLE__
    eval Update{} = __IMPOSSIBLE__
    eval Error{} = __IMPOSSIBLE__
    eval App{} = __IMPOSSIBLE__
    eval t@Bind{} = error $ "MISSING: " ++ prettyShow t

    evalVal :: Val -> Eval mf Value
    evalVal = fromMaybeM __IMPOSSIBLE__ . runMaybeT . evalVal'

    -- TODO fix __IMPOSSIBLE__ / Nothing
    evalVal' :: Val -> MaybeT (Eval mf) Value
    evalVal' (Var n) = stackFrameLookup n
    evalVal' (ConstantNode tag v vs) = 
      evalVal' v >>= \case
        BasNat i -> VNode tag i . toList <$> mapM evalVal' vs
        _ -> __IMPOSSIBLE__
    evalVal' (VariableNode n v vs) = do
      v_tag <- stackFrameLookup n
      v' <- evalVal' v
      case (v_tag, v') of
        (VTag tag, BasNat i) -> VNode tag i <$> mapM evalVal' vs -- TODO fromMaybe Undefined ?
        _        -> do 
          sf <- view lensStackFrame
          heap <- gets heap
          error $ "VARIABLENODE: " ++ prettyShow (VariableNode n v vs) ++ " GER  "  ++ show (v_tag, v')  ++
                  "\nSF:\n" ++ prettyShow sf ++ "\nHEAP:\n" ++ prettyShow heap
    evalVal' (Tag tag) = hoistMaybe (Just (VTag tag))
    evalVal' Empty   = hoistMaybe (Just VEmpty)
    evalVal' (Lit lit)
      | LitNat i <- lit = hoistMaybe (Just (BasNat i))
      | otherwise = __IMPOSSIBLE__
    evalVal' Prim{} = __IMPOSSIBLE__
    evalVal' Def{} = __IMPOSSIBLE__ -- TODO CAF

    stackFrameLookup :: MonadReader Stack m => Int -> MaybeT m Value
    stackFrameLookup n = MaybeT (view lensStackFrame <&> \sf -> map snd sf !!! n)

    stackFrameLookup' :: MonadReader Stack m => Int -> MaybeT m (Abs, Value)
    stackFrameLookup' n = MaybeT $ (!!! n) <$> view lensStackFrame

    stackFrameLookupLoc :: MonadReader Stack m => Int -> MaybeT m Loc
    stackFrameLookupLoc = (stackFrameLookup >=>) $ hoistMaybe . \case
      Loc loc -> Just loc
      _       -> Nothing

    stackFrameLocal :: MonadReader Stack m => Abs -> Value -> m a -> m a
    stackFrameLocal x v = locally lensStackFrame ((x, v) :)

    stackFrameLocals :: MonadReader Stack m => [Abs] -> [Value] -> m a -> m a
    stackFrameLocals [] _ = id
    stackFrameLocals xs vs = foldl (\f (x, v) -> f . stackFrameLocal x v) id xvs
      where
      xvs = zip xs (vs ++ repeat Undefined)

-- Instances

instance Pretty Heap where
  pretty (Heap heap) 
    | Map.null heap = text "∅"
    | otherwise = vcat $ map prettyEntry $ Map.toList heap
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
  pretty (VNode tag rc vs) =
    text "{" <> text (intercalate ", " ([prettyShow tag, prettyShow rc] ++ map prettyShow vs)) <> text "}"
  pretty VEmpty = text "()"
  pretty Undefined = text "⊥"
  pretty (Loc loc) = pretty loc
