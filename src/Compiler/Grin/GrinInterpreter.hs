{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE LexicalNegation     #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Compiler.Grin.GrinInterpreter
  ( interpretGrin
  , printInterpretGrin ) where

import           Control.Monad             (ap, when, (<=<), (>=>))
import           Control.Monad.Except      (Except, ExceptT,
                                            MonadError (throwError), liftEither,
                                            runExceptT, tryError)
import           Control.Monad.Reader      (MonadIO (liftIO), MonadReader,
                                            ReaderT (runReaderT), ask, asks,
                                            local)
import           Control.Monad.State       (MonadState, StateT (runStateT),
                                            evalStateT, get, gets)
import           Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import           Data.Bifunctor            (Bifunctor (bimap))
import           Data.Foldable             (find, foldlM, maximumBy, toList)
import           Data.Function             (on)
import           Data.List                 (intercalate, intersperse, singleton,
                                            sortBy, sortOn)
import           Data.List.Extra           (cons)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Ord                  (Down (Down), comparing)
import           Data.Tuple.Extra          (dupe, second)
import           GHC.Stack.Types           (CallStack, HasCallStack)
import           Prelude                   hiding ((!!))

import           Agda.Compiler.Backend     hiding (Prim, initEnv)
import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.Literal       (Literal (LitNat))
import           Agda.Utils.CallStack      (withCallerCallStack)
import           Agda.Utils.Either         (maybeToEither)
import           Agda.Utils.Functor
import           Agda.Utils.Impossible     (__IMPOSSIBLE__)
import           Agda.Utils.Lens
import           Agda.Utils.List
import           Agda.Utils.Maybe


import           Agda.Utils.Function       (applyWhen)
import           Compiler.Grin.Grin
import           Debug.Trace               (trace, traceM)
import           System.IO.Unsafe          (unsafePerformIO)
import qualified Utils.Map                 as Map
import           Utils.Utils


------------------------------------------------------------------------
-- * Data types and auxiliary functions
------------------------------------------------------------------------

-- | Semantic values
data Value
  = BasNat Integer
  | VTag Tag
  | VNode Tag [Value]
  | Loc Loc
  | VEmpty
  | Undefined
  deriving (Show)

instance Pretty Value where
  pretty (BasNat n) = pretty n
  pretty (VTag tag) = pretty tag
  pretty (VNode tag vs) =
    text "{" <> text (intercalate ", " (prettyShow tag : map prettyShow vs)) <> text "}"
  pretty VEmpty = text "()"
  pretty Undefined = text "⊥"
  pretty (Loc loc) = pretty loc

data Error
  = NoStackFrame
  | DanglingStackPointer Abs
  | DanglingHeapPointer Loc
  | UnboxedAllocation Value
  | ExpectedHeapPointer Value
  | ExpectedNode Value
  | ExpectedTag Value
  | ExpectedNat Value
  | StackFrameIndexOutOfBounds Int StackFrame
  | TagMismatch Tag Tag
  | DefinitionNotInScope
  | MainNotInScope
  deriving Show

errorString :: Error -> String
errorString = \case
  NoStackFrame                 -> "NoStackFrame"
  DanglingStackPointer{}       -> "DanglingStackPointer"
  DanglingHeapPointer{}        -> "DanglingHeapPointer"
  UnboxedAllocation{}          -> "UnboxedAllocation"
  ExpectedHeapPointer{}        -> "ExpectedHeapPointer"
  ExpectedNode{}               -> "ExpectedNode"
  ExpectedTag{}                -> "ExpectedTag"
  ExpectedNat{}                -> "ExpectedNat"
  StackFrameIndexOutOfBounds{} -> "StackFrameIndexOutOfBounds"
  TagMismatch{}                -> "TagMismatch"
  DefinitionNotInScope         -> "DefinitionNotInScope"
  MainNotInScope               -> "MainNotInScope"

mkErrorDoc :: Error -> String -> Doc
mkErrorDoc e s = text (errorString e) <> text ":" <> space <> text s

-- TODO
instance Pretty Error where
  pretty e = case e of
    NoStackFrame                    -> mkErrorDoc e "Stack is empty!"
    DanglingHeapPointer loc         -> mkErrorDoc e (prettyShow loc)
    TagMismatch tag tag'            -> mkErrorDoc e $ unwords ["Expected", prettyShow tag, "but got", prettyShow tag']
    ExpectedHeapPointer v           -> mkErrorDoc e $ "Expected heap pointer but got " ++ prettyShow v
    StackFrameIndexOutOfBounds n sf -> mkErrorDoc e $ unwords ["Index", show n, "is out of bounds for the stack frame", prettyShow sf]
    _                               -> mkErrorDoc e ""

data InterpreterError = InterpreterError
  { err      :: Error
  , location :: CallStack }
  deriving Show

interpreterError :: HasCallStack => Error -> InterpreterError
interpreterError e = withCallerCallStack $ InterpreterError e

instance Pretty InterpreterError where
  pretty (InterpreterError err loc) = text (prettyCallStackShort loc) <> text "\n" <> pretty err

throw :: (HasCallStack, MonadError InterpreterError m) => Error -> m a
throw e = withCallerCallStack $ throwError . InterpreterError e

-- FIXME: record
data HeapNode = HeapNode Integer Tag [Value]

instance Pretty HeapNode where
  pretty (HeapNode n tag vs) = text "{" <> text (intercalate ", " $ prettyShow n : prettyShow tag : map prettyShow vs) <> text "}"

type Heap = Map Loc HeapNode

prettyHeap :: Heap -> Doc
prettyHeap heap
  | Map.null heap = text "∅"
  | otherwise = vcat $ map step $ Map.toList heap
  where
  step (x, v) = pretty x <+> text "→" <+> pretty v

type StackFrame = [(Abs, Value)]
type Stack = [StackFrame]

prettyStackFrame :: StackFrame -> Doc
prettyStackFrame stackFrame = vcat $ map step stackFrame
  where
  step (x, v) = pretty x <+> text "→" <+> pretty v

data Ctx = Ctx
  { stack :: Stack
  , defs  :: [GrinDefinition]
  , name  :: String }

initCtx :: [GrinDefinition] -> Ctx
initCtx defs = Ctx
  { stack = [mempty]
  , defs = defs
  , name = "main" }

lensStack :: Lens' Ctx Stack
lensStack f env = f env.stack <&> \stack -> env{stack = stack}

lensDefs :: Lens' Ctx [GrinDefinition]
lensDefs f env = f env.defs <&> \defs -> env{defs = defs}

lensName :: Lens' Ctx String
lensName f env = f env.name <&> \name -> env{name = name}

stackFrameLookup :: (HasCallStack, MonadError InterpreterError m, MonadReader Ctx m) => Int -> m Value
stackFrameLookup n =  do
  stack <- view lensStack
  liftEither do
    stackFrame <- caseList stack (Left $ interpreterError NoStackFrame) (const . Right)
    maybeToEither
      do interpreterError $ StackFrameIndexOutOfBounds n stackFrame
      do snd <$> stackFrame !!! n

stackFrameLocal :: (HasCallStack, MonadError InterpreterError m, MonadReader Ctx m) => Abs -> Value -> m a -> m a
stackFrameLocal x v f = do
  stack <- view lensStack
  g <- liftEither $ caseList stack (Left $ interpreterError NoStackFrame)
                  $ Right .: const . cons . cons (x, v)
  locally lensStack g f

stackFrameLocals :: (HasCallStack, MonadReader Ctx m, MonadError InterpreterError m) => [Abs] -> [Value] -> m a -> m a
stackFrameLocals [] [] = id
stackFrameLocals xs vs = foldl do \ f (x, v) -> f . stackFrameLocal x v
                               do id
                               do zip xs (vs ++ repeat Undefined)

data ActionType = Produce | Consume

data Action = Action ActionType Loc String


move :: Loc -> String -> String -> [Action] -> [Action]
move loc name name' xs = Action Consume loc name : Action Produce loc name' : xs

fromActionType :: ActionType -> Int -> Int
fromActionType Produce = (+ 1)
fromActionType Consume = (- 1)

toActions :: (Num n, Ord n) => Loc -> String -> n -> [Action] -> [Action]
toActions loc name n xs
  | n <= -1   = toActions loc name (n + 1) (Action Consume loc name : xs)
  | n >= 1    = toActions loc name (n - 1) (Action Produce loc name : xs)
  | otherwise = xs

foldAction :: [Action] -> String -> Loc -> Int
foldAction xs name loc = foldr step 0 xs
  where
  step (Action typ loc' name') =
    applyWhen (loc == loc' && name == name') (fromActionType typ)

data Env = Env
  { heap                 :: Heap
  , allocation_info      :: Map Tag Int
  , highest_memory_usage :: Heap
  , actions              :: [Action] }

initEnv :: Env
initEnv = Env
  { heap                 = mempty
  , allocation_info      = mempty
  , highest_memory_usage = mempty
  , actions              = mempty }

lensHeap :: Lens' Env Heap
lensHeap f env = f env.heap <&> \heap -> env{heap = heap}

lensAllocationInfo :: Lens' Env (Map Tag Int)
lensAllocationInfo f env = f env.allocation_info <&> \x -> env{allocation_info = x}

lensHighestMemoryUsage :: Lens' Env Heap
lensHighestMemoryUsage f env = f env.highest_memory_usage <&> \x -> env{highest_memory_usage = x}

lensActions :: Lens' Env [Action]
lensActions f env = f env.actions <&> \ x -> env{actions = x}

allocate
  :: ( HasCallStack, MonadFresh Int m
     , MonadError InterpreterError m
     , MonadState Env m, MonadReader Ctx m )
  => Value
  -> m Loc
allocate (VNode tag vs) = do
  loc <- freshLoc
  heap <- use lensHeap
  name <- view lensName
  lensActions %= cons (Action Produce loc name)
  let heap' = Map.insert loc (HeapNode 1 tag vs) heap
  lensHeap .= heap'
  lensAllocationInfo %= Map.adjustWithDefault tag succ 1
  highest_memory_usage <- use lensHighestMemoryUsage
  when do on (>) length heap' highest_memory_usage
       do lensHighestMemoryUsage .= heap'
  pure loc
allocate v = throw (UnboxedAllocation v)

heapLookup :: (HasCallStack, MonadError InterpreterError m, MonadState Env m) => Loc -> m HeapNode
heapLookup loc = do
  heap <- gets heap
  liftEither $ maybeToEither
    do interpreterError (DanglingHeapPointer loc)
    do Map.lookup loc heap


sel :: Int -> HeapNode -> Value
sel 0 (HeapNode i _ _) = BasNat i
sel 1 (HeapNode _ tag _) = VTag tag
sel i (HeapNode _ _ vs)
  | Just v <- vs !!! (i - 2) = v
  | 2 <= i && i <= 3 = Undefined
sel _ _ = __IMPOSSIBLE__ -- TODO error

fromHeapNode :: HeapNode -> Value
fromHeapNode (HeapNode _ tag vs) = VNode tag vs

fetch :: (HasCallStack, MonadError InterpreterError m, MonadState Env m, MonadReader Ctx m) => Int -> m HeapNode
fetch n = do
  v <- stackFrameLookup n
  case v of
    Loc loc -> heapLookup loc
    v       -> throw (ExpectedHeapPointer v)

------------------------------------------------------------------------
-- * The evaluator
------------------------------------------------------------------------

-- TODO remove
traceHeap :: (MonadState Env m, MonadIO m) => Doc -> m ()
traceHeap s = do
  heap <- use lensHeap
  logIO $ render $ vcat [s, prettyHeap heap]

eval :: ( HasCallStack, MonadState Env m, MonadReader Ctx m
        , MonadError InterpreterError m, MonadFresh Int m, MonadIO m)
     => Term
     -> m Value
eval (App (Def "printf") [v]) = evalVal v

-- FIXME
-- eval (App (Def "eval") [v]) = evalVal v

-- Use an new (unused) memory location each time
eval (Store _ v `Bind` LAltVar x t2) = do
  v' <- evalVal v
  loc <- allocate v'
  traceHeap (text "store" <+> pretty loc <+> pretty v')
  stackFrameLocal x (Loc loc) (eval t2)

eval (FetchOpaque n) = fromHeapNode <$> fetch n

eval (Fetch tag n) = do
  HeapNode _ tag' vs <- fetch n
  if tag' == tag
    then pure (VNode tag vs)
    else throw (TagMismatch tag tag')

eval (FetchOpaqueOffset n i) = sel i <$> fetch n

eval (FetchOffset tag n i) = do
  HeapNode m tag' vs <- fetch n
  if tag' == tag
    then pure $ sel i (HeapNode m tag vs)
    else throw (TagMismatch tag tag')

eval (Update tag' tag n (ConstantNode _ vs)) = do
  v' <- stackFrameLookup n
  loc <- case v' of
    Loc loc -> pure loc
    v'      -> throw (ExpectedHeapPointer v')

  v' <- evalVal (ConstantNode tag' vs)
  case v' of
    VNode tag vs -> update loc tag vs
    v'           -> throw (ExpectedNode v')

  pure VEmpty
  where
  update
    :: ( HasCallStack, MonadError InterpreterError m
       , MonadState Env m, MonadIO m )
    => Loc
    -> Tag
    -> [Value]
    -> m ()
  update loc tag' vs' = do
    HeapNode count tag vs <- heapLookup loc
    lensHeap %= Map.insert loc (HeapNode count tag' vs')
    traceHeap $ text "update" <+> pretty loc <+> brackets (pretty (VNode tag vs) <+> text "→" <+> pretty (VNode tag' vs'))

eval (Dup n) = do
  v <- stackFrameLookup n
  loc <- case v of Loc loc -> pure loc
                   v'      -> throw (ExpectedHeapPointer v')
  HeapNode count tag vs <- heapLookup loc
  lensHeap %= Map.insert loc (HeapNode (count + 1) tag vs)
  traceHeap (text "dup" <+> pretty (HeapNode count tag vs) <+> pretty (HeapNode (count + 1) tag vs))
  pure VEmpty


eval (Decref n) = do
  v <- stackFrameLookup n
  loc <- case v of Loc loc -> pure loc
                   v'      -> throw (ExpectedHeapPointer v')
  HeapNode count tag vs <- heapLookup loc
  lensHeap %= Map.insert loc (HeapNode (count - 1) tag vs)
  traceHeap (text "decref" <+> pretty (HeapNode count tag vs) <+> pretty (HeapNode (count - 1) tag vs))
  pure VEmpty

eval (t1 `Bind` LAltVar x t2) = do
  v <- eval t1
  stackFrameLocal x v (eval t2)

eval (t1 `Bind` LAltConstantNode tag1 xs t2) = do
  v <- eval t1
  case v of
    VNode tag2 vs
      | tag2 == tag1 -> stackFrameLocals xs vs (eval t2)
      | otherwise    -> throw (TagMismatch tag1 tag2)
    v                -> throw (ExpectedNode v)

eval (t1 `Bind` LAltVariableNode x xs t2) = do
  v <- eval t1
  case v of
    VNode tag vs -> stackFrameLocals (x : xs) (VTag tag : vs) (eval t2)
    v            -> throw (ExpectedNode v)

eval (t1 `BindEmpty` t2) = eval t1 *> eval t2

eval (App (Prim prim) vs) = do
  vs' <- mapM evalVal vs
  ns <- mapM toNat vs'
  pure $ BasNat $ runPrim prim ns
  where
  toNat (BasNat i) = pure i
  toNat v          = throw (ExpectedNat v)

  runPrim :: TPrim -> [Integer] -> Integer
  runPrim PAdd [n1, n2] = n1 + n2
  runPrim PSub [n1, n2] = n1 - n2
  runPrim p vs          = error $ "TODO " ++ show p ++ " " ++ show vs

eval (App (Def "free") [Var n]) = do
  v <- stackFrameLookup n
  case v of
    Loc loc -> do
      lensHeap %= Map.delete loc
      traceHeap (text "free" <+> pretty loc)
    v       -> throw (ExpectedHeapPointer v)
  pure VEmpty

eval (App (Def name') vs) = do
  vs' <- mapM evalVal vs
  defs <- view lensDefs
  def <- liftEither $ maybeToEither (interpreterError DefinitionNotInScope)
                    $ find (\ def -> def.gr_name == name') defs
  let sf = reverse $ zip def.gr_args vs'
  let locs = forMaybe vs' \case Loc loc -> Just loc
                                _       -> Nothing
  name <- view lensName
  lensActions %= \ xs -> foldr (\ loc -> move loc name name') xs locs
  logIO $ unwords ["call", name', "from", name]
  v <- locally lensStack (sf :) $ locally lensName (const name') (eval def.gr_term)
  logIO $ unwords ["return to", name]
  pure v

eval (Case v def alts) = do
  v' <- evalVal v
  let (xs, t) = selAlt v' alts def
      f = case v' of
        VNode _ vs -> stackFrameLocals xs vs
        BasNat _   -> id
        VTag _     -> id
        _          -> __IMPOSSIBLE__ -- TODO

  f (eval t)

  where
  vTagView (VNode tag _) = Just tag
  vTagView (VTag tag)    = Just tag
  vTagView _             = Nothing

  selAlt :: Value -> [CAlt] -> Term -> ([Abs], Term)
  selAlt (vTagView -> Just tag1) alts t =
    case matching of
      [] | t == Unreachable -> error $ "Couln't match tag " ++ prettyShow tag1 ++ " in\n" ++ render (nest 4 (pretty (Case v def alts)))
      []    -> ([], t)
      [alt] -> alt
      _     -> __IMPOSSIBLE__-- TODO
    where
    matching = forMaybe alts $ \case
      CAltConstantNode tag2 xs t -> boolToMaybe (tag2 == tag1) (xs, t)
      CAltTag tag2 t             -> boolToMaybe (tag2 == tag1) ([], t)
      _                          -> __IMPOSSIBLE__ -- TODO

  selAlt (BasNat n1) alts t =
    case matching of
      [] | t == Unreachable -> error $ "Couln't match lit " ++ prettyShow n1
      []                    -> ([], t)
      [t]                   -> ([], t)
      _                     -> __IMPOSSIBLE__ -- TODO
    where
    matching = forMaybe alts $ \case
      CAltLit (LitNat n2) t -> boolToMaybe (n2 == n1) t
      _                     -> __IMPOSSIBLE__ -- TODO
  selAlt v _ _ = error $ show v

eval (Unit v) = do
  v' <- evalVal v
  case v' of
    VNode _ vs -> do
      let locs = forMaybe vs \case Loc loc -> Just loc
                                   _       -> Nothing
      name <- view lensName
      lensActions %= \ xs -> foldr (\ loc -> cons $ Action Consume loc name) xs locs
      pure v'
    _ -> pure v'

eval Store{} = __IMPOSSIBLE__
eval Fetch'{} = __IMPOSSIBLE__
eval Update{} = __IMPOSSIBLE__
eval Error{} = __IMPOSSIBLE__
eval App{} = __IMPOSSIBLE__
eval t@Bind{} = error $ "MISSING: " ++ prettyShow t

evalVal :: (HasCallStack, MonadError InterpreterError m, MonadReader Ctx m) => Val -> m Value
evalVal (Var n) = stackFrameLookup n
evalVal (ConstantNode tag vs) = VNode tag . toList <$> mapM evalVal vs
evalVal (VariableNode n vs) = do
  v <- stackFrameLookup n
  case v of
    VTag tag -> VNode tag <$> mapM evalVal vs
    v        -> throw (ExpectedTag v)
evalVal (Tag tag) = pure (VTag tag)
evalVal Empty   = pure VEmpty
evalVal (Lit (LitNat i)) = pure (BasNat i)
evalVal _ = __IMPOSSIBLE__

------------------------------------------------------------------------
-- Interface
------------------------------------------------------------------------

-- Orphaned instance
instance MonadFresh i m => MonadFresh i (ExceptT e m)

interpretGrin :: (MonadIO m, MonadFresh Int m)
              => [GrinDefinition]
              -> m (Either InterpreterError (Value, Env))
interpretGrin defs
  | Just main <- find gr_isMain defs =
    runExceptT $ flip runReaderT (initCtx defs)
               $ flip runStateT initEnv
               $ eval main.gr_term
  | otherwise = pure $ Left (interpreterError MainNotInScope)

printResult :: (Value, Env) -> IO ()
printResult (val, env) = do
  let allocation_counts =
        map (\(tag, n) -> (pretty tag <> text ":") <+> pretty n) $
        sortOn (Down . snd) $
        Map.toList env.allocation_info

  let allocations = foldr (\x y -> snd x + y) 0 $ Map.toList env.allocation_info


  putStrLn $ render $ vcat
    [ text "Result:" <+> pretty val
    , text "Allocations" <+> vcat (allocation_counts ++ [text "Total:" <+> pretty allocations])
    , text "Highest memory usage: " <+> pretty (length env.highest_memory_usage) <+> text "nodes."
    , text "In use at exit:" <+> pretty (Map.size env.heap) ]
  putStrLn $ render $ prettyHeap env.heap
  -- putStrLn $ render $ text "\nHeap at highest memory usage:\n" <> nest 2 (pretty env.highest_memory_usage)

printInterpretGrin :: (MonadIO m, MonadFresh Int m) => [GrinDefinition] -> m ()
printInterpretGrin = liftIO . either (putStrLn . prettyShow) printResult <=< interpretGrin
