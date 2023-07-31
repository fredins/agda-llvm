{-# OPTIONS_GHC -Wno-all #-}

{-# LANGUAGE OverloadedRecordDot #-}

module Agda.Llvm.GrinInterpreter (interpretGrin) where

import           Control.Monad.Except   (Except, MonadError (throwError),
                                         runExcept, void)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, asks,
                                         runReaderT)
import           Data.Foldable          (find)
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IntMap

import           Agda.Llvm.Grin
import           Agda.Llvm.HeapPointsTo
import           Agda.Syntax.Literal    (Literal (LitNat))
import           Agda.Utils.Impossible
import           Agda.Utils.List
import           Agda.Utils.Maybe

newtype Heap = Heap{unHeap :: IntMap Term}

data Context = Context
  { heap :: Heap
  , abss :: [Abs]
  }

initContext :: [Abs] -> Context
initContext abss = Context{heap = Heap mempty, abss = abss}

type Eval = ReaderT Context (Except String)


-- TODO Finish once transformed into lower level GRIN

interpretGrin :: AbstractContext -> [GrinDefinition] -> Either String Integer
interpretGrin absCxt defs =
    runExcept $ runReaderT (eval main.gr_term) (initContext main.gr_args)
  where
    main = fromMaybe __IMPOSSIBLE__ $ find gr_isMain defs

    lookupHeap :: MonadReader Context m => Loc -> m (Maybe Term)
    lookupHeap (MkLoc gid) = asks $ IntMap.lookup (unGid gid) . unHeap . heap

    insertHeap :: MonadReader Context m => Loc -> Term -> m ()
    insertHeap (MkLoc gid) v =
      void $ asks $ IntMap.insert (unGid gid) v . unHeap . heap

    deBruijnLookup :: MonadReader Context m => Int -> m Abs
    deBruijnLookup n = asks $ fromMaybe __IMPOSSIBLE__ . (!!! n) . abss

    eval :: Term -> Eval Integer

    eval (App (Def "printf") [v]) =
      evalVal v >>= \case
        Lit (LitNat i) -> pure i
        _              -> throwError ""

    eval (Case n t alts) = undefined
    eval (App v vs)      = undefined
    eval (Unit v)        = undefined

    eval (Bind t alt)    = undefined
      where
        go (Store loc v)    = insertHeap loc v
        go (Fetch v)        = undefined
        go (Update _ v1 v2) = undefined
        go (Case n t alts)  = undefined
        go (App v vs)       = undefined
        go (Unit v)         = undefined
        go (Bind t alt)     = undefined
        go Error{}          = __IMPOSSIBLE__


    eval Error{}  = __IMPOSSIBLE__
    eval Store{}  = __IMPOSSIBLE__
    eval Fetch{}  = __IMPOSSIBLE__
    eval Update{} = __IMPOSSIBLE__


    evalVal val = case val of
      Var n   -> undefined -- fromMaybe __IMPOSSIBLE__ . envLookup =<< deBruijnLookup n

      -- WHNF
      Lit{}   -> pure val
      Node{}  -> pure val

      -- Unsure
      Empty{} -> __IMPOSSIBLE__
      Def{}   -> __IMPOSSIBLE__
      Prim{}  -> __IMPOSSIBLE__







