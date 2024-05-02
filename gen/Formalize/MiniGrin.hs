module Formalize.MiniGrin where

import Data.Bifunctor (bimap, first)
import Formalize.Scope (Atom(None), Binder(MkBinder, body, usage), Cover(CEmptyL, CEmptyR, CExtendB, CExtendL, CExtendR), Pair(MkPair), Split(SEmptyL, SEmptyR, SExtendL, SExtendR), Split3, rezzSplit, rezzSplitLeft, splitJoinRight)
import qualified Formalize.Syntax.Grin (Definition(term, vars, varsUsage), Name(Only), Names(NCons, NNil), Term(AppDef, Bind, Return), Val(Var))
import qualified Formalize.Syntax.RcGrin (Definition(MkDef), Name(Dup, Only), Names(NCons, NNil), Term(AppDef, Bind, Return), Val(Var), dropsAux)
import Formalize.Utils.Erase (mapSig)

data SplitCover = SplitCover{splitlDelta :: Split3,
                             splitrDelta :: Split, splitrGamma :: Split, splitDelta :: Split3,
                             splitGamma :: Split3, splitl :: Split, splitr :: Split}
                    deriving Show

splitAndCover :: Split -> Cover -> SplitCover
splitAndCover SEmptyL CEmptyL
  = SplitCover (SEmptyR, SEmptyR) SEmptyR SEmptyL (SEmptyR, SEmptyR)
      (SEmptyL, SEmptyL)
      SEmptyR
      SEmptyL
splitAndCover SEmptyL CEmptyR
  = SplitCover (SEmptyR, SEmptyR) SEmptyR SEmptyR (SEmptyR, SEmptyR)
      (SEmptyL, SEmptyR)
      SEmptyL
      SEmptyR
splitAndCover SEmptyL (CExtendL c)
  = SplitCover (splitlDelta (splitAndCover SEmptyL c))
      (splitrDelta (splitAndCover SEmptyL c))
      (splitrGamma (splitAndCover SEmptyL c))
      (splitDelta (splitAndCover SEmptyL c))
      (mapSig (bimap (\ s -> SExtendR s) (\ s -> SExtendL s))
         (splitGamma (splitAndCover SEmptyL c)))
      (SExtendR (splitl (splitAndCover SEmptyL c)))
      (splitr (splitAndCover SEmptyL c))
splitAndCover SEmptyL (CExtendR c)
  = SplitCover (splitlDelta (splitAndCover SEmptyL c))
      (splitrDelta (splitAndCover SEmptyL c))
      (SExtendR (splitrGamma (splitAndCover SEmptyL c)))
      (splitDelta (splitAndCover SEmptyL c))
      (mapSig (bimap (\ s -> SExtendR s) (\ s -> SExtendR s))
         (splitGamma (splitAndCover SEmptyL c)))
      (splitl (splitAndCover SEmptyL c))
      (SExtendR (splitr (splitAndCover SEmptyL c)))
splitAndCover SEmptyL (CExtendB c)
  = SplitCover
      (mapSig (bimap (\ s -> SExtendR s) (\ s -> SExtendR s))
         (splitlDelta (splitAndCover SEmptyL c)))
      (splitrDelta (splitAndCover SEmptyL c))
      (SExtendL (splitrGamma (splitAndCover SEmptyL c)))
      (splitDelta (splitAndCover SEmptyL c))
      (mapSig (first (\ s -> SExtendL s))
         (splitGamma (splitAndCover SEmptyL c)))
      (SExtendL (splitl (splitAndCover SEmptyL c)))
      (SExtendR (splitr (splitAndCover SEmptyL c)))
splitAndCover SEmptyR CEmptyL
  = SplitCover (SEmptyL, SEmptyL) SEmptyL SEmptyL (SEmptyL, SEmptyL)
      (SEmptyR, SEmptyL)
      SEmptyR
      SEmptyR
splitAndCover SEmptyR CEmptyR
  = SplitCover (SEmptyL, SEmptyR) SEmptyL SEmptyL (SEmptyL, SEmptyR)
      (SEmptyL, SEmptyL)
      SEmptyR
      SEmptyR
splitAndCover SEmptyR (CExtendL c)
  = SplitCover
      (mapSig (bimap (\ s -> SExtendR s) (\ s -> SExtendL s))
         (splitlDelta (splitAndCover SEmptyR c)))
      (splitrDelta (splitAndCover SEmptyR c))
      (splitrGamma (splitAndCover SEmptyR c))
      (mapSig (bimap (\ s -> SExtendR s) (\ s -> SExtendL s))
         (splitDelta (splitAndCover SEmptyR c)))
      (splitGamma (splitAndCover SEmptyR c))
      (SExtendL (splitl (splitAndCover SEmptyR c)))
      (splitr (splitAndCover SEmptyR c))
splitAndCover SEmptyR (CExtendR c)
  = SplitCover (splitlDelta (splitAndCover SEmptyR c))
      (SExtendR (splitrDelta (splitAndCover SEmptyR c)))
      (splitrGamma (splitAndCover SEmptyR c))
      (mapSig (bimap (\ s -> SExtendR s) (\ s -> SExtendR s))
         (splitDelta (splitAndCover SEmptyR c)))
      (splitGamma (splitAndCover SEmptyR c))
      (splitl (splitAndCover SEmptyR c))
      (SExtendL (splitr (splitAndCover SEmptyR c)))
splitAndCover SEmptyR (CExtendB c)
  = SplitCover
      (mapSig (first (\ s -> SExtendL s))
         (splitlDelta (splitAndCover SEmptyR c)))
      (SExtendL (splitrDelta (splitAndCover SEmptyR c)))
      (splitrGamma (splitAndCover SEmptyR c))
      (mapSig (first (\ s -> SExtendL s))
         (splitDelta (splitAndCover SEmptyR c)))
      (splitGamma (splitAndCover SEmptyR c))
      (SExtendL (splitl (splitAndCover SEmptyR c)))
      (SExtendL (splitr (splitAndCover SEmptyR c)))
splitAndCover (SExtendL s) CEmptyL
  = SplitCover (splitlDelta (splitAndCover s CEmptyL))
      (SExtendR (splitrDelta (splitAndCover s CEmptyL)))
      (splitrGamma (splitAndCover s CEmptyL))
      (mapSig (bimap (\ s -> SExtendR s) (\ s -> SExtendR s))
         (splitDelta (splitAndCover s CEmptyL)))
      (splitGamma (splitAndCover s CEmptyL))
      (splitl (splitAndCover s CEmptyL))
      (SExtendL (splitr (splitAndCover s CEmptyL)))
splitAndCover (SExtendL s) CEmptyR
  = SplitCover
      (mapSig (bimap (\ s -> SExtendR s) (\ s -> SExtendL s))
         (splitlDelta (splitAndCover s CEmptyR)))
      (splitrDelta (splitAndCover s CEmptyR))
      (splitrGamma (splitAndCover s CEmptyR))
      (mapSig (bimap (\ s -> SExtendR s) (\ s -> SExtendL s))
         (splitDelta (splitAndCover s CEmptyR)))
      (splitGamma (splitAndCover s CEmptyR))
      (SExtendL (splitl (splitAndCover s CEmptyR)))
      (splitr (splitAndCover s CEmptyR))
splitAndCover (SExtendL s) (CExtendL c)
  = SplitCover
      (mapSig (bimap (\ s -> SExtendR s) (\ s -> SExtendL s))
         (splitlDelta (splitAndCover s c)))
      (splitrDelta (splitAndCover s c))
      (splitrGamma (splitAndCover s c))
      (mapSig (bimap (\ s -> SExtendR s) (\ s -> SExtendL s))
         (splitDelta (splitAndCover s c)))
      (splitGamma (splitAndCover s c))
      (SExtendL (splitl (splitAndCover s c)))
      (splitr (splitAndCover s c))
splitAndCover (SExtendL s) (CExtendR c)
  = SplitCover (splitlDelta (splitAndCover s c))
      (SExtendR (splitrDelta (splitAndCover s c)))
      (splitrGamma (splitAndCover s c))
      (mapSig (bimap (\ s -> SExtendR s) (\ s -> SExtendR s))
         (splitDelta (splitAndCover s c)))
      (splitGamma (splitAndCover s c))
      (splitl (splitAndCover s c))
      (SExtendL (splitr (splitAndCover s c)))
splitAndCover (SExtendL s) (CExtendB c)
  = SplitCover
      (mapSig (first (\ s -> SExtendL s))
         (splitlDelta (splitAndCover s c)))
      (SExtendL (splitrDelta (splitAndCover s c)))
      (splitrGamma (splitAndCover s c))
      (mapSig (first (\ s -> SExtendL s))
         (splitDelta (splitAndCover s c)))
      (splitGamma (splitAndCover s c))
      (SExtendL (splitl (splitAndCover s c)))
      (SExtendL (splitr (splitAndCover s c)))
splitAndCover (SExtendR s) CEmptyL
  = SplitCover (splitlDelta (splitAndCover s CEmptyL))
      (splitrDelta (splitAndCover s CEmptyL))
      (SExtendR (splitrGamma (splitAndCover s CEmptyL)))
      (splitDelta (splitAndCover s CEmptyL))
      (mapSig (bimap (\ s -> SExtendR s) (\ s -> SExtendR s))
         (splitGamma (splitAndCover s CEmptyL)))
      (splitl (splitAndCover s CEmptyL))
      (SExtendR (splitr (splitAndCover s CEmptyL)))
splitAndCover (SExtendR s) CEmptyR
  = SplitCover (splitlDelta (splitAndCover s CEmptyR))
      (splitrDelta (splitAndCover s CEmptyR))
      (splitrGamma (splitAndCover s CEmptyR))
      (splitDelta (splitAndCover s CEmptyR))
      (mapSig (bimap (\ s -> SExtendR s) (\ s -> SExtendL s))
         (splitGamma (splitAndCover s CEmptyR)))
      (SExtendR (splitl (splitAndCover s CEmptyR)))
      (splitr (splitAndCover s CEmptyR))
splitAndCover (SExtendR s) (CExtendL c)
  = SplitCover (splitlDelta (splitAndCover s c))
      (splitrDelta (splitAndCover s c))
      (splitrGamma (splitAndCover s c))
      (splitDelta (splitAndCover s c))
      (mapSig (bimap (\ s -> SExtendR s) (\ s -> SExtendL s))
         (splitGamma (splitAndCover s c)))
      (SExtendR (splitl (splitAndCover s c)))
      (splitr (splitAndCover s c))
splitAndCover (SExtendR s) (CExtendR c)
  = SplitCover (splitlDelta (splitAndCover s c))
      (splitrDelta (splitAndCover s c))
      (SExtendR (splitrGamma (splitAndCover s c)))
      (splitDelta (splitAndCover s c))
      (mapSig (bimap (\ s -> SExtendR s) (\ s -> SExtendR s))
         (splitGamma (splitAndCover s c)))
      (splitl (splitAndCover s c))
      (SExtendR (splitr (splitAndCover s c)))
splitAndCover (SExtendR s) (CExtendB c)
  = SplitCover
      (mapSig (bimap (\ s -> SExtendR s) (\ s -> SExtendR s))
         (splitlDelta (splitAndCover s c)))
      (splitrDelta (splitAndCover s c))
      (SExtendL (splitrGamma (splitAndCover s c)))
      (splitDelta (splitAndCover s c))
      (mapSig (first (\ s -> SExtendL s))
         (splitGamma (splitAndCover s c)))
      (SExtendL (splitl (splitAndCover s c)))
      (SExtendR (splitr (splitAndCover s c)))

perceusName ::
            Split -> Formalize.Syntax.Grin.Name -> Formalize.Syntax.RcGrin.Name
perceusName SEmptyL Formalize.Syntax.Grin.Only
  = Formalize.Syntax.RcGrin.Only
perceusName SEmptyR Formalize.Syntax.Grin.Only
  = Formalize.Syntax.RcGrin.Dup
perceusName (SExtendL SEmptyL) Formalize.Syntax.Grin.Only
  = Formalize.Syntax.RcGrin.Dup
perceusName (SExtendL SEmptyR) Formalize.Syntax.Grin.Only
  = Formalize.Syntax.RcGrin.Dup
perceusName (SExtendR SEmptyL) Formalize.Syntax.Grin.Only
  = Formalize.Syntax.RcGrin.Only
perceusName (SExtendR SEmptyR) Formalize.Syntax.Grin.Only
  = Formalize.Syntax.RcGrin.Only

perceusNames ::
             Split ->
               Formalize.Syntax.Grin.Names -> Formalize.Syntax.RcGrin.Names
perceusNames SEmptyL (Formalize.Syntax.Grin.NNil None)
  = Formalize.Syntax.RcGrin.NNil None
perceusNames SEmptyR (Formalize.Syntax.Grin.NNil None)
  = Formalize.Syntax.RcGrin.NNil None
perceusNames s
  (Formalize.Syntax.Grin.NCons
     (MkPair c Formalize.Syntax.Grin.Only ns))
  = Formalize.Syntax.RcGrin.NCons
      (MkPair c
         (perceusName (splitl (splitAndCover s c))
            Formalize.Syntax.Grin.Only)
         (perceusNames (splitr (splitAndCover s c)) ns))

perceusVal ::
           Split -> Formalize.Syntax.Grin.Val -> Formalize.Syntax.RcGrin.Val
perceusVal s (Formalize.Syntax.Grin.Var Formalize.Syntax.Grin.Only)
  = Formalize.Syntax.RcGrin.Var
      (perceusName s Formalize.Syntax.Grin.Only)

perceusTerm ::
            Split -> Formalize.Syntax.Grin.Term -> Formalize.Syntax.RcGrin.Term
perceusTerm s (Formalize.Syntax.Grin.Return v)
  = Formalize.Syntax.RcGrin.Return (perceusVal s v)
perceusTerm s (Formalize.Syntax.Grin.AppDef p xs)
  = Formalize.Syntax.RcGrin.AppDef p (perceusNames s xs)
perceusTerm s (Formalize.Syntax.Grin.Bind r (MkPair c tl b))
  = Formalize.Syntax.RcGrin.Bind r
      (MkPair c (perceusTerm (splitl (splitAndCover s c)) tl)
         (MkBinder SEmptyR
            (Formalize.Syntax.RcGrin.dropsAux (snd (rezzSplit (usage b) r))
               (usage b)
               (perceusTerm
                  (splitJoinRight (rezzSplitLeft (usage b) r)
                     (splitr (splitAndCover s c)))
                  (body b)))))

perceus ::
        Formalize.Syntax.Grin.Definition ->
          Formalize.Syntax.RcGrin.Definition
perceus f
  = Formalize.Syntax.RcGrin.MkDef (Formalize.Syntax.Grin.vars f)
      (Formalize.Syntax.RcGrin.dropsAux
         (snd
            (rezzSplit (Formalize.Syntax.Grin.varsUsage f)
               (Formalize.Syntax.Grin.vars f)))
         (Formalize.Syntax.Grin.varsUsage f)
         (perceusTerm SEmptyL (Formalize.Syntax.Grin.term f)))

