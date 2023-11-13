{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Agda.Llvm.TreelessTransform
  ( definitionToTreeless
  , TreelessDefinition(..)
  ) where

import           Control.Monad                         (when, zipWithM, (<=<))
import           Control.Monad.IO.Class                (MonadIO (liftIO))
import           Data.Function                         (on)

import           Agda.Compiler.Backend                 hiding (Prim, initEnv)
import           Agda.Compiler.MAlonzo.HaskellTypes    (hsTelApproximation)
import           Agda.Compiler.Treeless.Builtin        (translateBuiltins)
import           Agda.Compiler.Treeless.NormalizeNames (normalizeNames)
import           Agda.Llvm.Grin                        (getShortName)
import           Agda.Llvm.Utils
import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.Internal                  (Type, arity)
import qualified Agda.Syntax.Internal                  as I
import           Agda.Syntax.Parser.Parser             (splitOnDots)
import           Agda.TypeChecking.Substitute
import           Agda.Utils.Functor
import           Agda.Utils.Impossible
import           Agda.Utils.List1             (List1, pattern (:|), (<|))
import qualified Agda.Utils.List1             as List1
import           Agda.Utils.List                       (downFrom, lastMaybe,
                                                        snoc)
import           Agda.Utils.Maybe
import           Agda.Utils.Monad                      (mapMM)
import           Data.Bitraversable                    (bimapM)
import           Data.List                             (mapAccumR)
import           Data.Tuple.Extra                      (first, firstM, second)
import Agda.Compiler.ToTreeless (closedTermToTreeless)
import Control.Applicative (Applicative(liftA2))
import Debug.Trace
import Agda.Llvm.Utils 
import Agda.Utils.Function (applyWhen)

data TreelessDefinition = TreelessDefinition
  { tl_name      :: String
  , tl_isMain    :: Bool
  , tl_arity     :: Int
  , tl_type      :: Type
  , tl_term      :: TTerm
  , tl_primitive :: Maybe TPrim
  }

instance Pretty TreelessDefinition where
  pretty TreelessDefinition{tl_name, tl_arity, tl_type, tl_term} =
    vcat
      [ pretty tl_name <+> text ":" <+> pretty tl_type
      , pretty tl_name <+> prettyList_ (take tl_arity ['a' ..]) <+>
        text "=" <+> pretty tl_term
      ]

-- | Rewrap primitives so they can be used lazily
wrapPrimitives :: TTerm -> TCM TTerm
wrapPrimitives (TPrim PAdd) = primNatPlus >>= closedTermToTreeless LazyEvaluation 
wrapPrimitives (TPrim PSub) = primNatMinus >>= closedTermToTreeless LazyEvaluation 
wrapPrimitives (TPrim PMul) = primNatTimes >>= closedTermToTreeless LazyEvaluation 
wrapPrimitives (TLet t1 t2) = liftA2 TLet (wrapPrimitives t1) (wrapPrimitives t2)
wrapPrimitives (TApp t ts) = liftA2 TApp (wrapPrimitives t) (mapM wrapPrimitives ts)
wrapPrimitives (TLam t) = TLam <$> wrapPrimitives t
wrapPrimitives (TCase n info t alts) = liftA2 (TCase n info) (wrapPrimitives t) (mapM step alts)
  where
  step :: TAlt -> TCM TAlt
  step (TACon q n t) = TACon q n <$> wrapPrimitives t
  step (TALit lit t) = TALit lit <$> wrapPrimitives t
  step TAGuard{} = __IMPOSSIBLE__ -- TODO
wrapPrimitives t = pure t

definitionToTreeless :: IsMain -> Definition -> TCM (Maybe TreelessDefinition)
definitionToTreeless isMainModule Defn{defName, defType, theDef=Function{funCompiled = Just cc}} = do
  (fmap . fmap)
    (mkTreelessDef . normalizeLets'' . insertLets  . skipLambdas )
    (mapMM (wrapPrimitives <=< normalizeNames) (toTreeless LazyEvaluation defName))
  where
    mkTreelessDef term = TreelessDefinition
      { tl_name      = prettyShow defName
      , tl_isMain    = isMain
      , tl_arity     = arity defType
      , tl_type      = defType
      , tl_term      = term
      , tl_primitive = Nothing
      }

    isMain =
      isMainModule == IsMain &&
      "main" == (prettyShow . nameConcrete . qnameName) defName

-- Create definitions for builtin functions so they can be used lazily.
definitionToTreeless _ Defn{defName, defType, theDef=Primitive{}} = do
    builtins <- mapM (firstM getBuiltin)
      [ (builtinNatPlus,  PAdd)
      , (builtinNatMinus, PSub)
      ]
    pure $ lookup (I.Def defName []) builtins <&> mkTreelessDef <*> skipLambdas . mkPrimApp
  where
    mkPrimApp prim =
      mkTLam defArity $ mkTApp (TPrim prim) $ map TVar $ downFrom defArity

    mkTreelessDef prim term = TreelessDefinition
      { tl_name      = prettyShow defName
      , tl_isMain    = False
      , tl_arity     = defArity
      , tl_type      = defType
      , tl_term      = term
      , tl_primitive = Just prim
      }

    defArity = arity defType

definitionToTreeless _ _ = pure Nothing

-- | Skip initial lambdas
skipLambdas :: TTerm -> TTerm
skipLambdas (TLam t) = skipLambdas t
skipLambdas t        = t

data LetSplit = LetSplit 
  { left    :: [TTerm]
  , letLhss :: List1 TTerm
  , letRhs  :: TTerm
  , right   :: [TTerm] }
  deriving Show

splitOnLet :: [TTerm] -> Maybe LetSplit
splitOnLet (t : ts)  
  | (l : ls, r) <- tLetView t = 
    Just LetSplit 
      { left = []
      , letLhss = l :| ls 
      , letRhs = r
      , right = ts }
  | otherwise = splitOnLet ts <&> \x -> x{left = t : x.left}
splitOnLet [] = Nothing

mkSplit :: [TTerm] -> Maybe LetSplit
mkSplit = splitOnLet . map normalizeLets 



-- f a 
--   (let b'' = g (let b' = i b in b') in b'') 
--   (let c' = h c in c')
-- >>>
-- let b' = i b 
--     b'' = g b' 
--     c' = h c in 
-- f a b'' c'
normalizeLets :: TTerm -> TTerm
normalizeLets (TLet (TLet t1 t2) t3) = 
  normalizeLets $ TLet t1 $ TLet t2 $ raiseFrom 1 1 t3
normalizeLets (TLet t1 t2) = on TLet normalizeLets t1 t2
normalizeLets (TApp t (mkSplit -> Just split)) = 
  normalizeLets $ foldr TLet (TApp t' ts') split.letLhss
  where
  raiseN :: Subst a => a -> a
  raiseN = raise (length split.letLhss)
  t' = raiseN $ normalizeLets t
  ts' = raiseN split.left ++ [split.letRhs] ++ raiseN split.right
normalizeLets (TApp (TLet t1 t2) ts) = TLet t1 (TApp t2 $ raise 1 ts)
normalizeLets (TCase n info t alts) = 
  TCase n info (normalizeLets t) (map step alts)
  where
  step (TACon q a t) = TACon q a (normalizeLets t)
  step (TALit l t) = TALit l (normalizeLets t)
  step _ = __IMPOSSIBLE__
normalizeLets (TLam t) = TLam (normalizeLets t)
normalizeLets t = t

normalizeLets'' t 
  | t == t' = t'
  | otherwise = normalizeLets'' t'
  where 
  t' = normalizeLets t


p = TPrim PLt
f x y z = TApp (TPrim PAdd) [x , y , z]
g x = TApp p [x]
h = g
i = g

a = TVar 10
b = TVar 11
c = TVar 12

ex1 = f (TLet (g a) $ TVar 0) b c
ex2 = f (TLet (g a) $ TLet (h $ TVar 0) $ TVar 0) b c



-- | Simplify complicated applications
--
-- f a (g b) (h c)
-- >>>
-- let b' = g b in
-- let c' = h c in
-- f a b' c'
--
-- The only exception is PSeq which is not simplified.


--
-- f a (g (i b)) (h c)
-- >>>
-- f a 
--   (let b'' = g (let b' = i b in b') in b'') 
--   (let c' = h c in c')
--
-- let b' = i b 
--     b'' = g b' 
--     c' = h c in 
-- f a b'' c'


-- insertLets' :: Bool -> TTerm -> TTerm
-- insertLets' True (TApp (TPrim PSeq) (TVar n : TApp f [TVar n'] : xs)) = 
--   -- TLet (TApp (TPrim PSeq) (TVar n : TApp


-- The x in @seq x f x y z@ are always equal and a variable


letWrap :: TTerm -> TTerm
letWrap t = TLet t $ TVar 0

insertLets' p (TApp (TPrim PSeq) (x : f : xs)) = 
  applyWhen p letWrap $ TApp (TPrim PSeq) $ x : f' : xs'
  where
  f' = insertLets' False f
  xs' = map (insertLets' True) xs
insertLets' p (TApp t ts) = applyWhen p letWrap $ TApp t $ map (insertLets' True) ts
insertLets' p (TCon q) = applyWhen p letWrap $ TCon q
insertLets' p (TLit lit) = applyWhen p letWrap $ TLit lit
insertLets' _ (TVar n) = TVar n
insertLets' _ (TDef q) = TDef q
insertLets' _ (TLam _) = __IMPOSSIBLE__
insertLets' _ (TError e) = TError e
insertLets' _ (TPrim p) = TPrim p
insertLets' p (TLet t1 t2) = TLet (insertLets' False t1) (insertLets' p t2)
insertLets' p (TCase n info t alts) = 
  applyWhen p letWrap $ TCase n info t' $ map step alts 
  where 
  t' = insertLets' False t
  step :: TAlt -> TAlt
  step (TACon q n t) = TACon q n $ insertLets' False t
  step (TALit lit t) = TALit lit $ insertLets' False t
  step TAGuard{} = __IMPOSSIBLE__ -- TODO
insertLets' _ t = error $ "insertLets': " ++ prettyShow t

insertLets :: TTerm -> TTerm
insertLets = insertLets' False

simplifyApp (TApp (TPrim PSeq) ts) = error $ prettyShow $ TApp (TPrim PSeq) ts -- FIXME

-- TODO rewrite this mess
simplifyApp (tAppView -> (f, splitArgs -> Just (before, t, after))) =
  simplifyApp (foldr mkLet app ts)
  where
  ts = uncurry snoc (tLetView (simplifyApp t))
  raiseN :: Subst a => a -> a
  raiseN = raise (length ts)
  app = mkTApp (raiseN f) $ raiseN before ++ TVar 0 : raiseN after


simplifyApp t = case t of
  TLet t1 t2            -> on mkLet simplifyApp t1 t2
  TCase n info def alts -> TCase n info (simplifyApp def) (simplifyAppAlts alts)
  TLam t                -> TLam (simplifyApp t)
  TCon _                -> t
  TLit _                -> t
  TError _              -> t
  TVar _                -> t
  TPrim _               -> t
  TDef _                -> t
  TApp _ _              -> t

  _                     -> error $ "TODO " ++ show t


simplifyAppAlts :: [TAlt] -> [TAlt]
simplifyAppAlts = map go
  where
  go alt@TACon{aBody} = alt{aBody = simplifyApp aBody}
  go alt@TALit{aBody} = alt{aBody = simplifyApp aBody}
  go alt                = error $ "TODO " ++ (show alt)


-- Split arguments on the first application, constructor, or literal
splitArgs :: Args -> Maybe (Args, TTerm, Args)
splitArgs []              = Nothing
splitArgs (a@TApp{} : as) = Just ([], a, as)
splitArgs (a@TCon{} : as) = Just ([], a, as)
splitArgs (a@TLit{} : as) = Just ([], a, as)
splitArgs (a : as)
  | Just (before, t, after) <- splitArgs as = Just (a:before, t, after)
  | otherwise = Nothing
