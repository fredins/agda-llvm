{-# LANGUAGE ViewPatterns #-}

module Agda.Llvm.TreelessTransform
  ( definitionToTreeless
  , TreelessDefinition(..)
  ) where

import           Control.Monad                         (when, zipWithM)
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
import           Agda.Utils.List                       (downFrom, lastMaybe,
                                                        snoc)
import           Agda.Utils.Maybe
import           Agda.Utils.Monad                      (mapMM)
import           Data.Bitraversable                    (bimapM)
import           Data.List                             (mapAccumR)
import           Data.Tuple.Extra                      (first, firstM, second)

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

definitionToTreeless :: IsMain -> Definition -> TCM (Maybe TreelessDefinition)
definitionToTreeless isMainModule Defn{defName, defType, theDef=Function{}} =
    (fmap . fmap)
      (mkTreelessDef . normalizeLets . simplifyApp . skipLambdas)
      (mapMM normalizeNames (toTreeless LazyEvaluation defName))
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

-- | Simplify complicated applications
--
-- f a (g b) (h c)
-- >>>
-- let b' = g b in
-- let c' = h c in
-- f a b' c'
-- simplifyApp :: TTerm -> TTerm
-- simplifyApp (tAppView -> (f, splitArgs -> Just (before, TCon q, after))) =
--   simplifyApp (TLet (TCon q) app)
--   where
--   app = mkTApp (raise 1 f) $ raise 1 before ++ TVar 0 : raise 1 after
--
-- simplifyApp (tAppView -> (f, splitArgs -> Just (before, TLit lit, after))) =
--   simplifyApp (TLet (TLit lit) app)
--   where
--   app = mkTApp (raise 1 f) $ raise 1 before ++ TVar 0 : raise 1 after

normalizeLets :: TTerm -> TTerm
normalizeLets (TLet (TLet t1 t2) t3) = 
  normalizeLets $ TLet t1 $ TLet t2 $ raiseFrom 1 1 t3

normalizeLets (TLet t1 t2) = on TLet normalizeLets t1 t2
normalizeLets (TLam t) = TLam (normalizeLets t)
normalizeLets (TCase n info t alts) = TCase n info (normalizeLets t) (map step alts)
  where
  step (TACon q a t) = TACon q a (normalizeLets t)
  step (TALit l t) = TALit l (normalizeLets t)
  step _ = __IMPOSSIBLE__

normalizeLets t = t



simplifyApp (tAppView -> (f, splitArgs -> Just (before, t, after))) =
  simplifyApp (foldr TLet app ts)
  where
  app = mkTApp (raiseN f) $ raiseN before ++ TVar 0 : raiseN after
  ts = uncurry snoc (tLetView (simplifyApp t))
  raiseN :: Subst a => a -> a
  raiseN = raise (length ts)

simplifyApp t = case t of
  TLet t1 t2            -> on TLet simplifyApp t1 t2
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
  go _                = error "TODO"


-- Split arguments on the first application or constructor
splitArgs :: Args -> Maybe (Args, TTerm, Args)
splitArgs []              = Nothing
splitArgs (a@TApp{} : as) = Just ([], a, as)
splitArgs (a@TCon{} : as) = Just ([], a, as)
splitArgs (a@TLit{} : as) = Just ([], a, as)
splitArgs (a : as)
  | Just (before, t, after) <- splitArgs as = Just (a:before, t, after)
  | otherwise = Nothing
