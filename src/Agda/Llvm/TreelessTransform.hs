{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Agda.Llvm.TreelessTransform
  ( definitionToTreeless
  , TreelessDefinition(..)
  ) where

import           Control.Monad.Cont                    (MonadIO (liftIO), when,
                                                        zipWithM)
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
import           Agda.Utils.List                       (downFrom, lastMaybe)
import           Agda.Utils.Maybe
import           Agda.Utils.Monad                      (mapMM)
import           Data.Bitraversable                    (bimapM)
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
      (mkTreelessDef . simplifyApp . skipLambdas)
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
simplifyApp :: TTerm -> TTerm
simplifyApp t | (f, as@(_:_)) <- tAppView t  = go f as where
  -- Create a let expression and raise de Bruijn indices, until
  -- simple application
  go f as
    | Just (before, t, after) <- splitArgs as =
      simplifyApp $ mkLet t
                  $ mkTApp (raise1 f)
                  $ raise1 before ++ TVar 0 : raise1 after
    | otherwise = mkTApp f as

  raise1 :: Subst a => a -> a
  raise1 = applySubst $ raiseS 1

  -- Split arguments on the first application
  splitArgs :: Args -> Maybe (Args, TTerm, Args)
  splitArgs []              = Nothing
  splitArgs (a@TApp{} : as) = Just ([], a, as)
  splitArgs (a : as)
    | Just (before, t, after) <- splitArgs as = Just (a:before, t, after)
    | otherwise = Nothing

simplifyApp t = case t of
  TCase n info def alts -> TCase n info (simplifyApp def) (simplifyAppAlts alts)
  TLet t1 t2            -> on TLet simplifyApp t1 t2
  TLit _                -> t
  TCon _                -> t
  TError _              -> t
  TVar _                -> t
  TPrim _               -> t
  TDef _                -> t
  TLam t                -> TLam $ simplifyApp t
  _                     -> error $ "TODO " ++ show t

simplifyAppAlts :: [TAlt] -> [TAlt]
simplifyAppAlts = map go where
  go alt@TACon{aBody} = alt{aBody = simplifyApp aBody}
  go alt@TALit{aBody} = alt{aBody = simplifyApp aBody}
  go _                = error "TODO"
