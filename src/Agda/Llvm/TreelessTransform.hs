{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Agda.Llvm.TreelessTransform
  ( definitionToTreeless
  , TreelessDefinition(..)
  ) where

import           Data.Function                         (on)

import           Agda.Compiler.Backend                 hiding (Prim, initEnv)
import           Agda.Compiler.Treeless.NormalizeNames (normalizeNames)
import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.Internal                  (Type)
import           Agda.TypeChecking.Substitute
import           Agda.Utils.Impossible
import           Agda.Utils.Maybe

data TreelessDefinition = TreelessDefinition
  { tl_name     :: String
  , tl_isMain   :: Bool
  , tl_arity    :: Int
  , tl_type     :: Type
  , tl_termOrig :: TTerm
  , tl_term     :: TTerm
  }

instance Pretty TreelessDefinition where
  pretty TreelessDefinition{tl_name, tl_arity, tl_type, tl_term} =
    vcat
      [ pretty tl_name <+> text ":" <+> pretty tl_type
      , pretty tl_name <+> prettyList_ (take tl_arity ['a' ..]) <+>
        text "=" <+> pretty tl_term
      ]

definitionToTreeless :: IsMain -> Definition -> TCM (Maybe TreelessDefinition)
definitionToTreeless isMainModule Defn{defName, defType, theDef=Function{}} = do
    term <- maybeM __IMPOSSIBLE__ normalizeNames $
            toTreeless LazyEvaluation defName
    let (arity, term') = skipLambdas term
        term'' = simplifyApp term'
    pure $ Just $ TreelessDefinition
      { tl_name     = prettyShow defName
      , tl_isMain   = isMain
      , tl_arity    = arity
      , tl_type     = defType
      , tl_termOrig = term
      , tl_term     = term''
      }
  where
    isMain =
      isMainModule == IsMain &&
      "main" == (prettyShow . nameConcrete . qnameName) defName

definitionToTreeless _ _ = pure Nothing


-- | Skip initial lambdas
skipLambdas :: TTerm -> (Int, TTerm)
skipLambdas = go . (0, ) where
  go (n, TLam t) = go (n + 1, t)
  go p           = p

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
