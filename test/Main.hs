{-# LANGUAGE OverloadedStrings #-}
import           Agda.Compiler.Backend

import           Control.Monad.State        (MonadState, State, evalState, get,
                                             modify)
import qualified Data.ByteString            as BS
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)

import           System.FilePath            ((<.>), (</>))
-- import           System.PosixCompat.Files   (touchFile)
import           Test.Tasty
import           Test.Tasty.Silver.Advanced

import           Agda.Llvm.Compiler
import           Agda.Llvm.Grin
import           Agda.Syntax.Literal
import           Agda.Utils.Pretty



main :: IO ()
main = defaultMain unitTests


unitTests :: TestTree
unitTests =
  goldenTestIO1
    "Normalise"
    readGolden
    (pure $ prettyText testNormalise)
    (textDiffWithWrite $ baseName <.> ".err")
    (pure . ShowText)
    Nothing
  where
    baseName = "test" </> "Normalise"

    readGolden :: IO (Maybe Text)
    readGolden = readTextFileMaybe $ baseName <.> ".golden"





newtype Test a = Test{runTest :: State Int a}
  deriving (Functor, Applicative, Monad, MonadState Int)

instance MonadFresh Int Test where
  fresh = get <* modify succ

evalTest :: Test a -> a
evalTest f = evalState (runTest f) 0

-- store (Cnil) λ x0 →
-- ( store (Cnat #1) ; λ x1 →
--   store (FdownFrom 0) ; λ x2 →
--   unit 0
-- ) ; λ x3 →
-- unit (Ccons 1 0)
-- >>>
-- store (Cnil) λ x0 →
-- store (Cnat #1) ; λ x1 →
-- store (FdownFrom 0) ; λ x2 →
-- unit 0 ; λ x3 →
-- unit (Ccons 3 0)
testNormalise :: Term
testNormalise = normalise $ evalTest input'
  where

    input' :: Test Term
    input' =
      store cnil `bindVarM`
      ( store (Node natTag [mkLit 1]) `bindVarM`
        store (Node downFromTag [Var 0]) `bindVarL`
        Unit (Var 0)
      ) `bindVarL`
      Unit (Node consTag [Var 1, Var 0])

    output :: Test Term
    output =
      store cnil `bindVarM`
      store (Node natTag [mkLit 1]) `bindVarM`
      store (Node downFromTag [Var 0]) `bindVarM`
      Unit (Var 0) `bindVar`
      Unit (Node consTag [Var 1, Var 0])

    cnil = Node nilTag []
    downFromTag = FTag{tDef = "downFrom", tArity = 1}
    nilTag = CTag {tCon = "nil", tArity = 0}
    consTag = CTag{tCon = "cons", tArity = 2}

    mkLoc :: Int -> Loc
    mkLoc = MkLoc . Gid

    mkAbs :: Int -> Abs
    mkAbs = MkAbs . Gid

    mkLit :: Integer -> Term
    mkLit = Lit . LitNat


prettyText :: Pretty a => a -> Text
prettyText = T.pack . prettyShow

readTextFileMaybe :: FilePath -> IO (Maybe Text)
readTextFileMaybe f = fmap decodeUtf8 <$> readFileMaybe f

writeTextFile :: FilePath -> Text -> IO ()
writeTextFile f = BS.writeFile f . encodeUtf8

textDiffWithWrite :: FilePath -> Text -> Text -> IO GDiff
textDiffWithWrite file t1 t2
    | T.words t1 == T.words t2 = return Equal
    | otherwise = do
        writeTextFile file t2
        pure $ DiffText Nothing t1 t2
