{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.State        (MonadState, State, evalState, get,
                                             modify)
import qualified Data.ByteString            as BS
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)

import           System.FilePath            ((<.>), (</>))
import           Test.Tasty
import           Test.Tasty.Silver.Advanced

import           Agda.Compiler.Backend      hiding (Prim)
import           Agda.Llvm.Compiler
import           Agda.Llvm.Grin
import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.Literal

main :: IO ()
main = defaultMain goldenTests

goldenTests :: TestTree
goldenTests =
  testGroup "Golden" $ map (uncurry mkSucceedTest) $
    ("normalise", testNormalise)             :
    ("rightHoistFetch", testRightHoistFetch) :
    []

testNormalise :: Term
testNormalise = normalise inputNormalise

-- storel0 (Cnil) ; λ x6 →
-- storel1 (Cnat #1) ; λ x4 →
-- storel2 (FdownFrom 0) ; λ x3 →
-- unit 0 ; λ x5 →
-- unit (Ccons 3 0)
inputNormalise :: Term
inputNormalise =
  evalTest $
    store cnil `bindVarM`
    ( store (ConstantNode natTag [mkLit 1]) `bindVarM`
      store (ConstantNode downFromTag [Var 0]) `bindVarL`
      Unit (Var 0)
    ) `bindVarL`
    Unit (ConstantNode consTag [Var 1, Var 0])

testRightHoistFetch :: Term
testRightHoistFetch = evalTest $ rightHoistFetch =<< inputRightHoistFetch

-- fetch 0 [0] ; λ x10 →
-- fetch 1 [1] ; λ x9 →
-- fetch 2 [2] ; λ x8 →
-- (case 2 of
--    FPrim.Sub →
--      PSub 1 0 ; λ x0 →
--      unit (Cnat 0)
--    Cnat → unit (Cnat 1)
-- ) ; λ Cnat x7 →
-- updateCnat 4 (Cnat 0) ; λ () →
-- case 0 of
--   0 → unit Cnil
--   _ → storel1 (Cnat #1) ; λ x6 →
--       storel2 (FPrim.Sub 5 0) ; λ x5 →
--       storel3 (FdownFrom 0) ; λ x4 →
--       unit (Ccons 1 0)
inputRightHoistFetch :: Test Term
inputRightHoistFetch =
    FetchOffset 0 0 `bindVarR`
    FetchOffset 1 1 `bindVarR`
    FetchOffset 2 2 `bindVarR`
    caseOf1 `bindCnatM`
    UpdateTag natTag 4 (ConstantNode natTag [Var 0]) `bindEmptyR`
    caseOf2
  where
    infixr 2 `bindCnatM`
    bindCnatM t1 t2 = Bind <$> t1 <*> (laltConstantNode natTag =<< t2)

    caseOf1 :: Test Term
    caseOf1 = do
      t <- App (Prim PSub) [Var 1, Var 0] `bindVar`
        Unit (ConstantNode natTag [Var 0])

      pure $
        Case (Var 2) unreachable
          [ CAltTag primSubTag t
          , CAltTag natTag (Unit (ConstantNode natTag [Var 1]))
          ]

    caseOf2 :: Test Term
    caseOf2 = do
      t <- store (ConstantNode natTag [mkLit 1]) `bindVarM`
           store (ConstantNode primSubTag [Var 5, Var 0]) `bindVarM`
           store (ConstantNode downFromTag [Var 0]) `bindVarL`
           Unit (ConstantNode consTag [Var 1, Var 0])

      pure $ Case (Var 0) t [CAltLit (LitNat 0) $ Unit (Tag nilTag)]

mkSucceedTest :: Pretty a => FilePath -> a -> TestTree
mkSucceedTest fileName test =
  goldenTestIO1
    fileName
    readGolden
    (pure $ prettyText test)
    (textDiffWithWrite $ baseName <.> ".err")
    (pure . ShowText)
    Nothing
  where
    baseName = "test" </> fileName

    readGolden :: IO (Maybe Text)
    readGolden = readTextFileMaybe $ baseName <.> ".golden"

evalTest :: Test a -> a
evalTest f = evalState (runTest f) 0

newtype Test a = Test{runTest :: State Int a}
  deriving (Functor, Applicative, Monad, MonadState Int)

instance MonadFresh Int Test where
  fresh = get <* modify succ

cnil :: Val
cnil = ConstantNode nilTag []

downFromTag :: Tag
downFromTag = FTag{tDef = "downFrom", tArity = 1}

primSubTag :: Tag
primSubTag = FTag{tDef = "Prim.Sub", tArity = 2}

nilTag :: Tag
nilTag = CTag {tCon = "nil", tArity = 0}

consTag :: Tag
consTag = CTag{tCon = "cons", tArity = 2}

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
