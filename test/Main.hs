{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.State           (MonadState, State, evalState,
                                                get, modify)
import qualified Data.ByteString               as BS
import           Data.Foldable                 (toList)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8, encodeUtf8)
import           System.FilePath               ((<.>), (</>))
import           Test.Tasty
import           Test.Tasty.Silver.Advanced

import           Agda.Compiler.Backend         hiding (Prim)
import           Agda.Llvm.Grin
import           Agda.Llvm.GrinTransformations
import           Agda.Llvm.Utils
import           Agda.Syntax.Common.Pretty
import           Agda.Syntax.Literal
import           Agda.TypeChecking.Substitute
import           Agda.Utils.List1              (pattern (:|), (<|))


main :: IO ()
main = defaultMain goldenTests

goldenTests :: TestTree
goldenTests =
  testGroup "Golden" $ map (uncurry mkSucceedTest) $
    ("normalise", testNormalise)               :
    ("rightHoistFetch1", testRightHoistFetch1) :
    ("rightHoistFetch2", testRightHoistFetch2) :
    ("useRegister", testUseRegister)           :
    ("useRegisters1", testUseRegisters1)       :
    ("useRegisters2", testUseRegisters2)       :
    ("useRegisters3", testUseRegisters3)       :
    ("introduceRegisters1", testIntroduceRegisters1)       :
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
    ( store (ConstantNode natTag $ mkLit 1 :| []) `bindVarM`
      store (ConstantNode downFromTag $ Var 0 :| []) `bindVarL`
      Unit (Var 0)
    ) `bindVarL`
    Unit (ConstantNode consTag $ Var 1 <| Var 0 :| [])


testRightHoistFetch1 :: Term
testRightHoistFetch1 = evalTest $ rightHoistFetch =<< inputRightHoistFetch1

-- fetch 0 [0] ; λ x1 →
-- fetch 1 [1] ; λ x0 →
-- case 1 of
--   Fsum → sum 0
--   Cnat → unit (Cnat 3)
inputRightHoistFetch1 :: Test Term
inputRightHoistFetch1 =
    FetchOffset 0 0 `bindVarR`
    FetchOffset 1 1 `bindVar`
    Case (Var 1) unreachable
      [ CAltTag sumTag (App (Def "sum") [Var 0])
      , CAltTag natTag (Unit (ConstantNode natTag $ Var 3 :| []))
      ]

testRightHoistFetch2 :: Term
testRightHoistFetch2 = evalTest $ rightHoistFetch =<< inputRightHoistFetch2

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
inputRightHoistFetch2 :: Test Term
inputRightHoistFetch2 =
    FetchOffset 0 0 `bindVarR`
    FetchOffset 1 1 `bindVarR`
    FetchOffset 2 2 `bindVarR`
    caseOf1 `bindCnatM`
    UpdateTag natTag 4 (ConstantNode natTag $ Var 0 :| []) `bindEmptyR`
    caseOf2
  where
    infixr 2 `bindCnatM`
    bindCnatM t1 t2 = Bind <$> t1 <*> (laltConstantNode natTag =<< t2)

    caseOf1 :: Test Term
    caseOf1 = do
      t <- App (Prim PSub) [Var 1, Var 0] `bindVar`
           Unit (ConstantNode natTag $ Var 0 :| [])

      pure $
        Case (Var 2) unreachable
          [ CAltTag primSubTag t
          , CAltTag natTag (Unit (ConstantNode natTag $ Var 1 :| []))
          ]

    caseOf2 :: Test Term
    caseOf2 = do
      t <- store (ConstantNode natTag $ mkLit 1 :| []) `bindVarM`
           store (ConstantNode primSubTag $ Var 5 <| Var 0 :| []) `bindVarM`
           store (ConstantNode downFromTag $ Var 0 :| []) `bindVarL`
           Unit (ConstantNode consTag $ Var 1 <| Var 0 :| [])

      pure $ Case (Var 0) t [CAltLit (LitNat 0) $ Unit (Tag nilTag)]


testUseRegister :: Term
testUseRegister =
  evalTest $ useRegister (ConstantNode natTag $ Var 0 :| []) (UpdateTag natTag 2)

testUseRegisters1 :: Term
testUseRegisters1 =
  evalTest $ useRegisters (mkLit 4 <| mkLit 2 :| []) (App (Def "Prim.Sub") . toList)

testUseRegisters2 :: Term
testUseRegisters2 =
  evalTest $ do
    alt <- laltVar $
      Unit (Var 0) `BindEmpty`
      Unit (Var 1) `BindEmpty`
      Unit (Var 2) `BindEmpty`
      Unit (Var 3)
    useRegisters (mkLit 4 <| mkLit 2 :| []) (\vs -> App (Def "Prim.Sub") (toList vs) `Bind` alt)

testUseRegisters3 :: Term
testUseRegisters3 =
  evalTest $ do
    alt <- laltVar $
      Unit (Var 0) `BindEmpty`
      Unit (Var 1) `BindEmpty`
      Unit (Var 2) `BindEmpty`
      Unit (Var 3)
    useRegisters (mkLit 4 <| Var 0 <| mkLit 2 <| Var 3 :| []) (\vs -> App (Def "foo") (toList vs) `Bind` alt)

testIntroduceRegisters1 :: Term
testIntroduceRegisters1 = evalTest $ introduceRegisters inputIntroduceRegisters1

inputIntroduceRegisters1 :: Term
inputIntroduceRegisters1 =
  UpdateTag natTag 1 (ConstantNode natTag $ Var 0 :| []) `BindEmpty`
  Unit (Var 4)


test :: Term
test = raiseFrom 1 10 $ evalTest $ Unit (Var 0) `bindVar` Unit (Var 1)

testSwap01 :: Term
testSwap01 =
  swap01' $
    Unit (Var 1) `BindEmpty`
    Unit (Var 0)

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
cnil = Tag nilTag

downFromTag :: Tag
downFromTag = FTag{tDef = "downFrom", tArity = 1}

sumTag :: Tag
sumTag = FTag{tDef = "sum", tArity = 1}

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
