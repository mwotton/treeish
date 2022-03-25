{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.Hspec.QuickCheck(prop,modifyArgs)
import Test.QuickCheck(Arbitrary(..),oneof,listOf,Args(..))
import qualified Test.QuickCheck as QC
import Treeish
import Data.Text(pack,Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec spec

niceText = pack <$> listOf QC.arbitraryASCIIChar
-- niceText = pack . pure <$> (QC.elements ['a' .. 'z'])
--   QC.arbitraryASCIIChar

--niceText = QC.elements ["abc","def","ghci"]

instance Arbitrary TreeVal where
  arbitrary = oneof [VBool <$> arbitrary
                    ,VText <$> niceText
                    ,VDouble <$> arbitrary
                    ,VDict <$> QC.sized genDict
                    ,VArray <$> QC.scale (`div` 2) (listOf arbitrary)
                    ]

genDict size =
  Map.fromList <$>
    listOf ((,) <$> niceText <*> QC.resize (size `div` 2) arbitrary)

data NotSame a = NotSame a a
  deriving Show
instance (Eq a, Arbitrary a) => Arbitrary (NotSame a) where
  arbitrary = do
    first <- arbitrary
    second <- arbitrary `QC.suchThat` (/= first)
    pure $ NotSame first second


spec :: Spec
spec = modifyArgs (\r -> r {maxSuccess=1000}) $
  describe "treeish" $ do
  it "works for zero as special case" $ do
    let d = (VDouble 0.0 `diff` VDouble 0.0)
    collapse d `shouldBe` Portion 1.0
  it "handles empty dicts" $ do
    let d =  (VDict mempty `diff` VDict mempty)
    (d,collapse d) `shouldBe` (d,Portion 1.0)

  prop "always identical to itself" $ \x ->
    let r = collapse (x `diff` x)
    in
       r == Portion 1.0

  prop "diff never goes above 1 or below 0" $ \(x,y) ->
    let z = unportion (collapse (x `diff` y))
    in z <= 1.0 && z >= 0.0

  prop "is not identical to anything but itself" $ \(NotSame x y) ->
    let z = unportion (collapse (x `diff` y))
    in z < 1.0
