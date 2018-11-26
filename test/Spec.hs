import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import P01(myLast)

main :: IO ()
main = hspec $ do
  describe "myLast" $ do
    it "return the last element of a list" $ do
      myLast [1,2] `shouldBe` (2 :: Int)

    it "return the last element of an *arbitrary* list" $
      property $ \x xs -> myLast(xs++[x]) == (x :: Int)

  it "throws an exception if used with an empty list" $ do
      evaluate (myLast []) `shouldThrow` anyException
