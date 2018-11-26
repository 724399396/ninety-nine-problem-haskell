import           Control.Exception (evaluate)
import           P01               (myLast)
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "myLast" $ do
    it "return the last element of a list" $
      myLast [1,2] `shouldBe` (2 :: Int)

    it "return the last element of an *arbitrary* list" $
      property $ \x xs -> myLast(xs++[x]) == (x :: Int)

  it "throws an exception if used with an empty list" $
      evaluate (myLast []) `shouldThrow` anyException
