import           Control.Exception (evaluate)
import           Lib
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

  describe "myButLast" $ do
    it "return the last but one element of list" $
      myButLast [1,2,3,4] `shouldBe` (3 :: Int)

    it "return the last but one element of an *arbitrary* list" $
      property $ \x x' xs -> myButLast(xs++[x,x']) == (x :: Int)

    it "throws an exception if used with an empty list" $
      evaluate (myButLast []) `shouldThrow` anyException

    it "throws an exception if used with a size 1 list" $
      evaluate (myButLast [1]) `shouldThrow` anyException

  describe "elementAt" $ do
    it "return element at location" $
      elementAt [1,2,3] 2 `shouldBe` (2 :: Int)

    it "throws an exception if index is negative" $
      evaluate (elementAt [] (-1)) `shouldThrow` anyException

    it "throws an exception if index too large" $
      evaluate (elementAt [1,2] 3) `shouldThrow` anyException

  describe "myLength" $ do
    it "should get length of list" $
      myLength [123, 456, 789] `shouldBe` (3 :: Int)

    it "should same as Prelude.length" $
      property $ \xs -> myLength (xs :: [Int]) == length xs
