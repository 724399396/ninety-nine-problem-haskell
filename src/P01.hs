module P01 (
  myLast
           ) where

myLast :: [a] -> a
myLast []       = error "myLast: empty list"
myLast [x]      = x
myLast (_ : xs) = myLast xs
