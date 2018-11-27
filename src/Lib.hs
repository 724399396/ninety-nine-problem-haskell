module Lib  where

-- P01
myLast :: [a] -> a
myLast []       = error "myLast: empty list"
myLast [x]      = x
myLast (_ : xs) = myLast xs

-- P02
myButLast :: [a] -> a
myButLast []  = error "myButLast: empty list"
myButLast [_] = error "myButLast: size 1 list"
myButLast [x, _] = x
myButLast (_ : xs) = myButLast xs

-- P03
elementAt :: [a] -> Int -> a
elementAt [] _ = error "index out of range"
elementAt _ i  | i <= 0 = error "index out of range"
elementAt xs 1 = head xs
elementAt xs i = elementAt (tail xs) (i - 1)

-- P04
myLength :: [a] -> Int
myLength = foldl (\acc _ -> acc + 1) 0
