module Main where

import Numeric.Natural
import Prelude hiding (even, odd)

-- printed
x :: Int
x = 5

onlyOne :: [Int]
onlyOne = 1 : onlyOne

f :: Int -> Int
f x = f x

-- not printed
fixObvious :: (a -> a) -> a
fixObvious f = f (fixObvious f)

-- printed
fixSubtle :: (a -> a) -> a
fixSubtle f = let x = f x in x

-- neither printed
even, odd :: Natural -> Bool
even 0 = True
even n = odd (n - 1)
odd 0 = False
odd n = even (n - 1)

emptyList = []

twoOne :: [Int]
twoOne = do
  let twoTwo = [2, 2]
  twoTwo ++ onlyOne

-- class Printable a where
--   printVal :: a -> String

-- instance Printable Int where
--   printVal x = "Int: " ++ show x

-- instance Printable Bool where
--   printVal True = "Bool: True"
--   printVal False = "Bool: False"

-- printed
main :: IO ()
main = do
  -- putStrLn $ printVal (42 :: Int)
  -- putStrLn $ printVal True
  return ()
