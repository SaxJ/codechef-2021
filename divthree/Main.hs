module Main where

import Control.Monad

readInteger :: IO Integer
readInteger = read <$> getLine

readInt :: IO Int
readInt = read <$> getLine

readIntegers :: IO [Integer]
readIntegers = map read <$> words <$> getLine

solve :: Integer -> Integer -> [Integer] -> Integer
solve k d as = min d x
  where
    x = sum as `div` k

runCase :: IO ()
runCase = do
  [n, k, d] <- readIntegers
  as <- readIntegers
  print $ solve k d as

main :: IO ()
main = do
  cases <- readInt
  replicateM_ cases runCase
