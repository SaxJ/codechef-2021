module Main where

import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace (trace)


readInteger :: IO Integer
readInteger = read <$> getLine

readInt :: IO Int
readInt = read <$> getLine

readIntegers :: IO [Integer]
readIntegers = map read <$> words <$> getLine

makePairs :: [Integer] -> [Integer] -> [(Integer, Integer)]
makePairs as bs = zip (sort as) (reverse $ sort bs)

countSwaps :: Integer -> [(Integer, Integer)] -> Maybe Integer
countSwaps _ [] = Nothing
countSwaps goal pairs = x
  where
    loop (a, i, m) [] = (i, i, m)
    loop (acc, i, m) ((a, b) : ps)
      | (b - a) <= 0 = (i, i, m)
      | otherwise = loop (acc + 2 * (b - a), i + 1, if isNothing m && total > goal then Just (i + 1) else m) ps
      where
        total = acc + 2 * (b - a)
    (_, _, x) = loop (0, 0, Nothing) pairs

solve :: Integer -> [Integer] -> [Integer] -> Integer
solve goal as bs
  | goal == 0 = -1
  | goal < 0 = 0
  | otherwise = case countSwaps goal $ makePairs as bs of
    Nothing -> -1
    Just x -> x

runCase :: IO ()
runCase = do
  _ <- readInt -- don't care about this input
  as <- readIntegers
  bs <- readIntegers
  let goal = sum bs - sum as
  print $ solve goal as bs

main :: IO ()
main = do
  cases <- readInt
  replicateM_ cases runCase
