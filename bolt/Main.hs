module Main where

import Control.Monad
import Data.List
import Debug.Trace (trace)

readInteger :: IO Integer
readInteger = read <$> getLine

readDecimals :: IO [Double]
readDecimals = map read . words <$> getLine

round' f n = fromInteger (round $ f * (10^n)) / (10.0^^n)

solve :: [Double] -> Bool
solve fs = round' (100 / product fs) 2 < round' 9.5800 2

testCase :: IO ()
testCase = do
  factors <- readDecimals
  putStrLn $ if solve factors then "YES" else "NO"

main :: IO ()
main = do
  cases <- readInteger
  replicateM_ (fromInteger cases) testCase
