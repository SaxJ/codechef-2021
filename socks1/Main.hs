module Main where

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Set as Set

readIntegers :: IO [Integer]
readIntegers = map read <$> words <$> getLine

hasPair :: [Integer] -> Bool
hasPair socks = Set.size sockSet /= length socks
  where
    sockSet = Set.fromList socks

main :: IO ()
main = do
  socks <- readIntegers
  putStrLn $ if hasPair socks then "YES" else "NO"
