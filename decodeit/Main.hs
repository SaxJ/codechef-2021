module Main where

import Control.Monad
import Data.List
import Debug.Trace (trace)

readInteger :: IO Integer
readInteger = read <$> getLine

readInt :: IO Int
readInt = read <$> getLine

readIntegers :: IO [Integer]
readIntegers = map read <$> words <$> getLine

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n l
  | n > 0 = take n l : groupsOf n (drop n l)
  | otherwise = error "Negative or zero n"

findLetter :: [Char] -> Char
findLetter code = letters !! x
  where
    letters = ['a' .. 'p']
    fun (l, u) c
      | c == '0' = (l, (l + u) `div` 2)
      | otherwise = ((l + u) `div` 2, u)
    x = snd $ foldl' fun (0, length letters - 1) code

decode :: String -> String
decode = map findLetter . groupsOf 4

runCase :: IO ()
runCase = do
  _ <- readInt -- don't care about this input
  encoded <- getLine
  putStrLn $ decode encoded

main :: IO ()
main = do
  cases <- readInt
  replicateM_ cases runCase
