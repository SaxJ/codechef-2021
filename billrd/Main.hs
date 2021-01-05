module Main where

import Control.Monad
import Data.Array
import Text.Printf (printf)

type Velocity = (Integer, Integer)

type Position = (Integer, Integer)

type Ball = (Position, Velocity)

readInteger :: IO Integer
readInteger = read <$> getLine

readInt :: IO Int
readInt = read <$> getLine

readIntegers :: IO [Integer]
readIntegers = map read <$> words <$> getLine

isCorner :: Integer -> Ball -> Bool
isCorner n ((x, y), _)
  | x == 0 && y == 0 = True
  | x == n && y == 0 = True
  | x == 0 && y == n = True
  | x == n && y == n = True
  | otherwise = False

nextVelocity :: Integer -> Ball -> Velocity
nextVelocity _ ((0, 0), _) = (0, 0)
nextVelocity n ((x, y), (vx, vy))
  | x == 0 && y == n = (0, 0)
  | x == n && y == 0 = (0, 0)
  | x == n && y == n = (0, 0)
  | otherwise = (nvx, nvy)
  where
    nvx = if x == 0 || x == n then - vx else vx
    nvy = if y == 0 || y == n then - vy else vy

findEdge :: Integer -> Ball -> Ball
findEdge n ((x, y), (vx, vy)) = ((ex, ey), (nvx, nvy))
  where
    gx = if vx < 0 then x else n - x
    gy = if vy < 0 then y else n - y
    mult = min gx gy
    (ex, ey) = (x + mult * vx, y + mult * vy)
    (nvx, nvy) = nextVelocity n ((ex, ey), (vx, vy))

bounceSequence :: Integer -> Position -> [Ball]
bounceSequence n p = iterate (findEdge n) (p, (1, 1))

solve :: Integer -> Integer -> Integer -> Integer -> Position
solve n k x y
  | x == 0 && y == 0 = (0, 0)
  | x == y = (n, n)
  | x == n && y == 0 = (n, 0)
  | x == 0 && y == n = (0, n)
  | otherwise = (px, py)
  where
    bounces = listArray (0, 5) $ take 5 $ bounceSequence n (x, y)
    ((px, py), _) = bounces ! (((fromInteger k - 1) `mod` 4) + 1)

runCase :: IO ()
runCase = do
  [n, k, x, y] <- readIntegers
  let (_ : bounces) = take 5 $ bounceSequence n (x, y)
  let (px, py) = solve n k x y
  putStrLn $ printf "%d %d" px py

main :: IO ()
main = do
  cases <- readInt
  replicateM_ cases runCase
