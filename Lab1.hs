{- |
Module      : Lab1
Description : Skeleton for lab 1: Power to the People
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : Susanna Berntsson, Eeric Brask, Max Jenslöv
Lab group   : 70
-}

module Lab1 where

import MeasureTime

-- The power function uses explicit recursion to calculate n^k. We developed
-- this function during a lecture.
power :: Int -> Int -> Int
power n k
  | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- Part A ----------------------------------------------------------------------

-- Evaluate the expression `power 3 2` by hand. Write the evaluation steps in
-- the comment below by replacing the dots. We have already done the first step
-- for you.

-- power 3 2
-- => { definition of power, third case }
-- 3 * power 3 (2 - 1)
-- => { apply subtraction }
--  3 * power 3 1
-- => { definition of power, third case }
-- 3 * 3 * power 3 (1 - 1)
-- => { apply subtraction }
-- 3 * 3 * power 3 0
-- => { definition of power, second case }
-- 3 * 3 * 1
-- => { apply multiplication }
-- 9 * 1
-- => { apply multiplication }
-- 9

-- Part B ----------------------------------------------------------------------

power1 :: Int -> Int -> Int

-- Case for negative exponents:
power1 n k
  | k < 0 = error "power: negative argument"
-- Case for k >= 0:
power1 n k = product (replicate k n)
-- Separate case for k = 0 not needed since product [] returns 1

-- Part C ----------------------------------------------------------------------

power2 :: Int -> Int -> Int

power2 n k
  | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k
  | even k = power2 (n * n) (k `div` 2)
  | otherwise = n * power2 n (k - 1)

-- Part D ----------------------------------------------------------------------

-- Part D.1 - describe your test cases here

-- Test positive odd exponent
test1 = power 2 3 == power1 2 3 && power 2 3 == power2 2 3

-- Test positive even exponent
test2 = power 3 4 == power1 3 4 && power 3 4 == power2 3 4

-- Test exponent is 0
test3 = power 3 0 == power1 3 0 && power 3 0 == power2 3 0

-- Test base is 0
test4 = power 0 5 == power1 0 5 && power 0 5 == power2 0 5

-- Test negative base
test5 = power (-5) 3 == power1 (-5) 3 && power (-5) 3 == power2 (-5) 3

-- Part D.2

comparePower1 :: Int -> Int -> Bool
comparePower1 n k = power n k == power1 n k

comparePower2 :: Int -> Int -> Bool
comparePower2 n k = power n k == power2 n k

-- Part D.3
testValues :: [(Int, Int)]
testValues = [(2, 3), (3, 4), (3, 0), (0, 5), ((-5), 3)]

testAll :: Bool
testAll = and [ comparePower1 n k && comparePower2 n k | x <- testValues, let n = fst x, let k = snd x ]

-- Part E

