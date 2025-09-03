{- |
Module      : Lab1
Description : Skeleton for lab 1: Power to the People
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : <list your names here>
Lab group   : <group number>
-}

module Lab1 where

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

-- Using prelude functions
-- power2 n k = if even k then (n * n)^(k `div` 2) else n * (n^(k - 1))

-- Recursive
power2 n k
  | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k = if even k then power2 (n * n) (k `div` 2) else n * power2 n (k - 1)

-- Part D ----------------------------------------------------------------------

-- Part D.1 - describe your test cases here

hasSameResult :: (Int -> Int -> Int) -> (Int -> Int -> Int) -> Int -> Int -> Bool
hasSameResult f g n k = f n k == g n k

matchesPower :: (Int -> Int -> Int) -> Int -> Int -> Bool
matchesPower f n k = hasSameResult power f n k

testBothPowers :: Int -> Int -> Bool
testBothPowers n k = matchesPower power1 n k && matchesPower power2 n k

test1 = testBothPowers 2 4  -- Test positive even exponent
test2 = testBothPowers 3 3 -- Test positive odd exponent
test3 = testBothPowers 4 0 -- Test exponent is 0
test4 = testBothPowers (-2) 3 -- Test negative base and odd exponent

-- Test negative exponent throws error
test5 = power1 4 (-3)
test6 = power2 4 (-3)

-- Part D.2
comparePower1 :: Int -> Int -> Bool
comparePower1 n k = matchesPower power1 n k

comparePower2 :: Int -> Int -> Bool
comparePower2 n k = matchesPower power2 n k

-- Part D.3
testAll :: Bool
testAll = undefined
