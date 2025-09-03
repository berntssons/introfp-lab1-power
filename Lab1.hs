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
--  ...
-- => { ... }
-- ...
-- => { ... }
-- ...
-- => { ... }
-- ...
-- => { ... }
-- ...
-- => { ... }
-- 9

-- Part B ----------------------------------------------------------------------

power1 = undefined


-- Part C ----------------------------------------------------------------------

power2 = undefined


-- Part D ----------------------------------------------------------------------

-- Part D.1 - describe your test cases here

test1 = power1 2 3 == power 2 3
test2 = undefined -- replace with your own test case
test3 = undefined -- replace with your own test case
test4 = undefined -- replace with your own test case
test5 = undefined -- replace with your own test case

-- Part D.2
comparePower1 :: Int -> Int -> Bool
comparePower1 n k = undefined

comparePower2 :: Int -> Int -> Bool
comparePower2 n k = undefined

-- Part D.3
testAll :: Bool
testAll = undefined
