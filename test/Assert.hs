module Assert ((@?<=), (@?∈), (@?∋), (@?~)) where

import CompReal
import Generators

import Data.CallStack
import Control.Monad (unless)
import Test.Tasty.HUnit

assertLEQ :: (Ord a, Show a, HasCallStack) => String -> a -> a -> Assertion
assertLEQ preface maxVal x = unless (x <= maxVal) (assertFailure msg)
    where msg = (if null preface then "" else preface ++ "\n") ++
                "expected at most: " ++ show maxVal ++ "\n but got: " ++ show x

(@?<=) :: (Ord a, Show a, HasCallStack) => a -> a -> Assertion
(@?<=) = flip (assertLEQ "")

assertIn :: (Ord a, Show a, HasCallStack) => String -> (a,a) -> a -> Assertion
assertIn preface (l,r) x = unless (l <= x && x <= r) (assertFailure msg)
    where msg = (if null preface then "" else preface ++ "\n") ++
                "expected value in range: " ++ show (l,r) ++ "\n but got: " ++ show x

(@?∈) :: (Ord a, Show a, HasCallStack) => a -> (a,a) -> Assertion
(@?∈) = flip (assertIn "")

assertContains :: (Ord a, Show a, HasCallStack) => String -> a -> (a,a) -> Assertion
assertContains preface x (l,r) = unless (l <= x && x <= r) (assertFailure msg)
    where msg = (if null preface then "" else preface ++ "\n") ++
                "expected range containing value: " ++ show x ++ "\n but got: " ++ show (l,r)

(@?∋) :: (Ord a, Show a, HasCallStack) => (a,a) -> a -> Assertion
(@?∋) = flip (assertContains "")


assertCREqual :: (CompReal r, HasCallStack) => String -> Rational -> r -> Acc -> Assertion
assertCREqual preface a x (Acc n) = assertContains preface a (bound x n)

(@?~) :: (CompReal r, HasCallStack) => r -> Rational -> Acc -> Assertion
(@?~) = flip (assertCREqual "")


infix 1 @?<=, @?∈, @?∋, @?~