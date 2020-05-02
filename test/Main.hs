module Main where

import Test.HUnit
import Control.Monad

test1 = TestCase (assertEqual "obviously true" 2 (1 + 1))
test2 = TestCase (assertEqual "obviously false" 3 (1 + 1))

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

main::IO Counts
main = runTestTT tests
