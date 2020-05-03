module Main where

import Test.HUnit
import Control.Monad

test1 = TestCase (assertEqual "obviously true" 2 (1 + 1))
test2 = TestCase (assertEqual "obviously false" 3 (1 + 1))

pathfinding1 = TestCase (assertEqual "simple pathfinding" [(0,2),(1,2)]
                         (findShortestPath buildFloorKestrel (0,2) (1,2)))
pathfinding2 = TestCase (assertEqual "longer pathfinding" 
                         [(6,0),(7,0),(7,1),(7,2),(6,2),(5,2),(5,3),(6,3),(7,3)]
                         (findShortestPath buildFloorKestrel (6,0) (7,3)))
pathfinding3 = TestCase (assertEqual "very long pathfinding" 
                         [(0,2),(1,2),(2,2),(2,1),(3,1),(4,1),(4,2),(5,2),
                         (6,2),(7,2),(7,1),(8,1),(8,2),(9,2),(10,2),(11,2)]
                         (findShortestPath buildFloorKestrel (0,2) (11,2)))

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2,
                  TestLabel "pathfinding1" pathfinding1, TestLabel "pathfinding2" pathfinding2,
                  TestLabel "pathfinding3" pathfinding3]

main::IO Counts
main = runTestTT tests
