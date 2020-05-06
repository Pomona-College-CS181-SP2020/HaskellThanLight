module Main where

import Test.HUnit
import Control.Monad
import SDL.Vect
import GHC.Int

import HTL.Engine.Floor

-- pathfinding tests
pathfinding1 = TestCase (assertEqual "simple pathfinding" (Just [(0,2),(1,2)])
                         (findShortestPath floorKestrel (0,2) (1,2)))
pathfinding2 = TestCase (assertEqual "longer pathfinding" 
                         (Just [(6,0),(7,0),(7,1),(7,2),(6,2),(5,2),(5,3),(6,3),(7,3)])
                         (findShortestPath floorKestrel (6,0) (7,3)))
pathfinding3 = TestCase (assertEqual "very long pathfinding" 
                         (Just [(0,2),(1,2),(2,2),(2,1),(3,1),(4,1),(4,2),(5,2),
                         (6,2),(6,1),(7,1),(8,1),(8,2),(9,2),(10,2),(11,2)])
                         (findShortestPath floorKestrel (0,2) (11,2)))
pathfinding4 = TestCase (assertEqual "impossible pathfinding" 
                         (Nothing) (findShortestPath floorKestrel (0,2) (3,2)))

-- tile selection tests
tileselect1 = TestCase (assertEqual "edge select upper left"
                        (Just (0,0))
                        (findTileByPosition floorKestrel (Just (P (V2 187 264)))))
tileselect2 = TestCase (assertEqual "no position"
                        Nothing
                        (findTileByPosition floorKestrel Nothing))
tileselect3 = TestCase (assertEqual "game like select"
                        (Just (0,2))
                        (findTileByPosition floorKestrel (Just (P (V2 190 342)))))
tileselect4 = TestCase (assertEqual "bad select"
                        Nothing
                        (findTileByPosition floorKestrel (Just (P (V2 134 432)))))

tests = TestList [TestLabel "pathfinding1" pathfinding1, TestLabel "pathfinding2" pathfinding2,
                  TestLabel "pathfinding3" pathfinding3, TestLabel "pathfinding4" pathfinding4,
                  TestLabel "tileselect1" tileselect1, TestLabel "tileselect2" tileselect2,
                  TestLabel "tileselect3" tileselect3, TestLabel "tileselect4" tileselect4]

main::IO Counts
main = runTestTT tests
