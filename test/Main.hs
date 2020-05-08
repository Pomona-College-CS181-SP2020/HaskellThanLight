module Main where

import Test.HUnit
import Control.Monad
import SDL.Vect
import GHC.Int

import HTL.Engine.Floor
import HTL.Engine.Crew
import HTL.Engine.Step

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

-- test movement
movement1 = TestCase (assertEqual "moving right"
                      (11,10) (moveTowards (10,10) (15,10)))
movement2 = TestCase (assertEqual "moving up"
                      (10,9) (moveTowards (10,10) (10,5)))
movement3 = TestCase (assertEqual "moving left and down"
                      (9,11) (moveTowards (10,10) (5,15)))
movement4 = TestCase (assertEqual "moving still"
                      (10,10) (moveTowards (10,10) (10,10)))

-- test step crew action
stepcrewaction1 = TestCase (assertEqual "crew sustain idle"
                            (Step'Sustain CrewAction'Idle)
                            (stepCrewAction Nothing (initCrewState floorKestrel (5,2))))
stepcrewaction2 = TestCase (assertEqual "crew change idle -> move"
                            (Step'Change CrewAction'Idle CrewAction'Move)
                            (stepCrewAction (Just [(5,2),(6,2)])
                            (CrewState True CrewAction'Idle 1 (findPositionByTile floorKestrel (5,2)) (5,2) Nothing)))

-- test which move anim
whichmoveanim1 = TestCase (assertEqual "crew right"
                           (CrewKey'Right)
                           (whichMoveAnim (10,10) (15,10) CrewKey'Idle))
whichmoveanim2 = TestCase (assertEqual "crew left"
                           (CrewKey'Left)
                           (whichMoveAnim (10,10) (5,10) CrewKey'Idle))
whichmoveanim3 = TestCase (assertEqual "crew up"
                           (CrewKey'Up)
                           (whichMoveAnim (10,10) (10,5) CrewKey'Idle))
whichmoveanim4 = TestCase (assertEqual "crew down"
                           (CrewKey'Down)
                           (whichMoveAnim (10,10) (10,15) CrewKey'Idle))
whichmoveanim5 = TestCase (assertEqual "crew default"
                           (CrewKey'Idle)
                           (whichMoveAnim (10,10) (10,10) CrewKey'Idle))

tests = TestList [TestLabel "pathfinding1" pathfinding1, TestLabel "pathfinding2" pathfinding2,
                  TestLabel "pathfinding3" pathfinding3, TestLabel "pathfinding4" pathfinding4,
                  TestLabel "tileselect1" tileselect1, TestLabel "tileselect2" tileselect2,
                  TestLabel "tileselect3" tileselect3, TestLabel "tileselect4" tileselect4,
                  TestLabel "movement1" movement1, TestLabel "movement2" movement2,
                  TestLabel "movement3" movement3, TestLabel "movement4" movement4,
                  TestLabel "stepcrewaction1" stepcrewaction1, TestLabel "stepcrewaction2" stepcrewaction2,
                  TestLabel "whichmoveanim1" whichmoveanim1, TestLabel "whichmoveanim2" whichmoveanim2,
                  TestLabel "whichmoveanim3" whichmoveanim3, TestLabel "whichmoveanim4" whichmoveanim4,
                  TestLabel "whichmoveanim5s" whichmoveanim5]

main::IO Counts
main = runTestTT tests
