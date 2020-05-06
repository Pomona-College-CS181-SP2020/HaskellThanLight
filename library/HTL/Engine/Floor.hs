module HTL.Engine.Floor where

import qualified Safe
import Linear (V2(..))
import SDL.Vect
import GHC.Int
import Control.Lens
import Data.List
import Data.Function

import HTL.Engine.Types

data TileAdjacent = TileAdjacent
  { taUp :: Bool
  , taDown :: Bool
  , taRight :: Bool
  , taLeft :: Bool
  } deriving (Show, Eq)

data TileInfo = TileInfo
  { tiPos :: (Int,Int)
  , tiAdjacent :: TileAdjacent
  } deriving (Show, Eq)

data FloorState = FloorState
  { fsTiles :: [TileInfo]
  , fsMoveable :: Bool
  , fsOffset :: Point V2 Int
  } deriving (Show, Eq)

floorKestrel :: FloorState
floorKestrel =
  FloorState
  { fsTiles = 
    [ TileInfo (0,2) $ TileAdjacent False True True False
    , TileInfo (0,3) $ TileAdjacent True False True False
    -----------------------------------
    , TileInfo (1,1) $ TileAdjacent False False True False
    , TileInfo (1,2) $ TileAdjacent False True True True
    , TileInfo (1,3) $ TileAdjacent True False True True
    , TileInfo (1,4) $ TileAdjacent False False True False
    -----------------------------------
    , TileInfo (2,1) $ TileAdjacent False True True True
    , TileInfo (2,2) $ TileAdjacent True True False True
    , TileInfo (2,3) $ TileAdjacent True True False True
    , TileInfo (2,4) $ TileAdjacent True False True True
    -----------------------------------
    , TileInfo (3,1) $ TileAdjacent False False True True
    , TileInfo (3,4) $ TileAdjacent False False True True
    -----------------------------------
    , TileInfo (4,1) $ TileAdjacent False True False True
    , TileInfo (4,2) $ TileAdjacent True True True False
    , TileInfo (4,3) $ TileAdjacent True True True False
    , TileInfo (4,4) $ TileAdjacent True False False True
    -----------------------------------
    , TileInfo (5,2) $ TileAdjacent False True True True
    , TileInfo (5,3) $ TileAdjacent True False True True
    -----------------------------------
    , TileInfo (6,0) $ TileAdjacent False False True False
    , TileInfo (6,1) $ TileAdjacent False True True False
    , TileInfo (6,2) $ TileAdjacent True False True True
    , TileInfo (6,3) $ TileAdjacent False True True True
    , TileInfo (6,4) $ TileAdjacent True False True False
    , TileInfo (6,5) $ TileAdjacent False False True False
    -----------------------------------
    , TileInfo (7,0) $ TileAdjacent False True False True
    , TileInfo (7,1) $ TileAdjacent True True True True
    , TileInfo (7,2) $ TileAdjacent True False False True
    , TileInfo (7,3) $ TileAdjacent False True False True
    , TileInfo (7,4) $ TileAdjacent True True True True
    , TileInfo (7,5) $ TileAdjacent True False False True
    -----------------------------------
    , TileInfo (8,1) $ TileAdjacent False True False True
    , TileInfo (8,2) $ TileAdjacent True True True False
    , TileInfo (8,3) $ TileAdjacent True True True False
    , TileInfo (8,4) $ TileAdjacent True False True True
    -----------------------------------
    -- , TileInfo (9,1) $ TileAdjacent -- MedbayTile
    , TileInfo (9,2) $ TileAdjacent False False True True
    , TileInfo (9,3) $ TileAdjacent False True True True
    , TileInfo (9,4) $ TileAdjacent True False False True
    -----------------------------------
    , TileInfo (10,2) $ TileAdjacent False False True True
    , TileInfo (10,3) $ TileAdjacent False False True True
    -----------------------------------
    , TileInfo (11,2) $ TileAdjacent False False True True
    , TileInfo (11,3) $ TileAdjacent False False True True
    -----------------------------------
    , TileInfo (12,2) $ TileAdjacent False True True True
    , TileInfo (12,3) $ TileAdjacent True False True True
    -----------------------------------
    , TileInfo (13,2) $ TileAdjacent False True False True
    , TileInfo (13,3) $ TileAdjacent True False True True
    -----------------------------------
    , TileInfo (14,2) $ TileAdjacent False True False False
    , TileInfo (14,3) $ TileAdjacent True False False True
    ]
  , fsMoveable = True
  , fsOffset = P (V2 187 264)
  }

tileSize :: Int
tileSize = 35

findShortestPath' :: FloorState -> (Int,Int) -> (Int,Int) -> [(Int,Int)] -> Maybe [(Int,Int)]
findShortestPath' floor (x,y) endTile acc =
  if (x,y) == endTile
  then Just ((x,y):acc)
  else case find (\ti -> tiPos ti == (x,y)) (fsTiles floor) of
         Just tInfo -> if pathLength shortestPath < 50 then shortestPath else Nothing
            where
              xPath dir xChange yChange = 
                if dir (tiAdjacent tInfo) && (not $ (x,y) `elem` acc)
                then findShortestPath' floor (x+xChange,y+yChange) endTile ((x,y):acc)
                else Nothing
              upPath = xPath taUp 0 (-1)
              downPath = xPath taDown 0 1
              rightPath = xPath taRight 1 0
              leftPath = xPath taLeft (-1) 0
              pathLength p = case p of
                Just p' -> length p'
                Nothing -> 50
              shortestPath = 
                minimumBy (compare `on` pathLength) [upPath,downPath,rightPath,leftPath]
         Nothing -> Nothing

findShortestPath :: FloorState -> (Int,Int) -> (Int,Int) -> Maybe [(Int,Int)]
findShortestPath floor initTile endTile = case findShortestPath' floor initTile endTile [] of
  Just path -> Just (reverse path)
  Nothing -> Nothing

findTileByPosition :: FloorState -> Maybe (Point V2 Int32) -> Maybe (Int,Int)
findTileByPosition floor maybeMPos = case maybeMPos of
  Just mPos -> if relX >= 0 && relY >= 0 &&
                  relX < (15 * tileSize) && relY < (6 * tileSize)
               then Just (relX `div` tileSize, relY `div` tileSize)
               else Nothing
               where
                 relX = fromIntegral (mPos ^._x) - (fsOffset floor) ^._x
                 relY = fromIntegral (mPos ^._y) - (fsOffset floor) ^._y
  Nothing -> Nothing