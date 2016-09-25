{-# LANGUAGE RecordWildCards, MultiWayIf #-}
module Grid where

import Prelude hiding (Left, Right)
import qualified Point

{- The entire puzzle Grid. A Grid with no obstructions will contain a number of
paths equal to (fst gridSize + 1) * (snd gridSize + 1).

gridSize = (4, 4)
-----------
| X X X X |
| X X X X |
| X X X X |
| X X X X |
-----------
-}
data Grid = Grid
    { gridSize :: (Int, Int) -- ^ Tuple of # blocks on x & y axes.
    , gridEntrances :: [Point.Point]
    , gridExits :: [Point.Point]
    } deriving (Show, Eq)

data Point = Point
    { point :: Point.Point
    , pointOccupied :: Bool
    , nearbyPoints :: [(Direction, Point)]
    } deriving (Show, Eq)

data Direction
    = Up
    | Down
    | Left
    | Right
    deriving (Eq, Show)

{- Given a Point and a list of Points, returns all the points within one move of
the current one, and their direction from the current one.
-}
nearby :: Point -> [Point] -> [(Direction, Point)]
nearby p ps =
    let
        px =
            (Point.x $ point $ p)
        py =
            (Point.y $ point $ p)

        nearbyPoints =
            filter (\(Point { point = Point.Point{..} }) ->
                    x == px + 1
                    || x == px - 1
                    || y == py + 1
                    || y == py - 1
                    )
                ps
    in
        -- Give each nearbyPoint a direction from the current point.
        (\(pnt@(Point { point = Point.Point{..} })) ->
                if | px + 1 == x -> [(Right, pnt)]
                   | px - 1 == x -> [(Left, pnt)]
                   | py + 1 == y -> [(Up, pnt)]
                   | py - 1 == y -> [(Down, pnt)]
                   | otherwise -> []
            ) =<< nearbyPoints


generateNewData :: (Int, Int) -> [Point]
generateNewData (xSize, ySize) =
    let
        points =
            [ Point
                { point = Point.Point { x = x, y = y }
                , pointOccupied = False
                , nearbyPoints = []
                }
            | x <- [0..xSize], y <- [0..ySize] ]

        updateWithNearby :: [Point] -> Point -> Point
        updateWithNearby allPoints point =
            point { nearbyPoints = nearby point allPoints }

    in
        map (updateWithNearby points) points


data Path = Path
    { pathCurrentLocation :: Point
    , pathAvailableDirections :: [Direction]
    , pathNextLocations :: [Path]
    }
