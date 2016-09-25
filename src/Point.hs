{-# LANGUAGE RecordWildCards #-}
module Point where

data Point = Point
    { x :: Int
    , y :: Int
    } deriving (Show, Eq)

data Offset = Offset
    { xOffset :: Int -> Int
    , yOffset :: Int -> Int
    }

applyOffset :: Point -> Offset -> Point
applyOffset Point{ x = oldX, y = oldY } Offset{..} =
    Point { x = xOffset oldX, y = yOffset oldY }
