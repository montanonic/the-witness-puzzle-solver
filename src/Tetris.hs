{-# LANGUAGE RecordWildCards #-}

module Tetris where

import Data.Foldable

-- The entire puzzle Grid
data Grid a = Grid
    { gridSize :: Int
    , gridData :: [[a]]
    } deriving (Show, Eq)

-- Negative blocks can be removed from the Positive.
data TetrisType
    = Positive
    | Negative
    deriving (Show, Eq)

data Point = Point
    { x :: Int
    , y :: Int
    } deriving (Show, Eq)

data PointOffset = PointOffset
    { xOffset :: Int -> Int
    , yOffset :: Int -> Int
    }

{- A piece is represented as an origin point, and a list of offsets from that
origin that denote its full structure. The origin may be left empty to indicate
that the piece is not placed on the board.

XXXX
would be represented as
TetrisPiece { origin = Nothing
            , offsets = [ PointOffset { xOffset = id, yOffset = id }
                        , PointOffset { xOffset = (+ 1), yOffset = id }
                        , PointOffset { xOffset = (+ 2), yOffset = id }
                        , PointOffset { xOffset = (+ 3), yOffset = id }
                        ]
            }
that is, it is a piece that starts anywhere, and which contains 3 additional
blocks along its x-axis, contiguously.

A vertically oriented piece would be the same, except instead of x-offsets, it
would be y-offsets. Note that both positive and negative offsets work, as it
depends solely on the origin.
-}
data TetrisPiece = TetrisPiece
    { origin :: Maybe Point
    , offsets :: [PointOffset]
    }

instance Show TetrisPiece where
    show piece =
        case (origin piece) of
            Nothing ->
                "TetrisPiece"
            Just originPoint ->
                show $
                    [ applyOffset originPoint offset
                    | offset <- offsets piece ]


applyOffset :: Point -> PointOffset -> Point
applyOffset Point{ x = oldX, y = oldY } PointOffset{..} =
    Point { x = xOffset oldX, y = yOffset oldY }

{- rotateClockwise 4 times returns the original piece
TODO: Add quickcheck property to prove this.

90 degrees clockwise rotation is (y, -x)
-}
rotateClockwise :: TetrisPiece -> TetrisPiece
rotateClockwise piece@TetrisPiece{..} =
    let
        rotateOffset :: PointOffset -> PointOffset
        rotateOffset PointOffset{ xOffset = oldX, yOffset = oldY } =
            PointOffset { xOffset = oldY, yOffset = negate . oldX }
    in
        piece { offsets = map rotateOffset offsets }


data Tetris = Tetris
    { tetrisType :: TetrisType
    , tetrisPiece :: Grid Bool
    } deriving (Show, Eq)
