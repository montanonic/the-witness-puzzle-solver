{-# LANGUAGE RecordWildCards #-}
module Tetris where

import Point (Point(..))
import qualified Point

-- Negative blocks can be removed from the Positive.
data TetrisType
    = Positive
    | Negative
    deriving (Show, Eq)


{- A piece is represented as an origin point, and a list of offsets from that
origin that denote its full structure. The origin may be left empty to indicate
that the piece is not placed on the board.

XXXX
could be represented as
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
depends solely upon where the origin. Left-to-right and top-to-bottom will be
prefered as the default orientation.
-}
data TetrisPiece = TetrisPiece
    { origin :: Maybe Point
    , offsets :: [Point.Offset]
    }

-- If no origin exists, default to (0,0)
instance Show TetrisPiece where
    show piece =
        case (origin piece) of
            Nothing ->
                show piece{ origin = Just (Point { x = 0, y = 0 }) }
            Just originPoint ->
                "TetrisPiece (" ++ show(
                    [ Point.applyOffset originPoint offset
                    | offset <- offsets piece ])
                    ++ ")"


{- rotateClockwise 4 times returns the original piece
TODO: Add quickcheck property to prove this.

90 degrees clockwise rotation is (y, -x)
-}
rotateClockwise :: TetrisPiece -> TetrisPiece
rotateClockwise piece@TetrisPiece{..} =
    let
        rotateOffset :: Point.Offset -> Point.Offset
        rotateOffset Point.Offset{ xOffset = oldX, yOffset = oldY } =
            Point.Offset { xOffset = oldY, yOffset = negate . oldX }
    in
        piece { offsets = map rotateOffset offsets }


data Tetris = Tetris
    { tetrisType :: TetrisType
    , tetrisPiece :: TetrisPiece
    } deriving (Show)
