module Main where

{-
import Tetris
import Point (Point(..))
import qualified Point

piece = TetrisPiece
    { origin = Just (Point { x = 0, y = 0 })
    , offsets =
        [ Point.Offset
            { Point.xOffset = id
            , Point.yOffset = id }
        , Point.Offset
            { Point.xOffset = (+ 1)
            , Point.yOffset = id }
        , Point.Offset
            { Point.xOffset = (+ 2)
            , Point.yOffset = id }
        , Point.Offset
            { Point.xOffset = (+ 3)
            , Point.yOffset = id }
        ]
    }
-}

main :: IO ()
main = do
  putStrLn "hello world"
