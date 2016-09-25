module Main where

import Tetris

piece =
    TetrisPiece { origin = Just (Point { x = 0, y = 0 })
                , offsets = [ PointOffset { xOffset = id, yOffset = id }
                            , PointOffset { xOffset = (+ 1), yOffset = id }
                            , PointOffset { xOffset = (+ 2), yOffset = id }
                            , PointOffset { xOffset = (+ 3), yOffset = id }
                            ]
                }

main :: IO ()
main = do
  putStrLn "hello world"
