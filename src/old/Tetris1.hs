{- First attempt at representing tetris pieces in The Witness -}

{-# LANGUAGE RecordWildCards #-}

module Tetris1 where

-- The entire puzzle Grid
data Grid a = Grid
    { gridSize :: Int
    , gridData :: [[a]]
    }

-- Negative blocks can be removed from the Positive.
data TetrisType = Positive | Negative

{- A piece is represented as a grid of boolean values, with True representing
the locations of the block values, and False indicating absenses. The size of
the grid will always be the minimal size large enough to fit all the pieces,
hence, it will be the square of the longest side. Each sublist indicates a
row; orientation is left-to-right, top-to-bottom. Here are two examples of the
layout:

XXXX == [[True, True, True, True],
         [False, False, False, False],
         [False, False, False, False],
         [False, False, False, False]]

X
X
X
X
 == [[True, False, False, False],
     [True, False, False, False],
     [True, False, False, False],
     [True, False, False, False]]

So writing out the values as a matrix gives the proper 2d representation.
-}
data Tetris = Tetris
    { tetrisType :: TetrisType
    , tetrisPiece :: Grid Bool
    }

{-
addPieces :: TetrisPiece -> TetrisPiece -> TetrisPiece
addPieces tp1 tp2 =
    let
        p1 = tetrisPiece tp1
        p2 = tetrisPiece tp2
-}
