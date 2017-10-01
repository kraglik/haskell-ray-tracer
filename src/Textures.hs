module Textures where

import BaseTypes
import Data.Vector.V3

--------------------------------------------- some basic textures here -------------------------------------------------

data ConstColor = ConstColor Color

instance TextureType ConstColor where
    color_at _ (ConstColor color) = color

data ChessBoard = ChessBoard Color Color Double

instance TextureType ChessBoard where
    color_at (Vector3 x _ y) (ChessBoard black white size)
        | mod (int_x + int_y) 2 == 0 = black
        | otherwise = white
            where (int_x, int_y) = (round (x / size), round (y / size))
