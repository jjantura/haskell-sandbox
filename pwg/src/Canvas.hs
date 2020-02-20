
module Canvas
    ( mkCanvas,
    mkCanvas',
    toWords
    ) where

import Data.Word
import qualified Data.List as L
import Color

-- custom type aliases
type Canvas = [Color]
type Width = Integer
type Height = Integer

-- functions 
mkCanvas :: Width -> Height -> Color -> Canvas
mkCanvas w h c = replicate (fromIntegral w * fromIntegral h) c

mkCanvas' :: Width -> Height -> (Integer -> Integer -> Color) -> Canvas
mkCanvas' w h f = [f x y | y <- [0..h - 1], x <- [0..w - 1]]

-- terribly slow ;)
toWords' :: Canvas -> [Word8]
toWords' c = L.foldr (\e -> \a -> a ++ [red e, green e, blue e] ) [] c

toWords :: Canvas -> [Word8]
toWords [] = []
toWords (x:xs) = [red x, green x, blue x] ++ toWords xs
