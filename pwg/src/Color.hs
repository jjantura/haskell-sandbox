module Color
    (
        Color,
        mkColor,
        
        red, 
        green, 
        blue,

        redColor
    ) where

import Data.Word
import Hex

-- custom types
data Color = Color { red :: Word8, green :: Word8, blue :: Word8 }

instance Show Color where
    show c = (show $ toHex $ red c) ++ (show $ toHex $ green c) ++ (show $ toHex $ blue c)

mkColor :: Word8 -> Word8 -> Word8 -> Color
mkColor r g b = Color { red = r, green = g, blue = b}

redColor :: Color
redColor =  mkColor 255 0 0 

-- (...)

