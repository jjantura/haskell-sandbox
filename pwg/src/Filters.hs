module Filters (
    darken,
    unblue
) where

import Data.Word
import Color 

darken :: Word8 -> Color -> Color
darken l sc = mkColor (darken' red) (darken' green) (darken' blue) where
    darken' f = if f sc <= l then 0 else f sc - l

unblue :: Color -> Color
unblue sc = mkColor (red sc) (green sc) 0 

    

