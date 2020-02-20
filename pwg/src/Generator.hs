module Generator
(
  gradient,
  gradient',
  plasma
) where
    
import Color
import Device

-- type alias to avoid repeating ...
type ColorGen = Integer -> Integer -> Color

gradient :: ColorGen
gradient x y = mkColor (fromIntegral x) 0 0 

gradient' :: ColorGen
gradient' x y = mkColor (fromIntegral $ round $ 255 * (fromIntegral x / fromIntegral screenWidth)) 127 0 

plasma :: ColorGen
plasma x y = mkColor fx fy fz 
   where fx = (fromIntegral $ round $ (255 * sin(pi * fromIntegral x / fromIntegral screenWidth)))
         fy = (fromIntegral $ round $ (255 * sin(pi * fromIntegral y / fromIntegral screenHeight)))
         fz = (fromIntegral $ round $ (255 * sin(pi / 2 * fromIntegral y / fromIntegral screenHeight)))   
