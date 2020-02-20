module Main where

import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import System.IO
import Canvas
import Color
import Generator
import Device
import Filters


main :: IO ()
main = do
    handle <- openFile "wallpaper.raw" WriteMode
--    B.hPut handle (B.pack $ toWords $ mkCanvas screenWidth screenHeight redColor)
--    B.hPut handle (B.pack $ toWords $ mkCanvas' screenWidth screenHeight gradient)
--    B.hPut handle (B.pack $ toWords $ mkCanvas' screenWidth screenHeight gradient')   
--    B.hPut handle (B.pack $ toWords $ mkCanvas' screenWidth screenHeight plasma)   
    B.hPut handle (B.pack $ toWords $ fmap (darken 127) $ mkCanvas' screenWidth screenHeight plasma)  
    B.hPut handle (B.pack $ toWords $ fmap (unblue . darken 127) $ mkCanvas' screenWidth screenHeight plasma)  
    hClose handle
