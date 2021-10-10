
module Main where

import qualified Data.QRCode as Q -- from `haskell-qrencode' package
import qualified Codec.Picture as P -- from `JuicyPixels' package
import qualified Codec.Picture.Png as P
import qualified Data.ByteString.Lazy as B
import System.Console.Haskeline

pixelPerCell = 5

data Lines = OneLine | MultiLine

main :: IO ()
main = do fName <- runInputT defaultSettings $ getText OneLine "Enter file name ( NO EXTENSION! ) for saving QR Code:  "
          l <- runInputT defaultSettings $ getText MultiLine "Enter a line of text or Ctrl D for no more lines:\n"
          --let l = frost
          let version = Nothing -- auto, or Maybe Int to select manually with Int -> 1 - 40
              errorLevel = Q.QR_ECLEVEL_M  --QR_ECLEVEL_L, QR_ECLEVEL_M, QR_ECLEVEL_Q, or QR_ECLEVEL_H (i.e. low, medium, quality, and high) high recommended for embeded logo
              mode = Q.QR_MODE_EIGHT       --QR_MODE_NUM, QR_MODE_AN, QR_MODE_EIGHT, or QR_MODE_KANJI  (i.e. numberic, alpha-numeric, 8-bit data, Kanji (shift-jis) )
          qr <- Q.encodeString l version errorLevel mode True
          let width = Q.getQRCodeWidth qr
              mat = Q.toMatrix qr
              pixelAt x y = let x' = x `div` pixelPerCell
                                y' = y `div` pixelPerCell
                                i = y'
                                j = x'
                            in if mat!!i!!j == 0 then maxBound else minBound :: P.Pixel8
              image = P.generateImage pixelAt (width*pixelPerCell) (width*pixelPerCell)
              encodedImage = P.encodePng image
          B.writeFile (fName ++ ".png") encodedImage

getText :: Lines -> String -> InputT IO (String)
getText nLines prompt = do
  l <- getInputLine prompt
  case l of
    Just txt -> case nLines of
                  OneLine -> return txt
                  MultiLine -> fmap (("\n" ++ txt) <> ) $ getText nLines prompt
    Nothing -> return "" :: InputT IO (String)





frost = "Two roads diverged in a yellow wood,\n\
        \And sorry I could not travel both\n\
        \And be one traveler, long I stood\n\
        \And looked down one as far as I could\n\
        \To where it bent in the undergrowth;\n\
        \ \n\
        \Then took the other, as just as fair,\n\
        \And having perhaps the better claim,\n\
        \Because it was grassy and wanted wear;\n\
        \Though as for that the passing there\n\
        \Had worn them really about the same,\n\
        \ \n\
        \And both that morning equally lay\n\
        \In leaves no step had trodden black.\n\
        \Oh, I kept the first for another day!\n\
        \Yet knowing how way leads on to way,\n\
        \I doubted if I should ever come back.\n\
        \ \n\
        \I shall be telling this with a sigh\n\
        \Somewhere ages and ages hence:\n\
        \Two roads diverged in a wood, and I -\n\
        \I took the one less traveled by,\n\
        \And that has made all the difference."













 
{--
getText :: InputT IO (String)
getText = do
  l <- getInputLine "Next line:\n"
  case l of
    Just txt -> return txt
    Nothing -> error "something wrong"
--}
  

 {--
main ::IO ()
main = do 
         aLine <- runInputT defaultSettings getText
         putStrLn ("You wrote:  " ++ aLine) 
--}  

{--
import Data.QRCode (encodeString, getQRCodeString, toMatrix, QREncodeLevel(..), QREncodeMode(..))
import qualified Data.ByteString.Char8 as C8 (unpack)
import GHC.Word
import qualified Diagrams.Prelude as DPRE
import qualified Diagrams.Backend.SVG as DSVG
import qualified Diagrams.QRCode as DQR 
--}

{--
main :: IO ()
main = do putStrLn "This is the next phase, get the matrix of 1's and 0's""
          let testString = "John Polo"
          code <- fmap getQRCodeString $ encodeString testString Nothing QR_ECLEVEL_M QR_MODE_EIGHT True
          putStrLn "\n\nThe QR code is:\n"
          putStrLn (C8.unpack code)
--}

{--
main :: IO ()
main = do putStrLn "This is the next phase, get the matrix of 1's and 0's"
          let testString = "John Polo"
          code <- encodeString testString Nothing QR_ECLEVEL_M QR_MODE_EIGHT True
          let matrix = toMatrix code
          putStrLn $ show  matrix
--}


{--
main :: IO ()
main = do putStrLn "This is the next phase, get the matrix of 1's and 0's"
          let testString = "John Polo"
          code <- encodeString testString Nothing QR_ECLEVEL_M QR_MODE_EIGHT True
          let matrix = toMatrix code 
              thePath = DQR.pathMatrix matrix
              theStroke = DQR.stroke thePath :: DPRE.Backend DSVG.SVG DPRE.V2 (DSVG.SVGFloat Float)             -- DPRE.QDiagram DSVG.SVG DPRE.V2 Double DPRE.Any 
              dia = DPRE.scale 6 $ DQR.stroke thePath
          putStrLn "Is it working?"
          putStrLn $ show matrix

testString = "John Polo"

code = encodeString testString Nothing QR_ECLEVEL_M QR_MODE_EIGHT True

matrix = fmap toMatrix code 

thePath = fmap DQR.pathMatrix matrix

s = DQR.stroke :: DPRE.Path DPRE.V2 Double -> DPRE.QDiagram DSVG.SVG DPRE.V2 Double DPRE.Any

theStroke = fmap DQR.stroke thePath :: IO ( DPRE.QDiagram DSVG.SVG DPRE.V2 Double DPRE.Any)
--}

--sc = DPRE.scale 6 :: DPRE.Additive (DPRE.Measured n)
