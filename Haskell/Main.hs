module Main (main) where

import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Prelude as IO
import qualified System.Environment as IO

import Control.Monad
import Data.Conduit
import Data.Conduit.Text

import qualified Eyeshadow.UTF8 as UTF8


import Prelude
  (Either(..),
   Maybe(..),
   Int,
   IO,
   show,
   (.),
   ($),
   (==),
   (++))


data SourcePosition =
  SourcePosition {
      sourcePositionByteOffset :: Int,
      sourcePositionCharacterOffset :: Int,
      sourcePositionLine :: Int,
      sourcePositionColumn :: Int
    }

data SourceSpan =
  SourceSpan {
      sourceSpanStart :: SourcePosition,
      sourceSpanEnd :: SourcePosition
    }

data Diagnostic =
  Diagnostic {
      diagnosticHeadline :: T.Text,
      diagnosticDescription :: T.Text,
      diagnosticDetails :: [(T.Text, SourceSpan)]
    }



main :: IO ()
main = do
  arguments <- IO.getArgs
  case arguments of
    [inputFilePath] -> process inputFilePath
    _ -> IO.putStrLn $ "Usage: eyeshadow input.hue"


process :: IO.FilePath -> IO ()
process sourcePath = do
  Exception.catch
    (do
       byteString <-
         mapM (\character ->
                 case UTF8.encode character of
                   Left e -> Exception.throwIO e
                   Right byteString -> return byteString)
              sourcePath
         >>= return . BS.concat
       let loop soFar rest = do
             if BS.null rest
               then return soFar
               else case UTF8.decode rest of
                      Left e -> Exception.throwIO e
                      Right (c, rest) -> loop (soFar ++ [c]) rest
       roundTrip <- loop "" byteString
       if sourcePath == roundTrip
         then IO.putStrLn $ "Round-tripped okay."
         else IO.putStrLn $ "Round-trip broken: " ++ (show byteString) ++ " "
                            ++ (show roundTrip))
    (\e -> IO.putStrLn $ show (e :: Exception.SomeException))
