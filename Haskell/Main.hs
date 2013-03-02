module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Prelude as IO
import qualified System.Environment as IO

import Control.Monad
import Data.Conduit
import Data.Conduit.Text

import Eyeshadow.Conduit


import Prelude
  (IO,
   show,
   ($))


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
  readFile sourcePath $$ do
    let loop = do
          maybeValue <- await
          case maybeValue of
            Nothing -> return ()
            Just (c, sourcePosition) -> do
              putStrLn $ show (c, sourcePosition)
              loop
    loop
