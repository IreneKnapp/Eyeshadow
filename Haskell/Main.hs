module Main (main) where

import Prelude hiding (Show(..))

import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as C
import Data.Conduit.Text
import System.Environment
import System.IO hiding (utf8)

import Knapp.Show
import Lexer


main :: IO ()
main = do
  parameters <- getArgs
  case parameters of
    [sourceFilePath] -> do
      runResourceT
        $ sourceFile sourceFilePath
        $= runLexer defaultLexer
        $= C.map show
        $= encode utf8
        $$ sinkHandle stdout
    _ -> do
      programName <- getProgName
      putStrLn $ "Usage: " ++ programName ++ " input.knapp"
