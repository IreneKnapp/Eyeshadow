module Main (main) where

import Prelude hiding (Show(..))

import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as C
import Data.Conduit.Text
import qualified Data.Text as T
import System.Environment
import System.IO hiding (utf8)

import Knapp.Conduit
import Knapp.Show
import Lexer
import Parser


main :: IO ()
main = do
  parameters <- getArgs
  case parameters of
    [sourceFilePath] -> do
      _ <- runResourceT
        $ sourceFile sourceFilePath
        $= runLexer defaultLexer
        $= sideStream runParser
        $$ split (do
                    liftIO $ putStrLn $ "Errors"
                    C.map show =$ encode utf8 =$ sinkHandle stdout)
                 (do
                    liftIO $ putStrLn $ ""
                    liftIO $ putStrLn $ "Results"
                    C.map show =$ encode utf8 =$ sinkHandle stdout)
      return ()
    _ -> do
      programName <- getProgName
      putStrLn $ "Usage: " ++ programName ++ " input.knapp"

