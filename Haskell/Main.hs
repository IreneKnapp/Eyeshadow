{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text as T
import qualified Prelude as IO
import qualified System.Environment as IO

import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit
import Data.List

import Eyeshadow.Conduit
import Eyeshadow.Types


import Prelude
  (Bool(..),
   Either(..),
   Maybe(..),
   String,
   Int,
   IO,
   show,
   (.),
   ($),
   (==),
   (++))


main :: IO ()
main = do
  arguments <- IO.getArgs
  let (diagnostics, options, arguments') = takeOptions arguments
  let eitherUsageInput =
        case arguments' of
          [] -> Left usageDiagnostic
          inputFilePaths -> Right inputFilePaths
  case (diagnostics, eitherUsageInput) of
    ([], Right inputFilePaths) -> do
      diagnostics <- mapM process inputFilePaths >>= return . concat
      case diagnostics of
        [] -> return ()
        _ -> outputDiagnostics options diagnostics
    _ -> outputDiagnostics options diagnostics


process :: IO.FilePath -> IO [Diagnostic]
process sourcePath = runResourceT $ do
  readFile sourcePath $$ do
    let loop = do
          maybeItem <- await
          case maybeItem of
            Nothing -> return ()
            Just item -> liftIO $ IO.putStrLn "..."
    loop
  return []


outputDiagnostics :: Options -> [Diagnostic] -> IO ()
outputDiagnostics options diagnostics = return ()


takeOptions :: [String] -> ([Diagnostic], Options, [String])
takeOptions arguments =
  let loop ("--" : rest) diagnosticsSoFar optionsSoFar =
        (diagnosticsSoFar, optionsSoFar, rest)
      loop ("--text" : rest) diagnosticsSoFar optionsSoFar =
        loop rest diagnosticsSoFar $ optionsSoFar {
                        optionsOutputFormat = TextOutputFormat
                      }
      loop ("--terminal" : rest) diagnosticsSoFar optionsSoFar =
        loop rest diagnosticsSoFar $ optionsSoFar {
                        optionsOutputFormat = TerminalOutputFormat
                      }
      loop ("--json" : rest) diagnosticsSoFar optionsSoFar =
        loop rest diagnosticsSoFar $ optionsSoFar {
                        optionsOutputFormat = JSONOutputFormat
                      }
      loop (option@('-' : _) : rest) diagnosticsSoFar optionsSoFar =
        loop rest (diagnosticsSoFar ++ [unknownOptionDiagnostic option])
             optionsSoFar
      loop rest diagnosticsSoFar optionsSoFar =
        (diagnosticsSoFar, optionsSoFar, rest)
  in loop arguments []
      $ Options {
            optionsOutputFormat = TerminalOutputFormat,
            optionsOutputSourceSnippets = True
          }


usageDiagnostic :: Diagnostic
usageDiagnostic =
  Diagnostic {
       diagnosticHeadline =
         "Usage: eyeshadow [options ...] [--] input.hue ...",
       diagnosticDescription =
         T.intercalate "\n"
           ["--text           Use plain-text output (no color-control",
            "                 sequences) for all diagnostics.",
            "--terminal       Use plain-text output with color-control",
            "                 sequences for all diagnostics.",
            "--json           Use JavaScript Object Notation output for all",
            "                 diagnostics.  This is typically useful for other",
            "                 programs, rather than for humans to read",
            "                 directly.",
            "--[no-]snippets  Include (or omit) snippets of source-code in",
            "                 diagnostic output, to show the context of each",
            "                 message."],
       diagnosticDetails = []
     }


unknownOptionDiagnostic :: String -> Diagnostic
unknownOptionDiagnostic option =
  Diagnostic {
       diagnosticHeadline = T.concat ["Unknown option: ", T.pack option],
       diagnosticDescription =
         T.concat
           ["This indicates that you (or a shell script) provided a ",
            "command-line option which does not exist with that name in ",
            "this version of the Eyeshadow compiler.  For safety's sake, ",
            "the compiler will take no further action; if you're certain ",
            "the option has no effects upon which you are depending, simply ",
            "try again without it."],
       diagnosticDetails = []
     }
