{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module Main (main) where

import qualified Data.Text as T
import qualified Prelude as IO
import qualified System.Environment as IO

import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit
import Data.List

import Eyeshadow.Conduit
import Eyeshadow.Lexical
import Eyeshadow.Types


import Prelude
  (Bool(..),
   Char(..),
   Either(..),
   Maybe(..),
   String,
   Int,
   IO,
   (.),
   ($),
   (==),
   (++))


main :: IO ()
main = do
  arguments <- IO.getArgs
  let (diagnostics, options, arguments') = takeOptions arguments
  case (diagnostics, arguments') of
    ([], inputFilePaths@(_:_)) -> do
      mapM_ (process options) inputFilePaths
    ([], []) -> repl options
    _ -> mapM_ outputDiagnostic options diagnostics


process :: Options -> IO.FilePath -> IO [Diagnostic]
process options sourcePath = runResourceT $ do
  readFile sourcePath $$ consume options


repl :: Options -> IO ()
repl options = do
  readTerminal $$ consume options
  liftIO $ IO.putStrLn $ "Managed to end the REPL."
  repl


consume
  :: (MonadIO m)
  => Options
  -> SourceFileSpecification
  -> Sink (Either Diagnostic (Char, SourcePosition)) m ()
consume options file = do
  lex file =$ sideStream showIt =$ discardRight $$ explain
  where showIt
          :: (MonadIO m, IO.Show b)
          => Conduit (b, SourcePosition)
                     m
                     (Either Diagnostic (b, SourcePosition))
        showIt = do
          let loop = do
                maybeItem <- await
                case maybeItem of
                  Nothing -> return ()
                  Just (c, position) -> do
                    liftIO $ IO.putStrLn
                     $ (IO.show $ sourcePositionLine position) ++ ":"
                       ++ (IO.show $ sourcePositionColumn position)
                       ++ " " ++ (IO.show c)
                    loop
          loop
        discardRight
          :: (Monad m)
          => Conduit (Either Diagnostic b) m Diagnostic
        discardRight = do
          let loop = do
                maybeItem <- await
                case maybeItem of
                  Nothing -> return ()
                  Just (Left diagnostic) -> do
                    yield diagnostic
                    loop
                  _ -> loop
          loop
        explain
          :: (MonadIO m)
          => Sink Diagnostic m ()
        explain = do
          maybeDiagnostic <- await
          case maybeDiagnostic of
            Nothing -> return ()
            Just diagnostic -> do
              liftIO $ outputDiagnostic options diagnostic
              explain


outputDiagnostic :: Options -> Diagnostic -> IO ()
outputDiagnostic options diagnostic = do
  IO.putStrLn $ (T.unpack $ diagnosticHeadline diagnostic)


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

