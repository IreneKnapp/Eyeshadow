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
import Eyeshadow.Process
import Eyeshadow.Spans
import Eyeshadow.Types


import Prelude
  (Bool(..),
   Char,
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
  case (diagnostics, invocationOptionsMode options, arguments') of
    ([], HelpInvocationMode, _) -> outputDiagnostic options usageDiagnostic
    ([], CompilationInvocationMode, inputFilePaths@(_:_)) -> do
      mapM_ (\sourcePath -> runResourceT $ do
               let file = FileSourceFileSpecification sourcePath
               readFile file
                 $$ consume options file False initialProcessingContext)
            inputFilePaths
    ([], CompilationInvocationMode, []) -> do
      let file = TerminalSourceFileSpecification
          loop processingContext = do
            processingContext <-
              readTerminal file $$ consume options file True processingContext
            loop processingContext
      loop initialProcessingContext
    _ -> mapM_ (outputDiagnostic options) diagnostics


consume
  :: (MonadIO m)
  => InvocationOptions
  -> SourceFileSpecification
  -> Bool
  -> ProcessingContext
  -> Sink (Either Diagnostic (Char, SourcePosition)) m ProcessingContext
consume options file interactive processingContext =
  sideStream (lex file)
  $= sideStream processingScope
  $$ sideStream (process options file processingContext)
  =$ sideStream showIt
  =$ discardRight
  =$ explain
  return processingContext
  where processingScope
          :: (MonadIO m)
          => Conduit SExpression m (Either Diagnostic SExpression)
        processingScope = do
          maybeItem <- await
          case maybeItem of
            Nothing -> return ()
            Just item -> yield $ Right item
          if interactive
            then return ()
            else processingScope
        showIt
          :: (MonadIO m)
          => Conduit SExpression m (Either Diagnostic SExpression)
        showIt = do
          let loop = do
                maybeItem <- await
                case maybeItem of
                  Nothing -> return ()
                  Just expression -> do
                    let span = expressionSpan expression
                        position = sourceSpanStart span
                    liftIO $ IO.putStrLn
                     $ (IO.show $ sourcePositionLine position) ++ ":"
                       ++ (IO.show $ sourcePositionColumn position)
                       ++ " " ++ (T.unpack $ show expression)
                    yield $ Right expression
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
          => Consumer Diagnostic m ()
        explain = do
          maybeDiagnostic <- await
          case maybeDiagnostic of
            Nothing -> return ()
            Just diagnostic -> do
              liftIO $ outputDiagnostic options diagnostic
              explain


outputDiagnostic :: InvocationOptions -> Diagnostic -> IO ()
outputDiagnostic options diagnostic = do
  IO.putStrLn $ (T.unpack $ diagnosticHeadline diagnostic)


takeOptions :: [String] -> ([Diagnostic], InvocationOptions, [String])
takeOptions arguments =
  let loop ("--" : rest) diagnosticsSoFar optionsSoFar =
        (diagnosticsSoFar, optionsSoFar, rest)
      loop ("--help" : rest) diagnosticsSoFar optionsSoFar =
        loop rest diagnosticsSoFar $ optionsSoFar {
                        invocationOptionsMode = HelpInvocationMode
                      }
      loop ("--text" : rest) diagnosticsSoFar optionsSoFar =
        loop rest diagnosticsSoFar $ optionsSoFar {
                        invocationOptionsOutputFormat = TextOutputFormat
                      }
      loop ("--terminal" : rest) diagnosticsSoFar optionsSoFar =
        loop rest diagnosticsSoFar $ optionsSoFar {
                        invocationOptionsOutputFormat = TerminalOutputFormat
                      }
      loop ("--json" : rest) diagnosticsSoFar optionsSoFar =
        loop rest diagnosticsSoFar $ optionsSoFar {
                        invocationOptionsOutputFormat = JSONOutputFormat
                      }
      loop (option@('-' : _) : rest) diagnosticsSoFar optionsSoFar =
        loop rest (diagnosticsSoFar ++ [unknownOptionDiagnostic option])
             optionsSoFar
      loop rest diagnosticsSoFar optionsSoFar =
        (diagnosticsSoFar, optionsSoFar, rest)
  in loop arguments []
      $ InvocationOptions {
            invocationOptionsOutputFormat = TerminalOutputFormat,
            invocationOptionsOutputSourceSnippets = True,
            invocationOptionsMode = CompilationInvocationMode
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

