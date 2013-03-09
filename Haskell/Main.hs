{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, Rank2Types #-}
module Main (main) where

import qualified Data.Text as T
import qualified Prelude as IO
import qualified System.Environment as IO

import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Conduit
import Data.List
import Data.Maybe

import Eyeshadow.Data.FrontEnd
import Eyeshadow.Data.Options
import Eyeshadow.Data.SExpression
import Eyeshadow.Data.Span
import Eyeshadow.Phase.File
import Eyeshadow.Phase.Lexical
import Eyeshadow.Phase.Process
import Eyeshadow.Diagnostic
import Eyeshadow.Instances
import Eyeshadow.Prelude


main :: IO.IO ()
main = do
  arguments <- IO.getArgs
  let (diagnostics, options, arguments') = takeOptions arguments
  case (diagnostics, invocationOptionsMode options, arguments') of
    ([], HelpInvocationMode, _) ->
      runDiagnosticT (invocationOptionsDiagnostic options) $ do
        diagnose usageDiagnostic
    ([], CompilationInvocationMode, inputFilePaths@(_:_)) ->
      runDiagnosticT (invocationOptionsDiagnostic options) $ do
        mapM_ (\sourcePath -> runResourceT $ runProcessingT $ do
                 let file = FileFileSpecification sourcePath
                 readFile file $$ consume options file False)
              inputFilePaths
    ([], CompilationInvocationMode, []) -> do
      let file = TerminalFileSpecification
          loop = do
            runDiagnosticT (invocationOptionsDiagnostic options) $ do
              readTerminal file $$ consume options file True
            loop
      _ <- runProcessingT loop
      return ()
    _ -> runDiagnosticT (invocationOptionsDiagnostic options) $ do
           mapM_ diagnose diagnostics


consume
  :: (MonadIO m, MonadDiagnostic m, MonadProcessing m)
  => InvocationOptions
  -> FileSpecification
  -> Bool
  -> Sink (Char, Span) m ()
consume options file interactive =
  lex file
  =$ limitProcessingScopeWhenInteractive
  =$ process options file
  =$ showIt
  where limitProcessingScopeWhenInteractive
          :: (MonadIO m)
          => Conduit SExpression m SExpression
        limitProcessingScopeWhenInteractive = do
          maybeItem <- await
          case maybeItem of
            Nothing -> return ()
            Just item -> yield item
          if interactive
            then return ()
            else limitProcessingScopeWhenInteractive
        showIt
          :: (MonadIO m)
          => Sink a m ()
        showIt = do
          let loop = do
                maybeItem <- await
                case maybeItem of
                  Nothing -> return ()
                  Just item -> do
                    liftIO $ IO.putStrLn "Okay."
                    loop
          loop


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
                        invocationOptionsDiagnostic =
                          (invocationOptionsDiagnostic optionsSoFar) {
                              diagnosticOptionsOutputFormat =
                                TextDiagnosticOutputFormat
                            }
                      }
      loop ("--terminal" : rest) diagnosticsSoFar optionsSoFar =
        loop rest diagnosticsSoFar $ optionsSoFar {
                        invocationOptionsDiagnostic =
                          (invocationOptionsDiagnostic optionsSoFar) {
                              diagnosticOptionsOutputFormat =
                                TerminalDiagnosticOutputFormat
                            }
                      }
      loop ("--json" : rest) diagnosticsSoFar optionsSoFar =
        loop rest diagnosticsSoFar $ optionsSoFar {
                        invocationOptionsDiagnostic =
                          (invocationOptionsDiagnostic optionsSoFar) {
                              diagnosticOptionsOutputFormat =
                                JSONDiagnosticOutputFormat
                            }
                      }
      loop (option@('-' : _) : rest) diagnosticsSoFar optionsSoFar =
        loop rest (diagnosticsSoFar ++ [unknownOptionDiagnostic option])
             optionsSoFar
      loop rest diagnosticsSoFar optionsSoFar =
        (diagnosticsSoFar, optionsSoFar, rest)
  in loop arguments []
      $ InvocationOptions {
            invocationOptionsDiagnostic =
              DiagnosticOptions {
                  diagnosticOptionsOutputFormat =
                    TerminalDiagnosticOutputFormat,
                  diagnosticOptionsOutputSourceSnippets = True
                },
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

