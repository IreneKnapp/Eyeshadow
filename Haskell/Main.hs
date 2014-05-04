{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, Rank2Types,
             FlexibleContexts #-}
module Main (main) where

import qualified Control.Eff as Eff
import qualified Control.Eff.Lift as Eff
import qualified Control.Eff.Resource as Eff
import qualified Control.Monad.Trans as Conduit
import qualified Data.Conduit as Conduit
import qualified Data.Text as Text
import qualified Prelude as IO
import qualified System.Environment as IO

import Control.Monad
import Data.Char
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
import Eyeshadow.Prelude


main :: IO.IO ()
main = do
  arguments <- IO.getArgs
  let (diagnostics, options, arguments') = takeOptions arguments
  case (diagnostics, invocationOptionsMode options, arguments') of
    ([], HelpInvocationMode, _) ->
      Eff.runLift $ runDiagnose (invocationOptionsDiagnostic options) $ do
        diagnose usageDiagnostic
    ([], CompilationInvocationMode, inputFilePaths@(_:_)) ->
      Eff.runLift $ runDiagnose (invocationOptionsDiagnostic options) $ do
        mapM_ (\sourcePath -> Eff.runResource $ runProcess $ do
                 let file = FileFileSpecification sourcePath
                 readFile file Conduit.$$ consume options file False)
              inputFilePaths
    ([], CompilationInvocationMode, []) -> do
      let file = TerminalFileSpecification
          loop = do
            runDiagnose (invocationOptionsDiagnostic options) $ do
              readTerminal file Conduit.$$ consume options file True
            loop
      _ <- Eff.runLift $ runProcess loop
      return ()
    _ -> Eff.runLift $ runDiagnose (invocationOptionsDiagnostic options) $ do
           mapM_ diagnose diagnostics


consume
  :: (Eff.SetMember Eff.Lift (Eff.Lift IO.IO) r,
      Eff.Member Diagnose r,
      Eff.Member Process r)
  => InvocationOptions
  -> FileSpecification
  -> Bool
  -> Conduit.Sink (Char, Span) (Eff.Eff r) ()
consume options file interactive =
  lex file
  Conduit.=$ limitProcessingScopeWhenInteractive
  Conduit.=$ process options file
  Conduit.=$ showIt
  where limitProcessingScopeWhenInteractive
          :: (Eff.SetMember Eff.Lift (Eff.Lift IO.IO) r)
          => Conduit.Conduit SExpression (Eff.Eff r) SExpression
        limitProcessingScopeWhenInteractive = do
          maybeItem <- Conduit.await
          case maybeItem of
            Nothing -> return ()
            Just item -> Conduit.yield item
          if interactive
            then return ()
            else limitProcessingScopeWhenInteractive
        showIt
          :: (Eff.SetMember Eff.Lift (Eff.Lift IO.IO) r)
          => Conduit.Sink a (Eff.Eff r) ()
        showIt = do
          let loop = do
                maybeItem <- Conduit.await
                case maybeItem of
                  Nothing -> return ()
                  Just item -> do
                    Conduit.lift $ Eff.lift $ IO.putStrLn "Okay."
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
      loop ("--snippets" : rest) diagnosticsSoFar optionsSoFar =
        loop rest diagnosticsSoFar $ optionsSoFar {
                        invocationOptionsDiagnostic =
                          (invocationOptionsDiagnostic optionsSoFar) {
                              diagnosticOptionsOutputSourceSnippets =
                                True
                            }
                      }
      loop ("--no-snippets" : rest) diagnosticsSoFar optionsSoFar =
        loop rest diagnosticsSoFar $ optionsSoFar {
                        invocationOptionsDiagnostic =
                          (invocationOptionsDiagnostic optionsSoFar) {
                              diagnosticOptionsOutputSourceSnippets =
                                False
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
         Text.intercalate "\n"
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
       diagnosticHeadline = Text.concat ["Unknown option: ", Text.pack option],
       diagnosticDescription =
         Text.concat
           ["This indicates that you (or a shell script) provided a ",
            "command-line option which does not exist with that name in ",
            "this version of the Eyeshadow compiler.  For safety's sake, ",
            "the compiler will take no further action; if you're certain ",
            "the option has no effects upon which you are depending, simply ",
            "try again without it."],
       diagnosticDetails = []
     }

