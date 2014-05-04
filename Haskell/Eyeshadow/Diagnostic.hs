{-# LANGUAGE NoImplicitPrelude, TypeOperators, DeriveDataTypeable,
             DeriveFunctor, FlexibleContexts, RecordWildCards #-}
module Eyeshadow.Diagnostic
  (DiagnosticOptions(..),
   DiagnosticOutputFormat(..),
   Diagnostic(..),
   Diagnose,
   diagnose,
   runDiagnose)
  where

import qualified Data.Text as Text
import qualified System.IO as IO
import qualified Control.Eff as Eff
import qualified Control.Eff.Lift as Eff

import Control.Monad
import Data.List ((++))
import Data.Typeable

import Eyeshadow.Data.Span
import Eyeshadow.Prelude


data DiagnosticOptions =
  DiagnosticOptions {
      diagnosticOptionsOutputFormat :: DiagnosticOutputFormat,
      diagnosticOptionsOutputSourceSnippets :: Bool
    }


data DiagnosticOutputFormat
  = TextDiagnosticOutputFormat
  | TerminalDiagnosticOutputFormat
  | JSONDiagnosticOutputFormat


data Diagnostic =
  Diagnostic {
      diagnosticHeadline :: Text.Text,
      diagnosticDescription :: Text.Text,
      diagnosticDetails :: [(Text.Text, FileSpecification, Span)]
    }


data Diagnose v = Diagnose Diagnostic v
  deriving (Typeable, Functor)


diagnose :: Eff.Member Diagnose r
         => Diagnostic
         -> Eff.Eff r ()
diagnose diagnostic =
  Eff.send $ \perform -> Eff.inj $ Diagnose diagnostic $ perform ()


runDiagnose :: Eff.SetMember Eff.Lift (Eff.Lift IO.IO) r
            => DiagnosticOptions
            -> Eff.Eff (Diagnose Eff.:>r) a
            -> Eff.Eff r a
runDiagnose DiagnosticOptions{..} topLevelAction =
  let loop soFar (Eff.E request) =
        Eff.handleRelay request (loop soFar) $ \(Diagnose diagnostic value) ->
          loop (soFar ++ [diagnostic]) value
      loop diagnostics (Eff.Val result) = do
        mapM_ (\Diagnostic{..} -> do
                 Eff.lift $ IO.putStrLn $ (Text.unpack diagnosticHeadline))
              diagnostics
        return result
  in loop [] (Eff.admin topLevelAction)
