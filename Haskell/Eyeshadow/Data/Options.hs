{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Eyeshadow.Data.Options
  (InvocationMode(..),
   InvocationOptions(..))
  where

import Eyeshadow.Diagnostic
import Eyeshadow.Prelude
   

data InvocationMode
  = HelpInvocationMode
  | CompilationInvocationMode


data InvocationOptions =
  InvocationOptions {
      invocationOptionsDiagnostic :: DiagnosticOptions,
      invocationOptionsMode :: InvocationMode
    }
