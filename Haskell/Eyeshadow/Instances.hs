{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Eyeshadow.Instances () where

import Control.Monad.Trans

import Eyeshadow.Phase.Process
import Eyeshadow.Diagnostic
import Eyeshadow.Prelude


instance (MonadDiagnostic m) => MonadDiagnostic (ProcessingT m) where
  diagnose diagnostic = lift $ diagnose diagnostic

instance (MonadIO m, MonadProcessing m)
         => MonadProcessing (DiagnosticT m) where
  getProcessingContext = lift $ getProcessingContext
  putProcessingContext context = lift $ putProcessingContext context
