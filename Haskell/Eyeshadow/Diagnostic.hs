{-# LANGUAGE NoImplicitPrelude #-}
module Eyeshadow.Diagnostic
  (DiagnosticOptions(..),
   DiagnosticOutputFormat(..),
   Diagnostic(..),
   MonadDiagnostic(..),
   DiagnosticT,
   runDiagnosticT)
  where

import qualified Data.Conduit as Conduit
import qualified Data.Text as T
import qualified System.IO as IO

import Control.Monad
import Control.Monad.Trans
import Data.List

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
      diagnosticHeadline :: T.Text,
      diagnosticDescription :: T.Text,
      diagnosticDetails :: [(T.Text, FileSpecification, Span)]
    }


class (Monad m) => MonadDiagnostic m where
  diagnose :: Diagnostic -> m ()
instance (MonadDiagnostic m) => MonadDiagnostic (Conduit.ConduitM i o m) where
  diagnose diagnostic = lift $ diagnose diagnostic


data DiagnosticT m a =
  InDiagnosticT {
      inDiagnosticTAction :: [Diagnostic] -> m ([Diagnostic], a)
    }
instance (MonadIO m) => Monad (DiagnosticT m) where
  (>>=) a b = InDiagnosticT $ \diagnostics -> do
    (diagnostics, v) <- inDiagnosticTAction a diagnostics
    inDiagnosticTAction (b v) diagnostics
  return a = InDiagnosticT $ \diagnostics -> return (diagnostics, a)
instance MonadTrans DiagnosticT where
  lift action = InDiagnosticT $ \diagnostics -> do
    result <- action
    return (diagnostics, result)
instance (MonadIO m) => MonadDiagnostic (DiagnosticT m) where
  diagnose diagnostic = InDiagnosticT $ \diagnostics -> do
    return (diagnostics ++ [diagnostic], ())
instance (MonadIO m) => MonadIO (DiagnosticT m) where
  liftIO action = lift $ liftIO action


runDiagnosticT :: (MonadIO m) => DiagnosticOptions -> DiagnosticT m a -> m a
runDiagnosticT options action = do
  (diagnostics, result) <- inDiagnosticTAction action []
  mapM_ (\diagnostic -> do
           liftIO $ IO.putStrLn $ (T.unpack $ diagnosticHeadline diagnostic))
        diagnostics
  return result

