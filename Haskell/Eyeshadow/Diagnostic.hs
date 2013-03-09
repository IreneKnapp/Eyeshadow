{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, FlexibleContexts,
             MultiParamTypeClasses, TypeFamilies #-}
module Eyeshadow.Diagnostic
  (DiagnosticOptions(..),
   DiagnosticOutputFormat(..),
   Diagnostic(..),
   MonadDiagnostic(..),
   DiagnosticT,
   runDiagnosticT)
  where

import qualified Data.Text as T
import qualified System.IO as IO

import Data.Conduit
import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
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


class (MonadIO m) => MonadDiagnostic m where
  diagnose :: Diagnostic -> m ()
instance (MonadDiagnostic m) => MonadDiagnostic (ConduitM i o m) where
  diagnose diagnostic = lift $ diagnose diagnostic
instance (MonadDiagnostic m) => MonadDiagnostic (ResourceT m) where
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
instance (MonadThrow m, MonadIO m) => MonadThrow (DiagnosticT m) where
  monadThrow e = lift $ monadThrow e
instance (Functor f) => Functor (DiagnosticT f) where
  fmap function functor = InDiagnosticT $ \diagnostics ->
    fmap (\(diagnostics, a) -> (diagnostics, function a))
         (inDiagnosticTAction functor diagnostics)
instance (Applicative f, MonadIO f) => Applicative (DiagnosticT f) where
  pure a = return a
  (<*>) a b = ap a b
instance (MonadResource m) => MonadResource (DiagnosticT m) where
  liftResourceT action = lift $ liftResourceT action
instance (MonadIO m, MonadBase IO.IO m)
         => MonadBase IO.IO (DiagnosticT m) where
  liftBase action = lift $ liftBase action
instance MonadTransControl DiagnosticT where
  data StT DiagnosticT a =
         StDiagnostic { stDiagnosticState :: ([Diagnostic], a) }
  liftWith action = InDiagnosticT $ \diagnostics -> do
      result <- action $ \subaction -> do
        (diagnostics, result) <- inDiagnosticTAction subaction diagnostics
        return $ StDiagnostic (diagnostics, result)
      return (diagnostics, result)
  restoreT action = InDiagnosticT $ \oldDiagnostics -> do
    result <- action
    let (newDiagnostics, innerResult) = stDiagnosticState result
    return (oldDiagnostics ++ newDiagnostics, innerResult)
instance (MonadIO m, MonadBaseControl IO.IO m)
         => MonadBaseControl IO.IO (DiagnosticT m) where
  data StM (DiagnosticT m) a =
         StMDiagnostic { stMDiagnosticAction :: ComposeSt DiagnosticT m a }
  liftBaseWith = defaultLiftBaseWith StMDiagnostic
  restoreM = defaultRestoreM stMDiagnosticAction


runDiagnosticT :: (MonadIO m) => DiagnosticOptions -> DiagnosticT m a -> m a
runDiagnosticT options action = do
  (diagnostics, result) <- inDiagnosticTAction action []
  mapM_ (\diagnostic -> do
           liftIO $ IO.putStrLn $ (T.unpack $ diagnosticHeadline diagnostic))
        diagnostics
  return result

