{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleInstances,
             FlexibleContexts, UndecidableInstances #-}
module Eyeshadow.Phase.Process
  (MonadProcessing(..),
   ProcessingT,
   runProcessingT,
   process)
  where

import qualified Data.Map as Map

import Control.Monad
import Control.Monad.Trans
import Data.Conduit
import Data.Maybe

import Eyeshadow.Data.SExpression
import Eyeshadow.Data.Span
import Eyeshadow.Diagnostic
import Eyeshadow.Prelude
import Eyeshadow.Types


data ProcessingContext =
  ProcessingContext {
      processingContextModuleNamespace :: Map.Map NameComponent Term,
      processingContextDeclarationNamespace :: Map.Map NameComponent Term,
      processingContextDefinitionNamespace :: Map.Map NameComponent Term,
      processingContextCurrentModule :: Maybe Term
    }


class (Monad m) => MonadProcessing m where
  getProcessingContext :: m ProcessingContext
  putProcessingContext :: ProcessingContext -> m ()
instance (MonadProcessing m, MonadTrans t, Monad (t m))
         => MonadProcessing (t m) where
  getProcessingContext = lift $ getProcessingContext
  putProcessingContext context = lift $ putProcessingContext context


data ProcessingT m a =
  InProcessingT {
      inProcessingTAction :: ProcessingContext -> m (ProcessingContext, a)
    }
instance (MonadIO m) => Monad (ProcessingT m) where
  (>>=) a b = InProcessingT $ \context -> do
    (context, v) <- inProcessingTAction a context
    inProcessingTAction (b v) context
  return a = InProcessingT $ \context -> return (context, a)
instance MonadTrans ProcessingT where
  lift action = InProcessingT $ \context -> do
    result <- action
    return (context, result)
instance (MonadIO m) => MonadIO (ProcessingT m) where
  liftIO action = lift $ liftIO action


runProcessingT :: (MonadIO m) => ProcessingT m a -> m a
runProcessingT action = do
  (_, result) <- inProcessingTAction action initialProcessingContext
  return result


initialProcessingContext :: ProcessingContext
initialProcessingContext =
  ProcessingContext {
      processingContextModuleNamespace = Map.empty,
      processingContextDeclarationNamespace = initialDeclarationNamespace,
      processingContextDefinitionNamespace = Map.empty,
      processingContextCurrentModule = Nothing
    }


initialDeclarationNamespace :: Map.Map NameComponent Term
initialDeclarationNamespace =
  Map.fromList
    [(NameComponent "language",
      PredefinedValueTerm LanguageDeclarationPredefinedValue)]


process
  :: (MonadDiagnostic m, MonadProcessing m)
  => InvocationOptions
  -> FileSpecification
  -> Conduit SExpression m Declaration
process options file = do
  maybeExpression <- await
  case maybeExpression of
    Nothing -> return ()
    Just expression -> do
      process options file

