{-# LANGUAGE OverloadedStrings #-}
module Eyeshadow.Process
  (initialProcessingContext,
   process)
  where

import qualified Data.Map as Map

import Data.Conduit

import Eyeshadow.Types


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
  :: (Monad m)
  => InvocationOptions
  -> SourceFileSpecification
  -> ProcessingContext
  -> ConduitM SExpression
              (Either Diagnostic Declaration)
              m
              ProcessingContext
process options file processingContext = do
  maybeExpression <- await
  case maybeExpression of
    Nothing -> return processingContext
    Just expression -> do
      process options file processingContext

