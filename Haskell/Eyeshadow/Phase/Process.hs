{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleInstances,
             FlexibleContexts, UndecidableInstances #-}
module Eyeshadow.Phase.Process
  (MonadProcessing(..),
   ProcessingT,
   runProcessingT,
   process)
  where

import qualified Data.Map as Map
import qualified Data.Text as T

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.List
import Data.Maybe

import Eyeshadow.Data.FrontEnd
import Eyeshadow.Data.Name
import Eyeshadow.Data.Options
import Eyeshadow.Data.SExpression
import Eyeshadow.Data.Span
import Eyeshadow.Diagnostic
import Eyeshadow.Prelude


data ProcessingContext =
  ProcessingContext {
      processingContextModuleNamespace :: Map.Map NameComponent Term,
      processingContextTermNamespace :: Map.Map NameComponent Term,
      processingContextPredefinedValueNamespace :: Map.Map T.Text Term,
      processingContextCurrentModule :: Maybe Term
    }


class (Monad m) => MonadProcessing m where
  getProcessingContext :: m ProcessingContext
  putProcessingContext :: ProcessingContext -> m ()


data ProcessingT m a =
  InProcessingT {
      inProcessingTAction :: ProcessingContext -> m (ProcessingContext, a)
    }
instance (MonadIO m) => Monad (ProcessingT m) where
  (>>=) a b = InProcessingT $ \context -> do
    (context, v) <- inProcessingTAction a context
    inProcessingTAction (b v) context
  return a = InProcessingT $ \context -> return (context, a)
instance (MonadIO m) => MonadProcessing (ProcessingT m) where
  getProcessingContext = InProcessingT $ \context -> do
    return (context, context)
  putProcessingContext context = InProcessingT $ \_ -> do
    return (context, ())
instance MonadTrans ProcessingT where
  lift action = InProcessingT $ \context -> do
    result <- action
    return (context, result)
instance (MonadIO m) => MonadIO (ProcessingT m) where
  liftIO action = lift $ liftIO action
instance (MonadThrow m, MonadIO m) => MonadThrow (ProcessingT m) where
  monadThrow e = lift $ monadThrow e
instance (Functor f) => Functor (ProcessingT f) where
  fmap function functor = InProcessingT $ \context ->
    fmap (\(context, a) -> (context, function a))
         (inProcessingTAction functor context)
instance (Applicative f, MonadIO f) => Applicative (ProcessingT f) where
  pure a = return a
  (<*>) a b = ap a b
instance (MonadResource m) => MonadResource (ProcessingT m) where
  liftResourceT action = lift $ liftResourceT action


runProcessingT :: (MonadIO m) => ProcessingT m a -> m a
runProcessingT action = do
  (_, result) <- inProcessingTAction action initialProcessingContext
  return result


initialProcessingContext :: ProcessingContext
initialProcessingContext =
  ProcessingContext {
      processingContextModuleNamespace = Map.empty,
      processingContextTermNamespace = initialTermNamespace,
      processingContextPredefinedValueNamespace = Map.empty,
      processingContextCurrentModule = Nothing
    }


initialTermNamespace :: Map.Map NameComponent Term
initialTermNamespace =
  Map.fromList $ map
    (\(name, value) -> (NameComponent name, PredefinedValueTerm value))
    [("language", LanguageDeclarationPredefinedValue)]


languageNamespaces
  :: Map.Map T.Text (Map.Map NameComponent Term, Map.Map T.Text Term)
languageNamespaces =
  Map.fromList
    [("eyeshadow2013001",
      (eyeshadow2013001TermNamespace,
       eyeshadow2013001PredefinedValueNamespace))]


eyeshadow2013001TermNamespace :: Map.Map NameComponent Term
eyeshadow2013001TermNamespace =
  Map.fromList $ map
    (\(name, value) -> (NameComponent name, PredefinedValueTerm value))
    [("language", LanguageDeclarationPredefinedValue),
     ("file", FileDeclarationPredefinedValue),
     ("current-module", CurrentModuleDeclarationPredefinedValue),
     ("open-module", OpenModuleDeclarationPredefinedValue),
     ("republish-module", RepublishModuleDeclarationPredefinedValue),
     ("bind-module", BindModuleDeclarationPredefinedValue),
     ("visibility", VisibilityDeclarationPredefinedValue),
     ("signature", SignatureDeclarationPredefinedValue),
     ("value", ValueDeclarationPredefinedValue),
     ("predefined-value", PredefinedValueDeclarationPredefinedValue)]


eyeshadow2013001PredefinedValueNamespace :: Map.Map T.Text Term
eyeshadow2013001PredefinedValueNamespace =
  Map.fromList $ map
    (\(name, value) -> (name, PredefinedValueTerm value))
    [("type-0", TypePredefinedValue),
     ("type-n", TypeOfTypesPredefinedValue),
     ("name", NamePredefinedValue),
     ("declaration", DeclarationPredefinedValue),
     ("visibility", VisibilityPredefinedValue),
     ("numeric", NumericPredefinedValue),
     ("string", StringPredefinedValue),
     ("list", ListPredefinedValue),
     ("folder", FolderPredefinedValue),
     ("record", RecordTypePredefinedValue),
     ("record-type", RecordTypeToTypePredefinedValue),
     ("concatenate-record-types", ConcatenateRecordTypesPredefinedValue),
     ("map-record-type", MapRecordTypePredefinedValue),
     ("project-record-field", ProjectRecordFieldPredefinedValue),
     ("fold-record-type", FoldRecordTypePredefinedValue),
     ("language", LanguageDeclarationPredefinedValue),
     ("file", FileDeclarationPredefinedValue),
     ("current-module", CurrentModuleDeclarationPredefinedValue),
     ("open-module", OpenModuleDeclarationPredefinedValue),
     ("republish-module", RepublishModuleDeclarationPredefinedValue),
     ("bind-module", BindModuleDeclarationPredefinedValue),
     ("visibility", VisibilityDeclarationPredefinedValue),
     ("signature", SignatureDeclarationPredefinedValue),
     ("value", ValueDeclarationPredefinedValue),
     ("predefined-value", PredefinedValueDeclarationPredefinedValue),
     ("list", ListDeclarationPredefinedValue),
     ("macro-expand", MacroInvocationDeclarationPredefinedValue)]


process
  :: (MonadDiagnostic m, MonadProcessing m)
  => InvocationOptions
  -> FileSpecification
  -> Conduit SExpression m Declaration
process options file = do
  let collect soFar = do
        maybeExpression <- await
        case maybeExpression of
          Nothing -> loop soFar
          Just expression -> collect (soFar ++ [expression])
      loop [] = return ()
      loop (expression : rest) = do
        maybeExpression <- await
        case maybeExpression of
          Nothing -> return ()
          Just expression -> do
            lift $ processDeclaration options file expression
            process options file
  collect []


processDeclaration
  :: (MonadDiagnostic m, MonadProcessing m)
  => InvocationOptions
  -> FileSpecification
  -> SExpression
  -> m ()
processDeclaration options file expression = do
  case expression of
    SList _ ((SSymbol nameSpan name) : items) -> do
      maybeTerm <- getDeclarationName name
      case maybeTerm of
        Nothing -> diagnoseUnknownNameAtTopLevel file nameSpan
        Just term -> do
          return ()
    _ -> diagnoseInvalidFormAtTopLevel file (spanOf expression)


getDeclarationName
  :: (MonadProcessing m)
  => Name
  -> m (Maybe Term)
getDeclarationName (Name components) = do
  context <- getProcessingContext
  let namespace = processingContextTermNamespace context


diagnoseInvalidFormAtTopLevel
  :: (MonadDiagnostic m)
  => FileSpecification
  -> Span
  -> m ()
diagnoseInvalidFormAtTopLevel file span = diagnose $
  Diagnostic {
       diagnosticHeadline = "Invalid form at top-level",
       diagnosticDescription =
         T.concat
           ["Only module directives, declarations, and macro invocations ",
            "that expand to these are allowed at the top level of a file.  ",
            "Most likely the offending form is either a literal value, or an ",
            "attempt to take some action.  The only way to have your program ",
            "take an action is to bind that action to the name \"main\".  ",
            "The only appropriate remedy is to remove the offending form."],
       diagnosticDetails =
         [("Offending form", file, span)]
     }


diagnoseUnknownNameAtTopLevel
  :: (MonadDiagnostic m)
  => FileSpecification
  -> Span
  -> m ()
diagnoseUnknownNameAtTopLevel file span = diagnose $
  Diagnostic {
       diagnosticHeadline = "Unknown name at top-level",
       diagnosticDescription =
         T.concat
           ["This name is not known to me."],
       diagnosticDetails =
         [("Offending name", file, span)]
     }
