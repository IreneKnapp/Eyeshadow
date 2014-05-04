{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleInstances,
             FlexibleContexts, UndecidableInstances, RecordWildCards,
             TypeOperators, DeriveDataTypeable #-}
module Eyeshadow.Phase.Process
  (Process,
   runProcess,
   process)
  where

import qualified Control.Eff as Eff
import qualified Control.Eff.State.Strict as Eff
import qualified Control.Monad.Trans as Conduit
import qualified Data.Conduit as Conduit
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Data.List (map, (++))
import Data.Maybe
import Data.Typeable

import Eyeshadow.Data.FrontEnd
import Eyeshadow.Data.Name
import Eyeshadow.Data.Options
import Eyeshadow.Data.SExpression
import Eyeshadow.Data.Span
import Eyeshadow.Diagnostic
import Eyeshadow.Prelude


data ProcessingContext =
  ProcessingContext {
      processingContextModuleNamespace :: Namespace,
      processingContextTermNamespace :: Namespace,
      processingContextPredefinedValueNamespace :: PredefinedValueNamespace,
      processingContextCurrentModule :: Maybe Term
    }
  deriving (Typeable)


type Process = Eff.State ProcessingContext


initialProcessingContext :: ProcessingContext
initialProcessingContext =
  ProcessingContext {
      processingContextModuleNamespace = Namespace HashMap.empty,
      processingContextTermNamespace = initialTermNamespace,
      processingContextPredefinedValueNamespace =
        PredefinedValueNamespace HashMap.empty,
      processingContextCurrentModule = Nothing
    }


initialTermNamespace :: Namespace
initialTermNamespace =
  Namespace $ HashMap.fromList $ map
    (\(name, value) ->
      (NameComponent name, (Just $ PredefinedValueTerm value, Nothing)))
    [("language", LanguageDeclarationPredefinedValue)]


languageNamespaces :: LanguageNamespace
languageNamespaces =
  LanguageNamespace $ HashMap.fromList
    [("eyeshadow2014001",
      (eyeshadow2014001TermNamespace,
       eyeshadow2014001PredefinedValueNamespace))]


eyeshadow2014001TermNamespace :: Namespace
eyeshadow2014001TermNamespace =
  Namespace $ HashMap.fromList $ map
    (\(name, value) ->
      (NameComponent name,
       (Just $ PredefinedValueTerm value,
        Nothing)))
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


eyeshadow2014001PredefinedValueNamespace :: PredefinedValueNamespace
eyeshadow2014001PredefinedValueNamespace =
  PredefinedValueNamespace $ HashMap.fromList $ map
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



runProcess :: Eff.Eff (Eff.State ProcessingContext Eff.:> r) a
           -> Eff.Eff r a
runProcess action = do
  (_, result) <- Eff.runState initialProcessingContext action
  return result


process
  :: (Eff.Member Diagnose r,
      Eff.Member (Eff.State ProcessingContext) r)
  => InvocationOptions
  -> FileSpecification
  -> Conduit.Conduit SExpression (Eff.Eff r) Declaration
process options file = Conduit.awaitForever $ \expression -> do
 Conduit.lift $ processDeclaration options file expression


processDeclaration
  :: (Eff.Member (Eff.State ProcessingContext) r,
      Eff.Member Diagnose r)
  => InvocationOptions
  -> FileSpecification
  -> SExpression
  -> Eff.Eff r ()
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
  :: (Eff.Member (Eff.State ProcessingContext) r)
  => Name
  -> Eff.Eff r (Maybe Term)
getDeclarationName (Name components) = do
  ProcessingContext{..} <- Eff.get
  let consider :: [NameComponent]
               -> Maybe Term
               -> Namespace
               -> Maybe Term
      consider [] maybeResult _ = maybeResult
      consider [component] _ (Namespace namespace) =
        case HashMap.lookup component namespace of
          Just (maybeTerm, _) -> maybeTerm
          Nothing -> Nothing
      consider (component : rest) _ (Namespace namespace) =
        case HashMap.lookup component namespace of
          Just (maybeTerm, Just subNamespace) ->
            consider rest maybeTerm subNamespace
          _ -> Nothing
  return $ consider components Nothing processingContextTermNamespace


diagnoseInvalidFormAtTopLevel
  :: (Eff.Member Diagnose r)
  => FileSpecification
  -> Span
  -> Eff.Eff r ()
diagnoseInvalidFormAtTopLevel file span = diagnose $
  Diagnostic {
       diagnosticHeadline = "Invalid form at top-level",
       diagnosticDescription =
         Text.concat
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
  :: (Eff.Member Diagnose r)
  => FileSpecification
  -> Span
  -> Eff.Eff r ()
diagnoseUnknownNameAtTopLevel file span = diagnose $
  Diagnostic {
       diagnosticHeadline = "Unknown name at top-level",
       diagnosticDescription =
         Text.concat
           ["This name is not known to me."],
       diagnosticDetails =
         [("Offending name", file, span)]
     }
