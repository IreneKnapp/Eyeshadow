{-# LANGUAGE OverloadedStrings #-}
module Eyeshadow.Types
  (Show(..),
   InvocationMode(..),
   InvocationOptions(..),
   OutputFormat(..),
   SourceFileSpecification(..),
   SourcePosition(..),
   SourceSpan(..),
   Diagnostic(..),
   SExpression(..),
   Declaration(..),
   Name(..),
   NameComponent(..),
   Visibility(..),
   Term(..),
   Pattern(..),
   PredefinedValue(..),
   ProcessingContext(..))
  where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Prelude as Prelude

import Data.Maybe

import Prelude
  (Bool(..),
   Eq(..),
   Ord(..),
   FilePath,
   Int,
   map,
   ($))


class Show showable where
  show :: showable -> T.Text


data InvocationMode
  = HelpInvocationMode
  | CompilationInvocationMode


data InvocationOptions =
  InvocationOptions {
      invocationOptionsOutputFormat :: OutputFormat,
      invocationOptionsOutputSourceSnippets :: Bool,
      invocationOptionsMode :: InvocationMode
    }


data OutputFormat
  = TextOutputFormat
  | TerminalOutputFormat
  | JSONOutputFormat


data SourceFileSpecification
  = FileSourceFileSpecification FilePath
  | TerminalSourceFileSpecification


data SourcePosition =
  SourcePosition {
      sourcePositionByteOffset :: Int,
      sourcePositionCharacterOffset :: Int,
      sourcePositionLine :: Int,
      sourcePositionColumn :: Int
    }


data SourceSpan =
  SourceSpan {
      sourceSpanStart :: SourcePosition,
      sourceSpanEnd :: SourcePosition
    }


data Diagnostic =
  Diagnostic {
      diagnosticHeadline :: T.Text,
      diagnosticDescription :: T.Text,
      diagnosticDetails :: [(T.Text, SourceFileSpecification, SourceSpan)]
    }


data SExpression
  = SNumber SourceSpan T.Text
  | SString SourceSpan T.Text
  | SSymbol SourceSpan [T.Text]
  | SList SourceSpan [SExpression]
  | SQuoted SourceSpan SExpression
  | SQuasiquoted SourceSpan SExpression
  | SAntiquoted SourceSpan SExpression
instance Show SExpression where
  show (SNumber _ int) = int
  show (SString _ string) =
    T.concat ["\"",
              T.foldl' (\soFar c ->
                          T.concat [soFar,
                                    case c of
                                      '"' -> "\"\""
                                      _ -> T.singleton c])
                       string
              "\""]
  show (SSymbol _ parts) = T.intercalate ":" parts
  show (SList _ items) =
    T.concat ["(", T.intercalate " " $ map show items, ")"]
  show (SQuoted _ item) = T.concat ["'", show item]
  show (SQuasiquoted _ item) = T.concat ["`", show item]
  show (SAntiquoted _ item) = T.concat [",", show item]


data Declaration
  = LanguageDeclaration T.Text
  -- This replaces the declaration namespace with a set of bindings drawn from
  -- a set of possibilities predefined by the compiler; makes there be no
  -- current module; and replaces the definition namespace with a set of
  -- bindings also, like the new declaration namespace, drawn from a set of
  -- possibilities predefined by the compiler.  The choices of which
  -- predefined bindings to use are determined by the compiler at its sole
  -- discretion, with reference to the given string.
  -- 
  -- Initially, the declaration namespace consists of a single binding, which
  -- is just sufficient to invoke this declaration exactly once, thereby
  -- loading a language.
  
  | FileDeclaration FilePath
  -- All modules defined at the top level in the named file are loaded and
  -- become visible in the module namespace for the remainder of the source
  -- file or interactive session where this declaration appears.
  -- 
  -- Often one wishes to bind, and possibly to open, all modules within the
  -- file, as well as loading them into the module namespace; for this purpose
  -- there is a declaration macro.
  
  | CurrentModuleDeclaration Name
  -- A module with the given name is created if it does not already exist,
  -- and the current module is set to it for the remainder of the source file
  -- or interactive session where this declaration occurs, or until the next
  -- current-module declaration.  The name may be given as either absolute or
  -- relative, but may only be relative when there is an already-current
  -- module, which in that case is used as the base.
  -- 
  -- It might be nice to have this declaration also require the
  -- module to not previously exist, so that modules could not be defined
  -- across multiple files, but this would also prevent modules from
  -- aggragating submodules defined each in their own file and being children
  -- of itself.
  
  | OpenModuleDeclaration Name
  -- The given name is resolved relative to the current module, which must
  -- have been set, finding the module to which it refers, which must exist.
  -- All published definitions from the module thus identified become visible
  -- immediately within the declaration namespace of this module, with their
  -- names as given within that module.
  -- 
  -- Normally one also wishes to call the module as well as open it; for this
  -- purpose there is a declaration macro.
  
  | BindModuleDeclaration Name NameComponent Visibility
  -- The given name is resolved relative to the current module, which must
  -- have been set, finding the module to which it refers, which must exist.  
  -- A definition is added to the declaration namespace of the current module,
  -- with the given visibility, and the module thus identified as its value.
  -- 
  -- Often one also wishes to open the module as well as bind it; for this
  -- purpose there is a declaration macro.
  
  | VisibilityDeclaration NameComponent Visibility
  -- A definition is added to the declaration namespace of the current module,
  -- with the given visibility, and the signature and value definitions given
  -- elsewhere in this source file or interactive session.
  -- 
  -- One must always give a signature and some values for the definition, as
  -- well as specifying its visibility, and often wishes to do these together;
  -- for this purpose there is a declaration macro.
  -- 
  -- When giving a declaration in an interactive session, all three parts -
  -- visibility, signature, and definitions - must be given by a single
  -- top-level form; as such, declaration macros or list declarations must be
  -- used.
  
  | SignatureDeclaration NameComponent Term
  -- A definition is added to the declaration namespace of the current module,
  -- with the given signature, and the visibility specified and value
  -- definitions given elsewhere in this source file or interactive session.
  -- 
  -- One must always specify a visibility and give some values for the
  -- definition, as well as giving its signature, and often wishes to do
  -- these together; for this purpose there is a declaration macro.
  -- 
  -- When giving a declaration in an interactive session, all three parts -
  -- visibility, signature, and definitions - must be given by a single
  -- top-level form; as such, declaration macros or list declarations must be
  -- used.
  
  | ValueDeclaration NameComponent [Pattern] Term
  -- A definition is added to the declaration namespace of the current module,
  -- with the result of evaluating the given term as one of its value
  -- definitions, and the visibility specified, and signature and other
  -- value definitions given, elsewhere in this source file or interactive
  -- session.
  -- 
  -- One must always specify a visibility, give a signature, and give all
  -- values for the definition, and often wishes to do these together; for
  -- this purpose there is a declaration macro.
  -- 
  -- When giving a declaration in an interactive session, all three parts -
  -- visibility, signature, and definitions - must be given by a single
  -- top-level form; as such, declaration macros or list declarations must be
  -- used.
  
  | PredefinedValueDeclaration NameComponent PredefinedValue
  -- As ValueDeclaration, except that the definition added has as its value
  -- the given predefined value.
  -- 
  -- This is used in bootstrapping the system library, so that the system
  -- library has full control of all symbols in scope, rather than the
  -- compiler having to assume the existence of certain modules and symbols.
  -- Should there be alternatives to the system library, this could also be
  -- used by them.
  
  | ListDeclaration [Declaration]
  -- Each declaration given is processed, in the order given, and its
  -- effects are performed.
  -- 
  -- List declarations are useful primarily as the results of declaration
  -- macros.
  
  | MacroInvocationDeclaration Term
  -- The given term is evaluated and its result, which must be a declaration,
  -- is processed and its effects are performed.


data Name = Name [NameComponent]


data NameComponent = NameComponent T.Text
  deriving (Eq, Ord)


data Visibility
  = PublishedVisibility
  -- Definitions that are "published" are public as described below, and
  -- additionally are imported into other modules when the module publishing
  -- them is opened.
  | PublicVisibility
  -- Definitions that are "public" are visible at all times.  (Of course, per
  -- symbol-lookup rules, they must be qualified appropriately.)
  | PrivateVisibility
  -- Definitions that are "private" are visible only when the current module
  -- is the module they are defined in, or a child of it.


data Term
  = NumericTerm T.Text
  | StringTerm T.Text
  | ListTerm [Term]
  | QuotedTerm Term
  | QuasiquotedTerm Term
  | AntiquotedTerm Term
  | VariableTerm Name
  -- This evaluates to the value of the bound name in the current declaration
  -- namespace.
  | ApplicationTerm Term Term
  -- This evaluates to the result of applying the result of evaluating the
  -- first given term to the result of evaluating the second given term.
  | SignedTerm Term Term
  -- Typically used to augment the automatically-inferred type information,
  -- this evaluates to the result of evaluating the first given term, and has
  -- the second given term as its type.
  | RecordTerm (Map.Map Name Term)
  -- This may be either a record value, or a record type, depending upon the
  -- kind of the terms within the record.  Furthermore, modules and records
  -- are actually the same thing, so this may also be a module, with its
  -- "fields" being the definitions immediately within its declaration
  -- namespace.
  | DeclarationTerm Declaration
  -- Typically used as the return value of a declaration macro, this is a
  -- first-class declaration.  It evaluates to itself, and can only be used
  -- by being returned in a declaration context.
  | PredefinedValueTerm PredefinedValue


data Pattern
  = WildcardPattern
  | NumericPattern T.Text
  | StringPattern T.Text
  | ListPattern [Pattern]
  | QuotedPattern Pattern
  | QuasiquotedPattern Pattern
  | AntiquotedPattern Pattern
  | VariablePattern Name
  | ApplicationPattern Pattern Pattern


data PredefinedValue
  = TypePredefinedValue
  -- The first-order type of types.  Takes no parameters.
  
  | TypeOfTypesPredefinedValue
  -- The type family of higher-order types of types.  Takes one parameter,
  -- which is itself a type of types, and returns the type of its parameter.
  -- 
  -- There is thus an inductively defined, countably infinite chain of
  -- types of types of types.
   
  | NamePredefinedValue
  -- The type of names, an inhabitant of Type.
  
  | DeclarationPredefinedValue
  -- The type of declarations, an inhabitant of Type.

  | VisibilityPredefinedValue
  -- The type of visibility specifications, an inhabitant of Type.

  | NumericPredefinedValue
  -- The type of number-like values, an inhabitant of Type.

  | StringPredefinedValue
  -- The type of string-like values, an inhabitant of Type.
  
  | ListPredefinedValue
  -- The type family of list-like predefined values, inhabitants of Type.
  -- Takes one parameter, the type of the list contents.
  
  | FolderPredefinedValue
  -- The type family which allows operations on the fields of record type.
  -- A function taking one parameter, a record type, and returning a type
  -- which is inhabited by all the possible permutations of the fields of
  -- that record.  This is used to determine the order in which fields are
  -- visited when folding over the record.  This needs to be predefined
  -- because otherwise there would be a circular dependency, whereby a
  -- hypothetical non-predefined folder could only hold the requisite
  -- information by being a record, which itself could only be described by
  -- use of a folder.
  
  | RecordTypePredefinedValue
  -- Takes one parameter, a kind which is the kind of the fields of the
  -- record type; in the base case, this kind is Type.
  
  | RecordTypeToTypePredefinedValue
  -- Takes one parameter, a record type, and returns the inhabitant of Type
  -- which is inhabited by values described by that record type.
  
  | ConcatenateRecordTypesPredefinedValue
  -- An operation on record types.  Takes two parameters, both record types.
  -- Their field names must be disjoint (otherwise we would lose
  -- commutativity of this operation).
  
  | MapRecordTypePredefinedValue
  -- Takes a record type and a function to be applied to each item in it.
  
  | ProjectRecordFieldPredefinedValue
  -- Takes a name, a type, a record type, and a record, and returns the
  -- field with the given name and type from the record, which is of the
  -- given record type.
  
  | FoldRecordTypePredefinedValue
  -- Takes several parameters:
  -- A kind, the kind of the fields in the record to be operated on.
  -- A function from a record type with the given kind to a type, which is
  -- the type of the accumulator parameter to the fold.
  -- A type function, used at each step of the iteration, taking a name
  -- which is the name of the visited field, a value which is the value of
  -- the visited field, a record which is the portion of the input record
  -- already visited prior to this step, a proof that the given name does
  -- not occur in the already-visited record, and a value which is the
  -- value of the accumulator parameter from the previous step.
  -- A value, which is the initial value of the accumulator parameter (to
  -- be passed into the first step).
  -- A record, which is to be folded over.
  -- A folder for the given record - that is, a value inhabiting the type
  -- resulting from an application of the folder type family to that record.
  -- Returns a value of the type returned by the given type function when
  -- applied to the entire record, which of course is the same as the type
  -- returned by the final step of the accumulator.
  
  | LanguageDeclarationPredefinedValue
  -- Takes one parameter, a string.  Evaluates to a language declaration.
  
  | FileDeclarationPredefinedValue
  -- Takes one parameter, a string.  Evaluates to a file declaration.
  
  | CurrentModuleDeclarationPredefinedValue
  -- Takes one parameter, a name.  Evaluates to a current-module declaration.
  
  | OpenModuleDeclarationPredefinedValue
  -- Takes one parameter, a name.  Evaluates to an open-module declaration.
  
  | BindModuleDeclarationPredefinedValue
  -- Takes three parameters: a name; another name which must be relative and
  -- a single component only; and a visibility specification.  Evalutes to a
  -- bind-module declaration.
  
  | VisibilityDeclarationPredefinedValue
  -- Takes two parameters: a name which must be relative and a single
  -- component only, and a visibility specification.  Evaluates to a
  -- visibility declaration.
  
  | SignatureDeclarationPredefinedValue
  -- Takes two parameters: a name which must be relative and a single
  -- component only, and a term.  Evalutes to a signature declaration with
  -- the given name-component and the result of evaluating the given term,
  -- which must be a type.
  
  | ValueDeclarationPredefinedValue
  -- Takes three parameters: a name which must be relative and a single
  -- component only; a list of patterns; and a term.  Evaluates to a value
  -- declaration with the given name-component, patterns, and the result of
  -- evaluating the given term.
  
  | PredefinedValueDeclarationPredefinedValue
  -- Takes two parameters: a name which must be relative and a single
  -- component only, and a term.  Evaluates to a predefined-value declaration
  -- with the given name-component and the result of evaluating the given
  -- term.
  
  | ListDeclarationPredefinedValue
  -- Takes one parameter, a list of terms.  Evaluates to a list declaration
  -- with the results of evaluating the given terms.
  
  | MacroInvocationDeclarationPredefinedValue
  -- Takes one parameter, a term.  Evaluates to a macro-invocation declaration
  -- with the result of evaluating the given term.


data ProcessingContext =
  ProcessingContext {
      processingContextModuleNamespace :: Map.Map NameComponent Term,
      processingContextDeclarationNamespace :: Map.Map NameComponent Term,
      processingContextDefinitionNamespace :: Map.Map NameComponent Term,
      processingContextCurrentModule :: Maybe Term
    }

