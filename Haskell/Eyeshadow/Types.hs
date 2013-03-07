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
   SExpression(..))
  where

import qualified Data.Text as T
import qualified Prelude as Prelude

import Prelude
  (Bool(..),
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

