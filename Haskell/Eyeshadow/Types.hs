{-# LANGUAGE OverloadedStrings #-}
module Eyeshadow.Types
  (Show(..),
   Options(..),
   OutputFormat(..),
   SourceFileSpecification(..),
   SourcePosition(..),
   SourceSpan(..),
   Diagnostic(..),
   SExpression)
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


data Options =
  Options {
      optionsOutputFormat :: OutputFormat,
      optionsOutputSourceSnippets :: Bool
    }

data OutputFormat
  = TextOutputFormat
  | TerminalOutputFormat
  | JSONOutputFormat

data SourceFileSpecification =
  SourceFileSpecification FilePath


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
  = SInteger Int
  | SSymbol [T.Text]
  | SList [SExpression]
  | SQuoted SExpression
  | SQuasiquoted SExpression
  | SAntiquoted SExpression
instance Show SExpression where
  show (SInteger int) = T.pack $ Prelude.show int
  show (SSymbol parts) = T.intercalate ":" parts
  show (SList items) = T.concat ["(", T.intercalate " " $ map show items, ")"]
  show (SQuoted item) = T.concat ["'", show item]
  show (SQuasiquoted item) = T.concat ["`", show item]
  show (SAntiquoted item) = T.concat [",", show item]

