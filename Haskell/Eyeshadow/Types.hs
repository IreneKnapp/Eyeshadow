module Eyeshadow.Types
  (Options(..),
   OutputFormat(..),
   SourceFileSpecification(..),
   SourcePosition(..),
   SourceSpan(..),
   Diagnostic(..))
  where

import qualified Data.Text as T


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
