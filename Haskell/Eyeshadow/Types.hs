module Eyeshadow.Types
  (SourcePosition(..),
   SourceSpan(..),
   Diagnostic(..))
  where

import qualified Data.Text as T


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
      diagnosticDetails :: [(T.Text, SourceSpan)]
    }

