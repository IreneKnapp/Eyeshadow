module Eyeshadow.Spans
  (concatenateSpans,
   extendSpanBackward,
   extendSpanForward,
   expressionSpan,
   singletonSpan,
   simpleSpan)
  where

import Eyeshadow.Types


concatenateSpans :: [SourceSpan] -> Maybe SourceSpan
concatenateSpans spans =
  let maybeFirstAndLast =
        if length spans > 0
          then Just (head spans, last spans)
          else Nothing
      result = fmap (\(first, last) ->
                        SourceSpan {
                            sourceSpanStart = sourceSpanStart first,
                            sourceSpanEnd = sourceSpanEnd last
                          })
                    maybeFirstAndLast
  in result


extendSpanBackward :: SourcePosition -> SourceSpan -> SourceSpan
extendSpanBackward start span =
  SourceSpan {
      sourceSpanStart = start,
      sourceSpanEnd = sourceSpanEnd span
    }


extendSpanForward :: SourceSpan -> SourcePosition -> SourceSpan
extendSpanForward span end =
  SourceSpan {
      sourceSpanStart = sourceSpanStart span,
      sourceSpanEnd = end
    }


expressionSpan :: SExpression -> SourceSpan
expressionSpan (SNumber span _) = span
expressionSpan (SString span _) = span
expressionSpan (SSymbol span _) = span
expressionSpan (SList span _) = span
expressionSpan (SQuoted span _) = span
expressionSpan (SQuasiquoted span _) = span
expressionSpan (SAntiquoted span _) = span


singletonSpan :: SourcePosition -> SourceSpan
singletonSpan position = simpleSpan position position


simpleSpan :: SourcePosition -> SourcePosition -> SourceSpan
simpleSpan start end =
  SourceSpan {
      sourceSpanStart = start,
      sourceSpanEnd = end
    }
