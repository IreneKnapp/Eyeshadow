{-# LANGUAGE NoImplicitPrelude #-}
module Eyeshadow.Data.Span
  (FileSpecification(..),
   Scan(..),
   Span(..),
   HasSpan(..),
   concatenateSpans,
   startScan,
   advanceScan,
   advanceScanByEncodingError)
  where

import qualified Prelude

import Data.Char
import Data.Functor
import Data.List
import Data.Maybe

import Eyeshadow.Prelude


data FileSpecification
  = FileFileSpecification Prelude.FilePath
  | TerminalFileSpecification


data Scan =
  Scan {
      scanByteOffset :: Int,
      scanCharacterOffset :: Int,
      scanLine :: Int,
      scanColumn :: Int
    }


data Span =
  Span {
      spanByteOffset :: Int,
      spanByteLength :: Int,
      spanCharacterOffset :: Int,
      spanCharacterLength :: Int,
      spanStartLine :: Int,
      spanEndLine :: Int,
      spanStartColumn :: Int,
      spanEndColumn :: Int
    }


class HasSpan item where
  spanOf :: item -> Span


concatenateSpans :: [Span] -> Maybe Span
concatenateSpans spans =
  let maybeFirstAndLast =
        if length spans > 0
          then Just (head spans, last spans)
          else Nothing
      result =
        fmap (\(first, last) ->
                 Span {
                     spanByteOffset = spanByteOffset first,
                     spanByteLength =
                       spanByteOffset last + spanByteLength last
                       - spanByteOffset first,
                     spanCharacterOffset = spanCharacterOffset first,
                     spanCharacterLength =
                       spanCharacterOffset last + spanCharacterLength last
                       - spanCharacterOffset first,
                     spanStartLine = spanStartLine first,
                     spanEndLine = spanEndLine last,
                     spanStartColumn = spanStartColumn first,
                     spanEndColumn = spanEndColumn last
                   })
             maybeFirstAndLast
  in result


startScan :: Scan
startScan =
  Scan {
      scanByteOffset = 0,
      scanCharacterOffset = 0,
      scanLine = 1,
      scanColumn = 1
    }


advanceScan :: Scan -> Char -> Int -> (Scan, Span)
advanceScan oldScan c byteLength =
  let isNewline = c == '\n'
  in (Scan {
          scanByteOffset = scanByteOffset oldScan + byteLength,
          scanCharacterOffset = scanCharacterOffset oldScan + 1,
          scanLine = if isNewline
                       then scanLine oldScan + 1
                       else scanLine oldScan,
          scanColumn = if isNewline
                         then 1
                         else scanColumn oldScan + 1
        },
      Span {
          spanByteOffset = scanByteOffset oldScan,
          spanByteLength = byteLength,
          spanCharacterOffset = scanCharacterOffset oldScan,
          spanCharacterLength = 1,
          spanStartLine = scanLine oldScan,
          spanEndLine = scanLine oldScan,
          spanStartColumn = scanColumn oldScan,
          spanEndColumn = scanColumn oldScan + 1
        })


advanceScanByEncodingError :: Scan -> Int -> (Scan, Span)
advanceScanByEncodingError oldScan byteLength =
  (Scan {
       scanByteOffset = scanByteOffset oldScan + byteLength,
       scanCharacterOffset = scanCharacterOffset oldScan,
       scanLine = scanLine oldScan,
       scanColumn = scanColumn oldScan
     },
   Span {
       spanByteOffset = scanByteOffset oldScan,
       spanByteLength = byteLength,
       spanCharacterOffset = scanCharacterOffset oldScan,
       spanCharacterLength = 0,
       spanStartLine = scanLine oldScan,
       spanEndLine = scanLine oldScan,
       spanStartColumn = scanColumn oldScan,
       spanEndColumn = scanColumn oldScan + 1
     })

