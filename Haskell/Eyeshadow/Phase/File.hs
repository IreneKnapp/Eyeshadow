{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, Rank2Types #-}
module Eyeshadow.Phase.File
  (readFile,
   readTerminal)
  where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified System.IO as IO

import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.Conduit
import Data.Conduit.Binary
import Data.Either
import Data.Maybe
import Data.Word

import Eyeshadow.Data.Span
import Eyeshadow.Data.UTF8
import Eyeshadow.Diagnostic
import Eyeshadow.Prelude


readFile
  :: (MonadResource m, MonadDiagnostic m)
  => FileSpecification
  -> Source m (Char, Span)
readFile file = do
  case file of
    FileFileSpecification filePath -> do
      sourceFile filePath $= toBytes $= addSpans file
    _ -> return ()


readTerminal
  :: (MonadIO m, MonadDiagnostic m)
  => FileSpecification
  -> Source m (Char, Span)
readTerminal file = do
  let loop = do
        liftIO $ IO.putStr "> "
        liftIO $ IO.hFlush IO.stdout
        line <- liftIO BS.getLine
        yield line
        yield $ BS.singleton $ fromIntegral $ ord '\n'
        loop
  loop $= toBytes $= addSpans file


toBytes :: (Monad m) => Conduit BS.ByteString m Word8
toBytes = do
  maybeByteString <- await
  case maybeByteString of
    Nothing -> return ()
    Just byteString -> do
      mapM_ yield $ BS.unpack byteString
      toBytes


addSpans
  :: (MonadDiagnostic m)
  => FileSpecification
  -> Conduit Word8 m (Char, Span)
addSpans file = do
  let loop
        :: (MonadDiagnostic m)
        => Scan -> BS.ByteString -> Conduit Word8 m (Char, Span)
      loop scan byteString = do
        case decode byteString of
          Left InvalidDataDecodingFailure -> do
            let (newScan, span) =
                  advanceScanByEncodingError scan (BS.length byteString)
            diagnoseInvalidUTF8 file span
            loop newScan BS.empty
          Left InsufficientDataDecodingFailure -> do
            maybeByte <- await
            case maybeByte of
              Nothing -> do
                if BS.null byteString
                  then return ()
                  else let (_, span) =
                             advanceScanByEncodingError scan
                               (BS.length byteString)
                       in diagnoseUnexpectedEndOfFileInUTF8 file span
              Just byte -> loop scan $ BS.snoc byteString byte
          Right (c, _) -> do
            let (newScan, span) = advanceScan scan c (BS.length byteString)
            yield (c, span)
            loop newScan BS.empty
  loop startScan BS.empty


diagnoseInvalidUTF8
  :: (MonadDiagnostic m)
  => FileSpecification
  -> Span
  -> m ()
diagnoseInvalidUTF8 file span = diagnose $
  Diagnostic {
      diagnosticHeadline = "Invalid UTF-8",
      diagnosticDescription =
        T.concat
          ["This implies that the source file is not valid UTF-8, ",
           "possibly because it has been truncated as by an ",
           "incomplete download.  Verify the integrity of the file."],
      diagnosticDetails =
        [("Invalid character", file, span)]
    }


diagnoseUnexpectedEndOfFileInUTF8
  :: (MonadDiagnostic m)
  => FileSpecification
  -> Span
  -> m ()
diagnoseUnexpectedEndOfFileInUTF8 file span = diagnose $
  Diagnostic {
       diagnosticHeadline = "Invalid UTF-8 (truncated by end-of-file)",
       diagnosticDescription =
         T.concat
           ["This implies that the source file is not valid UTF-8, ",
            "possibly because it has been truncated as by an ",
            "incomplete download.  Verify the integrity of the file."],
       diagnosticDetails =
         [("Invalid character", file, span)]
     }

