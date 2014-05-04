{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}
module Eyeshadow.Phase.File
  (readFile,
   readTerminal)
  where

import qualified Control.Eff as Eff
import qualified Control.Eff.Lift as Eff
import qualified Control.Eff.Resource as Eff
import qualified Control.Monad.Trans as Conduit
import qualified Data.ByteString as ByteString
import qualified Data.Conduit as Conduit
import qualified Data.Text as Text
import qualified System.IO as IO

import Control.Monad
import Data.Char
import Data.Either
import Data.Maybe
import Data.Word

import Eyeshadow.Data.Span
import Eyeshadow.Data.UTF8
import Eyeshadow.Diagnostic
import Eyeshadow.Prelude


readFile
  :: (Eff.SetMember Eff.Lift (Eff.Lift IO.IO) r,
      Eff.Member (Eff.Resource (IO.IO ())) r,
      Eff.Member Diagnose r)
  => FileSpecification
  -> Conduit.Source (Eff.Eff r) (Char, Span)
readFile file = do
  case file of
    FileFileSpecification filePath -> do
      let sourceFile :: (Eff.SetMember Eff.Lift (Eff.Lift IO.IO) r,
                         Eff.Member (Eff.Resource (IO.IO ())) r)
                     => IO.FilePath
                     -> Conduit.Source (Eff.Eff r) ByteString.ByteString
          sourceFile filePath = do
            handle <- Conduit.lift $ Eff.lift $
              IO.openFile filePath IO.ReadMode
            _ <- Conduit.lift $ Eff.register $ IO.hClose handle
            let loop = do
                   byteString <- Conduit.lift $ Eff.lift $
                     ByteString.hGetSome handle 4096
                   if ByteString.null byteString
                     then return ()
                     else do
                       Conduit.yield byteString
                       loop
            loop
      sourceFile filePath Conduit.$= toBytes Conduit.$= addSpans file
    _ -> return ()


readTerminal
  :: (Eff.SetMember Eff.Lift (Eff.Lift IO.IO) r,
      Eff.Member Diagnose r)
  => FileSpecification
  -> Conduit.Source (Eff.Eff r) (Char, Span)
readTerminal file = do
  let loop :: (Eff.SetMember Eff.Lift (Eff.Lift IO.IO) r)
           => Conduit.Source (Eff.Eff r) ByteString.ByteString
      loop = do
        Conduit.lift $ Eff.lift $ IO.putStr "> "
        Conduit.lift $ Eff.lift $ IO.hFlush IO.stdout
        line <- Conduit.lift $ Eff.lift ByteString.getLine
        Conduit.yield line
        Conduit.yield $ ByteString.singleton $ fromIntegral $ ord '\n'
        loop
  loop Conduit.$= toBytes Conduit.$= addSpans file


toBytes :: (Monad m) => Conduit.Conduit ByteString.ByteString m Word8
toBytes = do
  maybeByteString <- Conduit.await
  case maybeByteString of
    Nothing -> return ()
    Just byteString -> do
      mapM_ Conduit.yield $ ByteString.unpack byteString
      toBytes


addSpans
  :: (Eff.Member Diagnose r)
  => FileSpecification
  -> Conduit.Conduit Word8 (Eff.Eff r) (Char, Span)
addSpans file = do
  let loop
        :: (Eff.Member Diagnose r)
        => Scan -> ByteString.ByteString
        -> Conduit.Conduit Word8 (Eff.Eff r) (Char, Span)
      loop scan byteString = do
        case decode byteString of
          Left InvalidDataDecodingFailure -> do
            let (newScan, span) =
                  advanceScanByEncodingError scan (ByteString.length byteString)
            Conduit.lift $ diagnoseInvalidUTF8 file span
            loop newScan ByteString.empty
          Left InsufficientDataDecodingFailure -> do
            maybeByte <- Conduit.await
            case maybeByte of
              Nothing -> do
                if ByteString.null byteString
                  then return ()
                  else let (_, span) =
                             advanceScanByEncodingError scan
                               (ByteString.length byteString)
                       in Conduit.lift $
                            diagnoseUnexpectedEndOfFileInUTF8 file span
              Just byte -> loop scan $ ByteString.snoc byteString byte
          Right (c, _) -> do
            let (newScan, span) =
                  advanceScan scan c (ByteString.length byteString)
            Conduit.yield (c, span)
            loop newScan ByteString.empty
  loop startScan ByteString.empty


diagnoseInvalidUTF8
  :: (Eff.Member Diagnose r)
  => FileSpecification
  -> Span
  -> Eff.Eff r ()
diagnoseInvalidUTF8 file span = diagnose $
  Diagnostic {
      diagnosticHeadline = "Invalid UTF-8",
      diagnosticDescription =
        Text.concat
          ["This implies that the source file is not valid UTF-8, ",
           "possibly because it has been truncated as by an ",
           "incomplete download.  Verify the integrity of the file."],
      diagnosticDetails =
        [("Invalid character", file, span)]
    }


diagnoseUnexpectedEndOfFileInUTF8
  :: (Eff.Member Diagnose r)
  => FileSpecification
  -> Span
  -> Eff.Eff r ()
diagnoseUnexpectedEndOfFileInUTF8 file span = diagnose $
  Diagnostic {
       diagnosticHeadline = "Invalid UTF-8 (truncated by end-of-file)",
       diagnosticDescription =
         Text.concat
           ["This implies that the source file is not valid UTF-8, ",
            "possibly because it has been truncated as by an ",
            "incomplete download.  Verify the integrity of the file."],
       diagnosticDetails =
         [("Invalid character", file, span)]
     }

