{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module Eyeshadow.Conduit
  (split,
   toLeft,
   toRight,
   sideStream,
   byCharacter,
   readFile,
   readTerminal)
  where

import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString as BS
import qualified Data.Char as Char
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Internal hiding (await, yield)
import qualified Data.Conduit.List as C
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import qualified System.IO as IO

import Prelude hiding (readFile)

import Eyeshadow.Types
import qualified Eyeshadow.UTF8 as UTF8


split
  :: (Monad m) => Sink a m ra -> Sink b m rb -> Sink (Either a b) m (ra, rb)
split sinkA sinkB = do
  let loop as bs = do
        item <- await
        case item of
          Nothing -> do
            ra <- lift $ C.sourceList (reverse as) $$ sinkA
            rb <- lift $ C.sourceList (reverse bs) $$ sinkB
            return (ra, rb)
          Just (Left a) -> loop (a : as) bs
          Just (Right b) -> loop as (b : bs)
  loop [] []


{-
One thing to note is that you have to pass around the most recent finalizer explicitly. Each time you provide a HaveOutput finalizer, it resets the finalizer. In our case, when we yield a Right value, we don't want to adjust the finalizer at all, so we have to keep track of what finalizer was returned by the most recent HaveOutput for left. Initially, we provide a dummy `return ()` finalizer.
-- Michael Snoyman, regarding the implementation of toLeft.
-}


toLeft :: Monad m => Conduit a m a' -> Conduit (Either a b) m (Either a' b)
toLeft mainConduit = ConduitM $ go (return ()) $ unConduitM mainConduit
  where
    go final (PipeM mp) = PipeM (liftM (go final) mp)
    go final (Leftover p a) = Leftover (go final p) (Left a)
    go _ (Done ()) = Done ()
    go _ (HaveOutput p final a') = HaveOutput (go final p) final (Left a')
    go final left@(NeedInput p c) =
        NeedInput p' c'
      where
        p' (Left a) = go final (p a)
        p' (Right b) = HaveOutput (go final left) final (Right b)

        c' () = go final $ c ()


toRight :: Monad m => Conduit b m b' -> Conduit (Either a b) m (Either a b')
toRight mainConduit = ConduitM $ go (return ()) $ unConduitM mainConduit
  where
    go final (PipeM mp) = PipeM (liftM (go final) mp)
    go final (Leftover p b) = Leftover (go final p) (Right b)
    go _ (Done ()) = Done ()
    go _ (HaveOutput p final b') = HaveOutput (go final p) final (Right b')
    go final right@(NeedInput p c) =
        NeedInput p' c'
      where
        p' (Left a) = HaveOutput (go final right) final (Left a)
        p' (Right b) = go final (p b)

        c' () = go final $ c ()


sideStream
  :: Monad m
  => Conduit b m (Either a b')
  -> Conduit (Either a b) m (Either a b')
sideStream mainConduit = ConduitM $ go (return ()) $ unConduitM mainConduit
  where go final (PipeM mp) = PipeM (liftM (go final) mp)
        go final (Leftover p b) = Leftover (go final p) (Right b)
        go _ (Done ()) = Done ()
        go _ (HaveOutput p final r) = HaveOutput (go final p) final r
        go final right@(NeedInput p c) = NeedInput p' c'
          where p' (Left a) = HaveOutput (go final right) final (Left a)
                p' (Right b) = go final (p b)
                c' () = go final (c ())


byCharacter :: (Monad m) => Conduit Text m Char
byCharacter = do
  maybeText <- await
  case maybeText of
    Nothing -> return ()
    Just text -> do
      mapM_ yield $ T.unpack text
      byCharacter


readFile
  :: SourceFileSpecification
  -> Source (ResourceT IO) (Either Diagnostic (Char, SourcePosition))
readFile file = do
  case file of
    FileSourceFileSpecification filePath -> do
      sourceFile filePath $= toBytes $= addSourcePositions file
    _ -> return ()


readTerminal
  :: SourceFileSpecification
  -> Source IO (Either Diagnostic (Char, SourcePosition))
readTerminal file = do
  let loop = do
        liftIO $ IO.putStr "> "
        liftIO $ IO.hFlush IO.stdout
        line <- liftIO BS.getLine
        yield line
        yield $ BS.singleton $ fromIntegral $ Char.ord '\n'
        loop
  loop $= toBytes $= addSourcePositions file


toBytes :: (Monad m) => Conduit BS.ByteString m Word8
toBytes = do
  maybeByteString <- await
  case maybeByteString of
    Nothing -> return ()
    Just byteString -> do
      mapM_ yield $ BS.unpack byteString
      toBytes


addSourcePositions
  :: (Monad m)
  => SourceFileSpecification
  -> Conduit Word8 m (Either Diagnostic (Char, SourcePosition))
addSourcePositions file = do
  let loop position byteString = do
        case UTF8.decode byteString of
          Left UTF8.InvalidDataDecodingFailure -> do
            diagnoseInvalidUTF8 file
              $ spanForBytes position byteString
          Left UTF8.InsufficientDataDecodingFailure -> do
            maybeByte <- await
            case maybeByte of
              Nothing -> do
                if BS.null byteString
                  then return ()
                  else diagnoseUnexpectedEndOfFileInUTF8 file
                         $ spanForBytes position byteString
              Just byte -> loop position $ BS.snoc byteString byte
          Right (c, _) -> do
            let byteCount = BS.length byteString
                oldByteOffset = sourcePositionByteOffset position
                oldCharacterOffset =
                  sourcePositionCharacterOffset position
                oldLine = sourcePositionLine position
                oldColumn = sourcePositionColumn position
                newByteOffset = oldByteOffset + byteCount
                newCharacterOffset = oldCharacterOffset + 1
                isNewline = c == '\n'
                newLine = if isNewline
                            then oldLine + 1
                            else oldLine
                newColumn = if isNewline
                              then 1
                              else oldColumn + 1
                newPosition =
                  SourcePosition {
                      sourcePositionByteOffset = newByteOffset,
                      sourcePositionCharacterOffset =
                        newCharacterOffset,
                      sourcePositionLine = newLine,
                      sourcePositionColumn = newColumn
                    }
            yield $ Right (c, position)
            loop newPosition BS.empty
  loop (SourcePosition { 
            sourcePositionByteOffset = 0,
            sourcePositionCharacterOffset = 0,
            sourcePositionLine = 1,
            sourcePositionColumn = 1
          })
        BS.empty


spanForBytes :: SourcePosition -> BS.ByteString -> SourceSpan
spanForBytes position byteString =
  let endPosition =
        position {
            sourcePositionByteOffset =
              sourcePositionByteOffset position + BS.length byteString
          }
  in SourceSpan {
         sourceSpanStart = position,
         sourceSpanEnd = endPosition
       }


diagnoseInvalidUTF8
  :: (Monad m)
  => SourceFileSpecification
  -> SourceSpan
  -> Producer m (Either Diagnostic b)
diagnoseInvalidUTF8 file span = yield $ Left $
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
  :: (Monad m)
  => SourceFileSpecification
  -> SourceSpan
  -> Producer m (Either Diagnostic b)
diagnoseUnexpectedEndOfFileInUTF8 file span = yield $ Left $
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

