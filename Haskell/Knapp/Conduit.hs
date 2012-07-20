module Knapp.Conduit (split) where

import Control.Monad.Trans
import Data.Conduit
import qualified Data.Conduit.List as C


instance (MonadThrow m) => MonadThrow (Pipe l i o u m) where
  monadThrow error = lift $ monadThrow error


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

