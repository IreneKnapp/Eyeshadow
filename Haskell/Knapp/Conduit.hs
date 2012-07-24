module Knapp.Conduit (split, toLeft, toRight) where

import Control.Monad
import Control.Monad.Trans
import Data.Conduit
import Data.Conduit.Internal
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


{-
One thing to note is that you have to pass around the most recent finalizer explicitly. Each time you provide a HaveOutput finalizer, it resets the finalizer. In our case, when we yield a Right value, we don't want to adjust the finalizer at all, so we have to keep track of what finalizer was returned by the most recent HaveOutput for left. Initially, we provide a dummy `return ()` finalizer.
-- Michael Snoyman, regarding the implementation of toLeft.
-}


toLeft :: Monad m => Conduit a m a' -> Conduit (Either a b) m (Either a' b)
toLeft =
    go (return ())
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
toRight =
    go (return ())
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
