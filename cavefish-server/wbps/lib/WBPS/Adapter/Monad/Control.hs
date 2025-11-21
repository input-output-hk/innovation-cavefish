module WBPS.Adapter.Monad.Control (ifM, whenM, whenNothingThrow, whenLeftThrow) where

import Control.Monad.Error.Class

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM condM ma mb = do
  cond <- condM
  if cond then ma else mb

whenM :: Monad m => m Bool -> m () -> m ()
whenM condM ma = ifM condM ma (pure ())

whenNothingThrow :: MonadError e m => e -> Maybe a -> m a
whenNothingThrow err = maybe (throwError err) pure

whenLeftThrow :: MonadError e m => (a -> e) -> Either a b -> m b
whenLeftThrow toErr = either (throwError . toErr) pure
