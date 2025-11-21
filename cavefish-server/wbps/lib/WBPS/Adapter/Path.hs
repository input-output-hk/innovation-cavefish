module WBPS.Adapter.Path (
  readFrom,
  writeTo,
) where

import Control.Monad.IO.Class
import Data.Aeson
import Path as P
import Path.IO as P

writeTo ::
  (MonadIO m, ToJSON a) =>
  Path b File -> a -> m ()
writeTo path a = do
  P.ensureDir (P.parent path)
  liftIO $ encodeFile (toFilePath path) a

readFrom ::
  (MonadIO m, FromJSON a) =>
  Path b File -> m (Maybe a)
readFrom path = liftIO $ decodeFileStrict (toFilePath path)
