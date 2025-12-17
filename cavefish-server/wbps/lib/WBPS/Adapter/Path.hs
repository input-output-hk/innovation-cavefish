module WBPS.Adapter.Path (
  readFrom,
  writeTo,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (
  FromJSON,
  ToJSON,
  decodeFileStrict,
  encodeFile,
 )
import Path as P (File, Path, parent, toFilePath)
import Path.IO as P (ensureDir)

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
