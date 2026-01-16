module WBPS.WBPS (
  runWBPS,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, runReaderT)
import WBPS.Core.Failure (WBPSFailure)
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme)

runWBPS ::
  FileScheme ->
  (forall m. (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) => m a) ->
  IO (Either [WBPSFailure] a)
runWBPS scheme action =
  runExceptT $ runReaderT action scheme
