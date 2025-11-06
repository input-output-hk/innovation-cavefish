module WBPS.Adapter.Data (
  whenM,
  maybeIf,
  guard,
) where

import Control.Monad (when)
import Data.Bool
import Data.Validation (Validation (..))

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb act = mb >>= \b -> when b act

maybeIf :: a -> Bool -> Maybe a
maybeIf a = bool Nothing (Just a)

guard :: e -> Bool -> Validation [e] ()
guard _ True = Success ()
guard e False = Failure [e]
