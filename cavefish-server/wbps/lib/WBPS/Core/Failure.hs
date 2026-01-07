module WBPS.Core.Failure (
  RegistrationFailed (..),
  -- | Different failures that can occur during the registration process
  toWBPSFailure,
  -- | Convert an Either String a to a MonadError with RegistrationFailed
) where

import Control.Monad.Except (MonadError (throwError))
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.Account (AccountId)
import WBPS.Core.Session.Commitment (CommitmentId)
import WBPS.Core.Session.Session (SessionId)

toWBPSFailure :: MonadError [RegistrationFailed] m => Either String a -> m a
toWBPSFailure = either (throwError . pure . BuildCommitmentFailed) pure

data RegistrationFailed
  = AccountIdInvalidToCreateAFolder AccountId
  | VerificationNotFound UserWalletPublicKey
  | EncryptionKeysNotFound UserWalletPublicKey
  | AccountAlreadyRegistered UserWalletPublicKey
  | AccountNotFound UserWalletPublicKey
  | BuildCommitmentFailed String
  | SessionIdInvalidToCreateAFolder SessionId
  | SessionNotFound UserWalletPublicKey CommitmentId
  | SessionMessageNotFound UserWalletPublicKey CommitmentId
  | SessionRhoNotFound UserWalletPublicKey CommitmentId
  | SessionScalarsNotFound UserWalletPublicKey CommitmentId
  | SessionCommitmentNotFound UserWalletPublicKey CommitmentId
  deriving (Show, Eq)
