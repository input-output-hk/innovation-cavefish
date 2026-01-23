module WBPS.Core.Failure (
  WBPSFailure (..),
  -- | Different failures that can occur during the registration process
  toWBPSFailure,
  -- | Convert an Either String a to a MonadError with WBPSFailure
) where

import Control.Monad.Except (MonadError (throwError))
import WBPS.Core.Registration.RegistrationId (RegistrationId)
import WBPS.Core.Session.SessionId (SessionId)

toWBPSFailure :: MonadError [WBPSFailure] m => Either String a -> m a
toWBPSFailure = either (throwError . pure . BuildCommitmentFailed) pure

data WBPSFailure
  = -- Account/registration failures
    RegistrationIdInvalid RegistrationId
  | VerificationNotFound RegistrationId
  | EncryptionKeysNotFound RegistrationId
  | AccountAlreadyRegistered RegistrationId
  | AccountNotFound RegistrationId
  | -- Commitment/circuit failures
    BuildCommitmentFailed String
  | CircuitMessageDecodeFailed String
  | -- Transaction assembly failures
    TxBuiltTooLarge String
  | TxInputsCountMismatch String
  | -- Session directory/session lookup failures
    SessionIdInvalidToCreateAFolder SessionId
  | SessionNotFound SessionId
  | -- Session artefact persistence failures
    SessionPreparedMessageNotFound SessionId
  | SessionRhoNotFound SessionId
  | SessionScalarsNotFound SessionId
  | SessionCommitmentNotFound SessionId
  | SessionProofNotFound SessionId
  | SessionBlindSignatureNotFound SessionId
  | SessionTxSignatureNotFound SessionId
  | SessionSubmittedTxNotFound SessionId
  | -- Proof/signature failures
    ProofVerificationFailed String
  | SessionSubmittingFailed String
  deriving (Show, Eq)
