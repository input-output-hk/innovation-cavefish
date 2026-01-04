module WBPS.Specs.Session.Commitment.Scalars (specs) where

import Test.QuickCheck (Property, counterexample, (.&&.), (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import WBPS.Core.Keys.ElGamal (AffinePoint (AffinePoint, x, y), EncryptionKey (EncryptionKey), mkRho)
import WBPS.Core.Session.Commitment.Scalars (
  CommitmentScalars (CommitmentScalars, ekPowRho, gPowRho),
 )
import WBPS.Core.Session.Commitment.Scalars.Compute (compute)

specs :: TestTree
specs =
  testGroup
    "WBPS.Specs.Session.Commitment.Scalars"
    [ testProperty "compute matches zk circuit for fixed key and rho" computeMatchesCircuit
    ]

computeMatchesCircuit :: Property
computeMatchesCircuit =
  case mkRho rhoInteger of
    Left err -> counterexample ("Invalid rho: " <> err) False
    Right rho ->
      case compute sampleEncryptionKey rho of
        Left err -> counterexample ("compute failed: " <> show err) False
        Right CommitmentScalars {ekPowRho = ekPowRho', gPowRho = gPowRho'} ->
          counterexample "ek^rho mismatch" (ekPowRho' === expectedEkPowRho)
            .&&. counterexample "g^rho mismatch" (gPowRho' === expectedGPowRho)

sampleEncryptionKey :: EncryptionKey
sampleEncryptionKey =
  EncryptionKey $
    AffinePoint
      13949409190783008520894738635416501547122416709390247001419320903147870232235
      6230067313654301039366684823404445124569608018144478198755770506579514903435

rhoInteger :: Integer
rhoInteger = 1234567890123456789012345678901234567890

expectedEkPowRho :: AffinePoint
expectedEkPowRho =
  AffinePoint
    { x = 9137335444970201643256596907871748556752751480839053976600429028595682443974
    , y = 15717005515068243122442038781576298414355055582736444167988846187936743236921
    }

expectedGPowRho :: AffinePoint
expectedGPowRho =
  AffinePoint
    { x = 2390254713070255989319085409741733535856751730620877964421039371149382899586
    , y = 18931351235086622402032827747115362386859480978226383649260800615739626737477
    }
