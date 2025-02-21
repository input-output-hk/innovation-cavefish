pragma circom  2.0.0;

include "blake2b.circom";


template hashTx 
    ( 
    // how may of each component of a Tx
    numInputs ,
    numOutputs ,
    numMintAssets , 
    numSignatures ,

    // lengths of fields in bytes
    inputBitLen ,
    outputBitLen ,
    validityIntervalBitLen ,
    assetIdBitLen ,
    quantityBitLen ,
    auxDataBitLen ,
    keyLen ,
    signatureLen 
    ) {

    signal input inputs[numInputs] ;
    signal input outputs[numOutputs] ;
    signal input validityInterval[2] ;
    signal input mint[numMintAssets] ;
    signal input fee ;
    signal input auxData ;
    signal input sigs[numSignatures] ;

    var ll = numInputs * inputBitLen + numOutputs * outputBitLen + validityIntervalBitLen * 2 + numMintAssets * (assetIdBitLen + quantityBitLen) + quantityBitLen + auxDataBitLen + (keyLen + signatureLen) * numSignatures ;

    signal output hashedTx[32] ;
    signal output preimTx[ll] ;

    // TODO needs to convert input signals into bytes
    for(var i=0; i<ll; i++) {
      preimTx[i] <== 0 ;
    }

    component ht = Blake2b_bytes(ll) ; 
    //ht.inp_bytes <== preimTx ;
     
    for(var i=0; i<32; i++) {
      hashedTx[i] <== ht.hash_bytes[i] ;
    }
      
}


component main {public [inputs, outputs, validityInterval, mint, fee, auxData, sigs]} = hashTx (5, 5, 2, 5, 100000, 100000, 30000, 100000, 100000, 100000, 100000, 100000);

