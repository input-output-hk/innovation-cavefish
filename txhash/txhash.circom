pragma circom  2.0.0;

include "blake2b.circom";

template Num2Bits(n) {
    signal input in;
    signal output out[n];
    var lc1=0; 
    // this serves as an accumulator to "recompute" in bit-by-bit
    var e2=1;
    for (var i = 0; i<n; i++) {
        out[i] <-- (in >> i) & 1;
        out[i] * (out[i] -1 ) === 0; // force out[i] to be 1 or 0
        lc1 += out[i] * e2; //add to the accumulator if the bit is 1 
        e2 = e2+e2; // takes on values 1,2,4,8,...
    }

    lc1 === in;
}

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

    // total number of bytes in encoded transaction
    var ll = numInputs * inputBitLen + numOutputs * outputBitLen + validityIntervalBitLen * 2 + numMintAssets * (assetIdBitLen + quantityBitLen) + quantityBitLen + auxDataBitLen + (keyLen + signatureLen) * numSignatures ;
    var j = 0;
    var k = numInputs * inputBitLen;

    signal output hashedTx[32] ;
    signal output preimTxBytes[ll\8] ;
    var preimTxBits[ll] ;

    // convert tx field signals into bits and concatenate them
    component cinputs[numInputs] ; 
    for(var i=0; i<numInputs; i++) {
      cinputs[i] = Num2Bits(inputBitLen);
      cinputs[i].in <== inputs[i] ; 
      for(var n=j; n<j+k; n++) {
        preimTxBits[n] = cinputs[i].out[n-j] ;
      }  
    }

    j = j + k ;
    k = numOutputs * outputBitLen ;

    component coutputs[numOutputs] ;
    for(var i=0; i<numOutputs; i++) {
      coutputs[i] = Num2Bits(outputBitLen);
      coutputs[i].in <== outputs[i] ; 
      for(var n=j; n<j+k; n++) {
        preimTxBits[n] = coutputs[i].out[n-j] ;
      }  
    }

    j = j + k ;
    k = validityIntervalBitLen * 2 ;

    component cvalidityInterval[2] ;
    for(var i=0; i<2; i++) {
      cvalidityInterval[i] = Num2Bits(validityIntervalBitLen);
      cvalidityInterval[i].in <== validityInterval[i] ; 
      for(var n=j; n<j+k; n++) {
        preimTxBits[n] = cvalidityInterval[i].out[n-j] ;
      }  
    }

    j = j + k ;
    k = numMintAssets * (assetIdBitLen + quantityBitLen);

    component cmint[numMintAssets] ;
    for(var i=0; i<numMintAssets; i++) {
      cmint[i] = Num2Bits(assetIdBitLen + quantityBitLen);
      cmint[i].in <== mint[i] ; 
      for(var n=j; n<j+k; n++) {
        preimTxBits[n] = cmint[i].out[n-j] ;
      }  
    }

    j = j + k ;
    k = quantityBitLen  ;

    component cfee ;
    cfee = Num2Bits(quantityBitLen);
    cfee.in <== fee ; 
    for(var n=j; n<j+k; n++) {
      preimTxBits[n] = cfee.out[n-j] ;  
    }

    j = j + k ;
    k = auxDataBitLen ;
    
    component cauxData ;
    cauxData = Num2Bits(quantityBitLen);
    cauxData.in <== auxData ; 
    for(var n=j; n<j+k; n++) {
      preimTxBits[n] = cauxData.out[n-j] ;  
    }

    j = j + k ;
    k = (keyLen + signatureLen) * numSignatures ;
    
    component csigs[numSignatures] ;
    for(var i=0; i<numSignatures; i++) {
      csigs[i] = Num2Bits(keyLen + signatureLen);
      csigs[i].in <== sigs[i] ; 
      for(var n=j; n<j+k; n++) {
        preimTxBits[n] = csigs[i].out[n-j] ;
      }  
    }

    // make blake2b circuit
    component ht ;
    ht = Blake2b_bytes(ll) ; 

    // convert to bytes by adding 8 bits at a time
    for(var j=0; j<ll\8; j++) {
        var acc = 0;
        for(var i=0; i<8; i++) { acc += preimTxBits[j*8+i] * (2**i); }
        preimTxBytes[j] <== acc;
    }

    for(var i=0; i < ll; i++) {
      ht.inp_bytes[i] <== preimTxBytes[i] ;
    }

    for(var i=0; i<32; i++) {
      hashedTx[i] <== ht.hash_bytes[i] ;
    }
      
}


component main {public [inputs, outputs, validityInterval, mint, fee, auxData, sigs]} = hashTx (5, 5, 2, 5, 100000, 100000, 30000, 100000, 100000, 100000, 100000, 100000);

