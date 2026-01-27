pragma circom 2.1.2;

include "../binary_utils.circom";

//------------------------------------------------------------------------------
// XOR 3 bits together

template XOR3_v1() {
  signal input  x;
  signal input  y;
  signal input  z;
  signal output out;

  component bs = Bits2();
  bs.xy <== x + y + z;
  bs.lo ==> out;
}

//------------------
// same number of constraints (that is, 2), in the general case
// however circom can optimize y=0 or z=0, unlike with the above
// and hopefully also x=0.

template XOR3_v2() {
  signal input  x;
  signal input  y;
  signal input  z;
  signal output out;

  signal tmp <== y*z;
  out <== x * (1 - 2*y - 2*z + 4*tmp) + y + z - 2*tmp;
}

//------------------------------------------------------------------------------
// converts a sequence of `n` big-endian 32-bit words to `4n` bytes
// (to be compatible with the output hex string of standard SHA2 tools)

template DWordsToByteString(n) { 
  
  signal input  inp[n][32];
  signal output out[4*n];

  for(var k=0; k<n; k++) {
    for(var j=0; j<4; j++) {

      var sum = 0;
      for(var i=0; i<8; i++) {
        sum += inp[k][j*8+i] * (1<<i);
      }

      out[k*4 + (3-j)] <== sum;
    }
  }
}

//------------------------------------------------------------------------------
// converts a sequence of `n` big-endian 64-bit words to `8n` bytes
// (to be compatible with the output hex string of standard SHA2 tools)

template QWordsToByteString(n) { 
  
  signal input  inp[n][64];
  signal output out[8*n];

  for(var k=0; k<n; k++) {
    for(var j=0; j<8; j++) {

      var sum = 0;
      for(var i=0; i<8; i++) {
        sum += inp[k][j*8+i] * (1<<i);
      }

      out[k*8 + (7-j)] <== sum;
    }
  }
}

//------------------------------------------------------------------------------
