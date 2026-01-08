pragma circom 2.1.2;

include "../core/hashing/blake2/blake2b.circom";

template blake2bcompress_templ() {

    signal input h[8];
    signal input m[16];
    signal output out[8];

    component compr = CompressionF(128, 1);
    compr.h   <== h;
    compr.m   <== m;
    compr.out ==> out;

}