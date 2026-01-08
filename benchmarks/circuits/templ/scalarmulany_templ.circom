pragma circom 2.1.2;

include "bitify.circom";
include "escalarmulany.circom";

template scalarmulany_templ(n_bits) {
    signal input point[2];
    signal input scalar;
    signal output commitment[2];

    component n2b = Num2Bits(n_bits);
    scalar ==> n2b.in;

    component mul_any = EscalarMulAny(n_bits);
    mul_any.p[0] <== point[0];
    mul_any.p[1] <== point[1];
    var i;
    for (i = 0; i < n_bits; i++) {
        n2b.out[i] ==> mul_any.e[i];
    }
    commitment[0] <== mul_any.out[0];
    commitment[1] <== mul_any.out[1];
}