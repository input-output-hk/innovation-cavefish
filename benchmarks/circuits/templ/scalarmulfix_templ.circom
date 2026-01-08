pragma circom 2.1.2;

include "bitify.circom";
include "escalarmulfix.circom";

template scalarmulfix_templ() {
    
    signal input scalar;
    signal output commitment[2];

    // This is BabyJubJub's prime order subgroup generator 
    var BASE[2] = [
        5299619240641551281634865583518297030282874472190772894086521144482721001553,
        16950150798460657717958625567821834550301663161624707787222815936182638968203
    ];

    component n2b = Num2Bits(251);
    scalar ==> n2b.in;

    component mul_fix = EscalarMulFix(251, BASE);
    for (var i = 0; i < 251; i++) {
        n2b.out[i] ==> mul_fix.e[i];
    }
    commitment[0] <== mul_fix.out[0];
    commitment[1] <== mul_fix.out[1];

}