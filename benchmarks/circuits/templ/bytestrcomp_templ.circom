pragma circom 2.1.2;

//include "comparators.circom";

template bytestrcomp_templ(len) {

    signal input a[len];
    signal input b[len];

    //component equal[len];

    signal int <== a[0] * a[0];
    int === b[0] * b[0];

    for (var i = 0; i < len; i++) {
        a[i] === b[i];
        //equal[i] = IsEqual();
        //equal[i].in[0] <== a[i];
        //equal[i].in[1] <== b[i];
        //equal[i].out ==> out[i]; // Not needed here, already constrained
    }

}