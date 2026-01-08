pragma circom 2.1.2;

include "poseidon.circom";

template poseidon_prf_chained_templ(len) {

    signal input in_seed_x;
    signal input in_seed_y;

    signal output prf[len];

    component pEx = PoseidonEx(2, 1);
    pEx.initialState <== 0;
    pEx.inputs[0] <== in_seed_x;
    pEx.inputs[1] <== in_seed_y;
    prf[0] <== pEx.out[0];

    component nexts[len];
    var k;
    for (k = 1; k < len; k += 2) {
        nexts[k] = PoseidonEx(1, 2);
        nexts[k].initialState <== 0;
        nexts[k].inputs[0] <== prf[k - 1];

        prf[k] <== nexts[k].out[0];
        if (k + 1 < len) {
            prf[k + 1] <== nexts[k].out[1];
        }
    }

}