pragma circom 2.1.2;

include "poseidon.circom";

template poseidon_prf_oneshot_templ(len) {

    signal input in_seed_x;
    signal input in_seed_y;

    signal output prf[len];

    component pEx = PoseidonEx(2, len);
    pEx.initialState <== 0;
    pEx.inputs[0] <== in_seed_x;
    pEx.inputs[1] <== in_seed_y;
    prf <== pEx.out;

}