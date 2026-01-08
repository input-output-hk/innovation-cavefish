pragma circom 2.1.2;

include "../core/hashing/poseidon2/poseidon2_sponge.circom";

template poseidon2_rate1_templ(input_len, output_len) {
  signal input  inp[input_len];
  signal output out[output_len];
  component sponge = PoseidonSponge(3, 2, input_len, output_len);
  sponge.inp <== inp;
  sponge.out ==> out;
}