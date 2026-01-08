pragma circom 2.1.2;

include "../core/hashing/blake2/blake2b.circom";

template blake2b_templ(n_bytes) {
	signal input in[n_bytes];
	signal output out[32];

	component hasher = Blake2b_bytes(n_bytes);
	
	for (var i = 0; i < n_bytes; i++) {
		hasher.inp_bytes[i] <== in[i];
	}
	
	for (var j = 0; j < 32; j++) {
		out[j] <== hasher.hash_bytes[j];
	}
}