pragma circom 2.1.2;

include "../core/hashing/sha2/sha256/sha256_hash_bits.circom";

template sha256_bits_templ(n_bits) {
    signal input in[n_bits];
    signal output out[32];

    component hasher = Sha256_hash_bits_digest(n_bits);

    for (var i = 0; i < n_bits; i++) {
		hasher.inp_bits[i] <== in[i];
	}
	
	for (var j = 0; j < 32; j++) {
		out[j] <== hasher.hash_bytes[j];
	}
}