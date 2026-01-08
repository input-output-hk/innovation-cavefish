pragma circom 2.1.2;

include "../core/hashing/sha2/sha512/sha512_hash_bytes.circom";

template sha512_bytes_templ(n_bytes) {
    signal input in[n_bytes];
    signal output out[64];

    component hasher = Sha512_hash_bytes_digest(n_bytes);

    for (var i = 0; i < n_bytes; i++) {
		hasher.inp_bytes[i] <== in[i];
	}
	
	for (var j = 0; j < 32; j++) {
		out[j] <== hasher.hash_bytes[j];
	}
}