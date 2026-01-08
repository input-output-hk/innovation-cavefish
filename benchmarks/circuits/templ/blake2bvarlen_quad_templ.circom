pragma circom 2.1.2;

include "../core/hashing/blake2/blake2b.circom";

template blake2bvarlen_quad_templ(n_block) {

    var input_len[n_block];     // len of message to be hashed based on i in (1, n_block)
	signal input in[n_block * 128];
    signal input sel[n_block];

	signal output out[32];              // Selected output
    signal inter_out[n_block][32];      // All possible outputs from where desired is selected

    component hasher[n_block];

    var acc = 0;
    for (var n = 0; n < n_block; n++) {
        acc += sel[n];
    }
    acc === 1;

    for (var n = 0; n < n_block; n++) {
        
        var input_len = (n + 1) * 128;
        hasher[n] = Blake2b_bytes(input_len);

        for (var i = 0; i < input_len; i++) {
            hasher[n].inp_bytes[i] <== in[i];
        }

        for (var j = 0; j < 32; j++) {
		    inter_out[n][j] <== hasher[n].hash_bytes[j] * sel[n];  // Assign 0 if not selected
	    }

    }

    for (var k = 0; k < 32; k++) {
        var sum = 0;
        for (var b = 0; b < n_block; b++) {
            sum += inter_out[b][k];
        }
        out[k] <== sum;
    }
}