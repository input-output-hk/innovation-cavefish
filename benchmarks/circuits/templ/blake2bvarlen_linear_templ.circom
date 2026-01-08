pragma circom 2.1.2;

include "../core/hashing/blake2var/blake2b.circom";
include "../core/hashing/blake2var/blake2_common.circom";
include "mux1.circom";

template blake2bvarlen_linear_templ(n_block) {

    signal input m_len;     // The real lenght of the unpadded message
	signal input in[n_block * 128];
    signal input sel[n_block];

	//signal output out[32];    // This should be equivalent to hash_bytes output
    signal inter_out[n_block][32];

    signal output hash_words[8];
    signal output hash_bytes[32];
    signal output hash_bits[256];

    var kk = 0;                   // key size in bytes
    var nn = 32;                  // final hash size in bytes
    
    //var dd = (ll + 127) \ 128;    // number of message blocks

    signal blocks[n_block][16];        // message blocks

    var p0 = 0x01010000 ^ (kk << 8) ^ nn;

    signal hs[n_block + 1][8];

    component iv = IV();
    hs[0][0] <== (0x6A09E667F3BCC908 ^ p0);
    for(var i=1; i<8; i++) { hs[0][i] <== iv.out[i]; }

    component compr[n_block];

    // -------------------------------------
    // Enforce sel is binary and exactly one 1
    var sum_sel = 0;
    for (var k = 0; k < n_block; k++) {
        sel[k] * (sel[k]-1) === 0;  // binary constraint
        sum_sel += sel[k];
    }
    sum_sel === 1;
    // -------------------------------------

    // -------------------------------------
    // Compute the offset counter to be added per block
    signal last_offset_quotient <-- m_len \ 128;
    signal last_offset <-- m_len % 128;
    m_len === last_offset_quotient * 128 + last_offset;

    signal oc[n_block];     // This is per round, not accumulated
    component mux[n_block];
    for (var k = 0; k < n_block; k++) {
        mux[k] = Mux1();
        mux[k].c[0] <== 128;
        mux[k].c[1] <== last_offset;
        mux[k].s <== sel[k];
        oc[k] <== mux[k].out;
    }

    signal oc_acc[n_block];
    oc_acc[0] <== oc[0];
    for (var k = 1; k < n_block; k++) {
        oc_acc[k] <== oc_acc[k-1] + oc[k];
    }
    // -------------------------------------

    for (var k = 0; k < n_block; k++) {

        // Compression gadget
        compr[k] = CompressionF();
        // Note: another option to keep f as a template param is to always execute
        // CompressionF(0) and CompressionF(1) and to select the correct one by
        // additive conditional selector. However, this is not that easy for the counter
        // t because for each block one can have 128 possible lengths (if it is the last block)

        // Adjust block words
        for(var j=0; j<16; j++) {
        var acc = 0;
            for(var q=0; q<8; q++) {
                var u = k*128 + j*8 + q;
                acc += in[u] * (256**q);
            }
            blocks[k][j] <== acc;
        }

        // Apply compression
        compr[k].t   <== oc_acc[k];
        compr[k].f   <== sel[k];
        compr[k].h   <== hs[k];
        compr[k].m   <== blocks[k];
        compr[k].out ==> hs[k+1];

    }

    signal result_words[8];
    signal filter_products[n_block][8];
    for (var k = 0; k < n_block; k++) {
        for (var i = 0; i < 8; i++) {
            filter_products[k][i] <== hs[k+1][i] * sel[k];
        }
    }
    var acc[8];
    for (var i = 0; i < 8; i++) { acc[i] = 0; }
    for (var k = 0; k < n_block; k++) {
        for (var i = 0; i < 8; i++) {
            acc[i] += filter_products[k][i];
        }
    }
    result_words <== acc;   // This is the filtered result

    // Convert the result from words to bytes
    var nw = nn \ 8;      // how many qwords in the output of the hash
    for(var j=0; j<nw; j++) {  result_words[j] ==> hash_words[j]; }

    component tbs[nw];
    for(var j=0; j<nw; j++) {
        tbs[j] = ToBits(64);
        tbs[j].inp <== hash_words[j];
        for(var i=0; i<64; i++) {
            tbs[j].out[i] ==> hash_bits[j*64+i];
        }    
    }

    for(var j=0; j<nn; j++) {
        var acc = 0;
        for(var i=0; i<8; i++) { acc += hash_bits[j*8+i] * (2**i); }
        hash_bytes[j] <== acc;
    }

}