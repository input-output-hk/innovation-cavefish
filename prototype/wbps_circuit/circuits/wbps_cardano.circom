// ======================================================================
// CardanoWBPS.circom — numeric-tag logs (Circom-safe)
// ======================================================================

pragma circom 2.1.2;

// Poseidon hash / sponge (for PRF stream)
include "../circomlib/circuits/poseidon.circom";

// Scalar multiplication gadgets
include "../circomlib/circuits/escalarmulany.circom";

// Bits <-> number helpers
include "../circomlib/circuits/bitify.circom";

// SHA-512 bitwise (Cardano EdDSA)
include "../hashing_circuits/sha2/sha512/sha512_hash_bits.circom";

template CardanoWBPS(n, sb_n, sb_o) {
    // ------------------------------------------------------------------
    // Params & sanity
    // ------------------------------------------------------------------
    var bits = 254;
    assert(n % bits == 0);

    var cmsg_elem = n \ bits;

    // ------------------------------------------------------------------
    // PUBLIC INPUTS
    // ------------------------------------------------------------------
    signal input A[256];            // Schnorr/EdDSA public key X (bits)
    signal input R8[256];           // Schnorr nonce point R (bits)
    signal input ek[2];             // encryption key point (x,y)
    signal input C0[2];             // C0 = g^rho
    signal input Cmsg[cmsg_elem];   // ciphertext limbs
    signal input c_schnorr[64];     // SHA-512 digest bytes
    signal input msg_pub[n];        // public message bits

    // ------------------------------------------------------------------
    // PRIVATE INPUTS
    // ------------------------------------------------------------------
    signal input rho;               // encryption randomness
    signal input msg_priv[sb_n];    // private message slice bits

    // ------------------------------------------------------------------
    // Internal signals / vars
    // ------------------------------------------------------------------
    var i;
    var j;

    // BabyJub base (affine)
    var base[2] = [
        5299619240641551281634865583518297030282874472190772894086521144482721001553,
        16950150798460657717958625567821834550301663161624707787222815936182638968203
    ];

    // ------------------------------------------------------------------
    // Rebuild message m (bitwise)
    // ------------------------------------------------------------------
    signal msg[n];

    for (i = 0; i < n; i++) {
        if (i >= sb_o && i < sb_o + sb_n) {
            msg[i] <== msg_priv[i - sb_o];
        } else {
            msg[i] <== msg_pub[i];
        }
    }

    // ------------------------------------------------------------------
    // ENC binding: C = Enc(ek, m; rho)
    // ------------------------------------------------------------------

    // rho → bits
    component n2b_rho = Num2Bits(251);
    rho ==> n2b_rho.in;

    // ek^rho (arbitrary-base mul)
    component ek_rho = EscalarMulAny(251);
    ek_rho.p[0] <== ek[0];
    ek_rho.p[1] <== ek[1];
    for (i = 0; i < 251; i++) {
        n2b_rho.out[i] ==> ek_rho.e[i];
    }

    // g^rho (fixed-base mul) and check C0
    component g_pow_rho = EscalarMulFix(251, base);
    for (i = 0; i < 251; i++) {
        n2b_rho.out[i] ==> g_pow_rho.e[i];
    }

    // --- Numeric logs for g^rho:
    // 900010 => g^rho.x
    // 900011 => g^rho.y
    log(900010);
    log(g_pow_rho.out[0]);
    log(900011);
    log(g_pow_rho.out[1]);

    C0[0] === g_pow_rho.out[0];
    C0[1] === g_pow_rho.out[1];

    // PoseidonEx(2,1) seed from ek^rho
    component pEx = PoseidonEx(2, 1);
    pEx.initialState <== 0;
    pEx.inputs[0] <== ek_rho.out[0];
    pEx.inputs[1] <== ek_rho.out[1];

    // rand stream as signals (so we can log/constraint)
    signal rand[cmsg_elem];
    rand[0] <== pEx.out[0];

    // Chain with PoseidonEx(1,2)
    component randEnc[cmsg_elem];
    for (var k = 1; k < cmsg_elem; k += 2) {
        randEnc[k] = PoseidonEx(1, 2);
        randEnc[k].initialState <== 0;
        randEnc[k].inputs[0] <== rand[k - 1];

        rand[k] <== randEnc[k].out[0];
        if (k + 1 < cmsg_elem) {
            rand[k + 1] <== randEnc[k].out[1];
        }
    }

    // Convert message bits to limbs and enforce Cmsg[i] = msg_i + rand[i]
    component msgBits2Num[cmsg_elem];
    signal msg_limb[cmsg_elem];
    signal sum_i[cmsg_elem];
    signal delta[cmsg_elem];

    for (var t = 0; t < cmsg_elem; t++) {
        msgBits2Num[t] = Bits2Num(bits);
        for (var b = 0; b < bits; b++) {
            msgBits2Num[t].in[b] <== msg[t*bits + b];
        }
        msg_limb[t] <== msgBits2Num[t].out;

        sum_i[t] <== msg_limb[t] + rand[t];
        delta[t] <== Cmsg[t] - sum_i[t];

        // --- Numeric limb logs (match generator and are human-traceable):
        // Tag = 900200 + t, then we print:
        //   1) tag
        //   2) msg_i
        //   3) rand_i
        //   4) sum_i
        //   5) Cmsg[i]
        //   6) delta = Cmsg[i] - sum_i (should be 0)
        log(900200 + t);
        log(msg_limb[t]);
        log(rand[t]);
        log(sum_i[t]);
        log(Cmsg[t]);
        log(delta[t]);

        // Constraint
        Cmsg[t] === sum_i[t];
    }

    // ------------------------------------------------------------------
    // Schnorr challenge binding (Cardano SHA-512 over (R, A, m_bits))
    // ------------------------------------------------------------------
    component hash = Sha512_hash_bits_digest(n + 512);

    // Feed R8 (256 bits) then A (256 bits), reversing bits within each byte
    for (i = 0; i < 256; i += 8) {
        for (j = 0; j < 8; j++) {
            hash.inp_bits[i + j]       <== R8[i + (7 - j)];
            hash.inp_bits[256 + i + j] <== A [i + (7 - j)];
        }
    }
    // Append message bits
    for (i = 0; i < n; i++) {
        msg[i] ==> hash.inp_bits[512 + i];
    }

    // Check digest; also log each byte with a numeric tag:
    // Tag = 900300 + i → then value
    for (i = 0; i < 64; i++) {
        log(900300 + i);
        log(hash.hash_bytes[i]);
        c_schnorr[i] === hash.hash_bytes[i];
    }
}

// Top-level
component main { public [A, R8, ek, C0, Cmsg, c_schnorr, msg_pub] } = CardanoWBPS(9*254, 333, 32);