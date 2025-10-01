// ─────────────────────────────────────────────────────────────────────────────
// CIRCOM VERSION AND LIBRARIES
// ─────────────────────────────────────────────────────────────────────────────

// Circom compiler version (syntax/features). Your file targets Circom 2.1.2.
pragma circom 2.1.2;

// Cryptographic gadgets used inside the circuit:

// Poseidon sponge/hash (field-friendly hash used here as a PRF/OTP stream for enc).
// Poseidon will be used as a pseudorandom generator to create a keystream (like a one-time pad) for encrypting data.
include "../circomlib/circuits/poseidon.circom";

// Scalar multiplication gadgets:
//  - EscalarMulAny: scalar mul by an arbitrary point provided at runtime
//  - EscalarMulFix: scalar mul by a fixed (hardcoded) point (faster / constraint-cheaper)
include "../circomlib/circuits/escalarmulany.circom";

// Bitwise SHA-512 implementation (we need it because Cardano’s EdDSA uses SHA-512).
include "../hashing_circuits/sha2/sha512/sha512_hash_bits.circom";


// ─────────────────────────────────────────────────────────────────────────────
// TEMPLATE: CardanoWBPS
// ─────────────────────────────────────────────────────────────────────────────
// Parameters:
//   n     : total number of message bits `m` (tx || aux) the circuit will handle.
//   sb_n  : number of *private* message bits (hidden slice you prove about).
//   sb_o  : offset (starting index) where the private slice is inserted inside `m`.
//            Everything else comes from `msg_pub` (public-projection bits).
//
// High-level role:
//   This circuit enforces the core WBPS relations used in Cavefish for Cardano:
//   - ElGamal-like ENC binding:   C = Enc(ek, m ; ρ)  (stream-OTP over field limbs)
//   - Schnorr challenge binding:  c = H(R, X, m)      (Cardano uses SHA-512)
//   - (Outside this file you’d also check intent on TxAbs, etc.)
//
template CardanoWBPS(n, sb_n, sb_o) {

    // BN254 field has 254-bit modulus. Most Circom libs use 254-bit field elements.
    // We’ll pack message into 254-bit limbs (not 255/256) to stay safe in-field.
    var bits = 254;

    // Sanity: total message length must be divisible by `bits` so we can chunk it.
    assert(n % bits == 0);

    // Number of 254-bit limbs to represent the `n`-bit message.
    var cmsg_elem = n\bits;   // (integer division)

    // ─────────────────────────────────────────────────────────────────────────
    // PUBLIC INPUTS (verifier sees these)
    // ─────────────────────────────────────────────────────────────────────────

    // A: LC's EdDSA/Schnorr public key X, encoded as 256 bits (curve point encoding).
    //    (You treat these as raw bits for SHA preimage; elsewhere it’s a curve point.)
    signal input A[256];

    // R8: Schnorr nonce point R, also given as 256 bits (byte-wise hashed below).
    signal input R8[256];

    // ek: Public encryption key for the ElGamal/DHIES-like encryption
    //     (we model points as 2 coordinates in this library).
    signal input ek[2];

    // C0: First ciphertext component, C0 = g^ρ (ElGamal-style).
    signal input C0[2];

    // Cmsg: The “message” side of the ciphertext stream. We treat enc as:
    //        for limb i:  Cmsg[i] = msg_limb[i] + rand[i]
    //       where rand[i] is derived from Poseidon( ek^ρ ) like a PRF/OTP.
    signal input Cmsg[cmsg_elem];

    // c_schnorr: The Schnorr challenge bytes (64 bytes for SHA-512).
    //            We compare the circuit’s SHA-512 output against this array.
    signal input c_schnorr[64];

    // msg_pub: public projection (bits) of m = tx || aux. The private slice (sb_n bits)
    //          will be spliced into `msg` at offset `sb_o`.
    signal input msg_pub[n];

    // ─────────────────────────────────────────────────────────────────────────
    // PRIVATE WITNESS INPUTS (prover knows; verifier doesn’t)
    // ─────────────────────────────────────────────────────────────────────────

    // ρ (rho): encryption randomness used for C = Enc(ek, m ; ρ).
    signal input rho;

    // msg_priv: sb_n private bits (the blinded slice of m).
    signal input msg_priv[sb_n];

    // Local loop variables for for-loops
    var i;
    var j;

    // Fixed base point for EscalarMulFix (BabyJubJub base in affine coords).
    // NOTE: “base[2]” is [x, y]. These big constants are the curve’s generator.
    var base[2] = [
        5299619240641551281634865583518297030282874472190772894086521144482721001553,
        16950150798460657717958625567821834550301663161624707787222815936182638968203
    ];

    // ─────────────────────────────────────────────────────────────────────────
    // BUILD THE FULL MESSAGE m (bitwise)
    // ─────────────────────────────────────────────────────────────────────────
    // msg = tx || aux_nt (conceptually).
    // We reconstruct `msg` by taking everything from `msg_pub`, except for the
    // sb_n-bit slice starting at sb_o which we take from `msg_priv`.
    signal msg[n];

    for(i=0; i<n; i++) {
        if(i >= sb_o && i < sb_o+sb_n) {
            // Inside the private window: take the hidden bit.
            msg[i] <== msg_priv[i - sb_o];
        } else {
            // Outside the private window: take the public bit.
            msg[i] <== msg_pub[i];
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // ELGAMAL-LIKE ENCRYPTION BINDING (C = Enc(ek, m ; ρ))
    // ─────────────────────────────────────────────────────────────────────────
    //
    // Model:
    //   - Compute ek^ρ (scalar-mul with arbitrary point ek): a shared secret.
    //   - Hash it with Poseidon to get PRF output; use it as OTP over limbs.
    //   - Also enforce C0 = g^ρ using fixed-base scalar-mul.
    //
    // Bit decomposition of ρ (we only use 251 bits to be safe in the field).
    component n2b_rho1 = Num2Bits(251);

    // Scalar mul by arbitrary point: ek^ρ (out is a 2-coord point).
    component ek_rho1 = EscalarMulAny(251);
    ek_rho1.p[0] <== ek[0];
    ek_rho1.p[1] <== ek[1];

    // Feed ρ bits into both gadgets:
    rho ==> n2b_rho1.in;
    for (i=0; i<251; i++) {
        n2b_rho1.out[i] ==> ek_rho1.e[i];  // e = exponent bits (ρ)
    }

    // Enforce C0 == g^ρ with fixed-base scalar mul.
    component g_pow_rho = EscalarMulFix(251, base);
    for (i=0; i<251; i++) {
        n2b_rho1.out[i] ==> g_pow_rho.e[i];
    }

    // log(...) writes the witness value to stdout at proving time — debug only.
    // Remove in production to avoid leaking info.
    log(g_pow_rho.out[0]);
    log(g_pow_rho.out[1]);

    // Hard constraints (===) enforce equality without consuming either side.
    C0[0] === g_pow_rho.out[0];
    C0[1] === g_pow_rho.out[1];

    // Derive PRF seed from ek^ρ via Poseidon sponge.
    // PoseidonEx(arity_in, arity_out). We set initialState=0 (sponge init).
    component pEx = PoseidonEx(2, 1);
    pEx.initialState <== 0;
    pEx.inputs[0] <== ek_rho1.out[0];
    pEx.inputs[1] <== ek_rho1.out[1];

    // rand[] will be the OTP stream over cmsg_elem limbs.
    var rand[cmsg_elem+1];
    rand[0] = pEx.out[0];  // first squeeze

    // We can’t “arbitrarily squeeze” PoseidonEx in-place, so we chain squeezes:
    // For pairs of limbs, use PoseidonEx(1,2) to expand one input into two outputs.
    // This amortizes cost vs. PoseidonEx(1,1) repeated many times.
    component randEnc[cmsg_elem];
    for (var i=1; i<cmsg_elem; i+=2) {
        randEnc[i] = PoseidonEx(1, 2);
        randEnc[i].initialState <== 0;
        randEnc[i].inputs[0] <== rand[i-1];
        rand[i]   = randEnc[i].out[0];
        rand[i+1] = randEnc[i].out[1];
    }

    // Convert each 254-bit chunk of msg[] into a field element,
    // then enforce: Cmsg[i] = msg_limb[i] + rand[i].
    component msgBits2Num[cmsg_elem];
    for (var i=0; i<cmsg_elem; i++) {
        msgBits2Num[i] = Bits2Num(bits);
        for (var j=0; j<bits; j++) {
            msgBits2Num[i].in[j] <== msg[i*bits + j];
        }

        // Debug. Remove for production.
        log(msgBits2Num[i].out + rand[i]);

        // Ciphertext limb = plaintext limb + OTP rand[i] (mod field)
        Cmsg[i] === msgBits2Num[i].out + rand[i];
    }

    // ─────────────────────────────────────────────────────────────────────────
    // SCHNORR CHALLENGE BINDING (Cardano EdDSA-style)
    // c = H(R, X, m) with SHA-512 over bits
    // ─────────────────────────────────────────────────────────────────────────
    //
    // The SHA-512 gadget expects bits in byte order; below we re-pack R and A
    // by reversing bit order within each byte (j from 0..7 fed from [7-j]).
    // This is a common endian “byte-lane flip” to match hash preimage convention.
    //
    component hash = Sha512_hash_bits_digest(n + 256 + 256);

    // Place R8 then A (each 256 bits) into the first 512 bits of the hash input.
    // For each byte (8 bits), reverse bit order: inp_bits[i+j] gets R8[i+(7-j)].
    for (i=0; i<256; i+=8) {
        for (j=0; j<8; j++) {
            hash.inp_bits[i + j]        <== R8[i + (7 - j)];
            hash.inp_bits[256 + i + j]  <== A [i + (7 - j)];
        }
    }

    // Then append the message bits m (n bits) after the first 512 bits.
    for (i=0; i<n; i++) {
        msg[i] ==> hash.inp_bits[512 + i];
    }

    // Finally, enforce the 64-byte (512-bit) digest equals the provided c_schnorr[].
    for (i=0; i<64; i++) {
        // Debug-print each byte of the computed hash.
        log(hash.hash_bytes[i]);

        // Enforce byte equality. This binds c = H(R, X, m).
        c_schnorr[i] === hash.hash_bytes[i];
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// TOP-LEVEL COMPONENT
// ─────────────────────────────────────────────────────────────────────────────
//
// main’s public signals are declared in the {public [...]} list. Everything else
// is private witness. Here we set:
//   - n    = 9 * 254 bits  (message length; 9 limbs of 254 bits)
//   - sb_n = 333 bits      (size of the private slice)
//   - sb_o = 32            (offset where private slice starts)
//
component main {public [A, R8, ek, C0, Cmsg, c_schnorr, msg_pub]} = CardanoWBPS(9*254, 333, 32);
