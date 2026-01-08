// BuildCommitment circuit (P2)
// Uses circomlib PoseidonEx (rate 2) and Bits2Num for packing/masking.
// Note: This mirrors the wbps_cardano.circom variant (254-bit limbs) so the
// verifier can rebuild the same commitment without mod-p aliasing.

pragma circom 2.1.2;

// Circom library path: use circuits/ relative to the include search path.
include "poseidon.circom";
// Use the canonical circomlib bit packing to avoid JS shift overflow on large limbs.
include "bitify.circom";

// ======================================================================
// 3) BuildCommitment — P2 (builder only)
// ----------------------------------------------------------------------
// Inputs:
//   in_seed_x, in_seed_y                        : (ek^ρ).x, (ek^ρ).y
//   in_message[message_size]                    : μ_bits
// Outputs:
//   out_message_chunk[nb_commitment_limbs]      : μ̂[i] (packing)
//   out_masked_chunk[nb_commitment_limbs]       : μ̂[i] + PRF[i]
//
// Property (P2):
//   BuildCommitment(ek^ρ, μ) → (μ̂, PRF, μ̂+PRF) — commitment builder only
//   (PRF via PoseidonEx seeded with ek^ρ; μ packed into limbs with Bits2Num)
// ======================================================================
template BuildCommitment(message_size, commitment_limb_size, nb_commitment_limbs) {
    signal input  in_seed_x;
    signal input  in_seed_y;
    signal input  in_message[message_size];

    signal output out_message_chunk[nb_commitment_limbs];
    signal output out_masked_chunk[nb_commitment_limbs];

    // PRF stream
    signal prf[nb_commitment_limbs];

    component pEx = PoseidonEx(2, 1);
    pEx.initialState <== 0;
    pEx.inputs[0] <== in_seed_x;
    pEx.inputs[1] <== in_seed_y;
    prf[0] <== pEx.out[0];

    component nexts[nb_commitment_limbs];
    var k;
    for (k = 1; k < nb_commitment_limbs; k += 2) {
        nexts[k] = PoseidonEx(1, 2);
        nexts[k].initialState <== 0;
        nexts[k].inputs[0] <== prf[k - 1];

        prf[k] <== nexts[k].out[0];
        if (k + 1 < nb_commitment_limbs) {
            prf[k + 1] <== nexts[k].out[1];
        }
    }

    // Pack μ and compute masked limbs
    component pack[nb_commitment_limbs];
    var t;
    var b;

    for (t = 0; t < nb_commitment_limbs; t++) {
        pack[t] = Bits2Num(commitment_limb_size);
        for (b = 0; b < commitment_limb_size; b++) {
            pack[t].in[b] <== in_message[t*commitment_limb_size + b];
        }
        out_message_chunk[t] <== pack[t].out;

        out_masked_chunk[t] <== out_message_chunk[t] + prf[t];

        // Deterministic trace (unchanged)
        log(900200 + t);
        log(out_message_chunk[t]);
        log(prf[t]);
        log(out_masked_chunk[t]);

    }
}

// Sizing targets the mean tx_body size observed on Cardano mainnet history.
// We round mean (~785B) to 800B and include a 32B nonce, then pad to 254-bit limbs:
//   (800B + 32B) * 8 = 6,656 bits -> 27 * 254 = 6,858 bits (202 bits padding).
// Using 254-bit limbs matches wbps_cardano.
// See wbps/README.md for the full size distribution stats.
component main = BuildCommitment(27*254, 254, 27);
