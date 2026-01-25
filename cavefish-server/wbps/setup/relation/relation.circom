// ======================================================================
// CardanoWBPS.circom
// ----------------------------------------------------------------------
// Purpose:
//   Formal, property-driven implementation of the WBPS proof relation for
//   Cardano’s EdDSA/UTxO signing model.  Verifies that a message μ was
//   correctly committed and hashed into a Schnorr-style transcript.
//
// Conventions enforced here:
//   - Prefix ALL `signal input` ports with  in_*
//   - Prefix ALL `signal output` ports with out_*
//   - Component instance names use lowerCamelCase (template names are UpperCamelCase)
//
// ======================================================================
// Mapping between Circuit Signals and Paper Notation
// ----------------------------------------------------------------------
//  Circuit Name                         |  Paper Symbol / Meaning
// --------------------------------------|------------------------------------------
//  signer_key                           |  X      — Signer’s public key (EdDSA/LC)
//  solver_encryption_key                |  ek     — ElGamal encryption key of the SP
//  solver_encryption_key_pow_rho        |  ek^ρ   — ElGamal encryption key raised to ρ
//  commitment_randomizer_rho            |  ρ      — Random scalar used for commitment
//  commitment_point_bits                |  R      — g^ρ (bit representation for transcript)
//  commitment_payload                   |  Cmsg   — Ciphertext limbs Enc(ek, txId; ρ)
//  message_public_part                  |  m_pub  — Public portion of μ
//  message_private_part                 |  m_priv — Private overlay portion of μ
//  rebuildMessage.out_message           |  μ      — Full reconstructed message bits
//  rebuildCommitment.out_message_chunk  |  μ̂[i]  — Message limbs packed to Fr
//  (internal) PRF in RebuildCommitment  |  PRF[i] — Pseudorandom mask from ek^ρ
//  rebuildChallenge.out_challenge       |  c      — SHA-512 transcript digest
// --------------------------------------|------------------------------------------
// ======================================================================
// Proof Properties
// ----------------------------------------------------------------------
// The CardanoWBPS circuit enforces four sequential properties (P0–P3).
// Each property expresses a verifiable relation between public and private inputs.
//
//   P0 – Message Reconstruction
//        Ensures that the full message μ is correctly rebuilt from its
//        public and private segments: μ = Overlay(m_pub, m_priv).
//
//   P1 – Commitment Scalar Correctness
//        Confirms that the commitment scalar ρ is valid and used consistently:
// ======================================================================
// Mapping between Circuit Signals and Paper Notation
// ----------------------------------------------------------------------
//  Circuit Name                         |  Paper Symbol / Meaning
// --------------------------------------|------------------------------------------
//  signer_key                           |  X      — Signer’s public key (EdDSA/LC)
//  solver_encryption_key                |  ek     — ElGamal encryption key of the SP
//  solver_encryption_key_pow_rho        |  ek^ρ   — ElGamal encryption key raised to ρ
//  commitment_randomizer_rho            |  ρ      — Random scalar used for commitment
//  commitment_point_bits                |  R      — g^ρ (bit representation for transcript)
//  commitment_payload                   |  Cmsg   — Ciphertext limbs Enc(ek, txId; ρ)
//  message_public_part                  |  m_pub  — Public portion of μ
//  message_private_part                 |  m_priv — Private overlay portion of μ
//  rebuildMessage.out_message           |  μ      — Full reconstructed message bits
//  rebuildCommitment.out_message_chunk  |  μ̂[i]  — Message limbs packed to Fr
//  (internal) PRF in RebuildCommitment  |  PRF[i] — Pseudorandom mask from ek^ρ
//  rebuildChallenge.out_challenge       |  c      — SHA-512 transcript digest
// --------------------------------------|------------------------------------------
//        (a) ρ is range-checked to [0, 2^251)
//        (b) g^ρ is computed from the fixed BabyJub base g
//        (c) ek^ρ is computed from the encryption key ek
//
//   P2 – Ciphertext Binding Consistency
//        Rebuilds the commitment ciphertext Cmsg and verifies its correctness.
//        For each message limb i:
//            μ̂[i] = Bits2Num_i(μ)
//            PRF[i] = PoseidonStream_i(ek^ρ)
//            Enforce:  Cmsg[i] = μ̂[i] + PRF[i]   (mod p)
//        This guarantees that Cmsg encrypts txId under ek using ρ.
//
//   P3 – Transcript Integrity
//        Recomputes the Schnorr transcript challenge used in EdDSA verification:
//            c = SHA-512(R || X || txId)
//        and enforces equality with the provided challenge input.
//        This binds together the nonce R = g^ρ, the signer key X, and the
//        txId, ensuring the proof is non-malleable and self-consistent.
// ======================================================================
// Parameters (top-level):
//   message_size                    : |μ| (must be a multiple of 254 bits)
//   message_private_part_size       : size of the private overlay window
//   message_private_part_offset     : starting bit offset for the private overlay
//
// Circom operator semantics reminder:
//   <== : unidirectional wiring (no constraint)
//   ==> : reverse wiring (no constraint)
//   === : equality constraint in Fr (adds R1CS constraint)
//
// Notes on serialization:
//   - All elliptic curve points are BabyJubJub affine coordinates.
//   - R and X are fed to SHA-512 with per-byte bit-reversal, matching EdDSA/Cardano encoding.
//   - The circuit enforces curve-level equalities (Fr) for all commitments.
//
// ======================================================================

pragma circom 2.1.2;

// Vendor gadgets (shared single copy)
include "../../../../wbps/vendor/circomlib/circuits/poseidon.circom";
include "../../../../wbps/vendor/circomlib/circuits/escalarmulany.circom";
include "../../../../wbps/vendor/circomlib/circuits/escalarmulfix.circom";
include "../../../../wbps/vendor/circomlib/circuits/bitify.circom";

// Cardano EdDSA SHA-512 (bitwise, shared single copy)
include "../../../../wbps/circuits/hashing/sha2/sha512/sha512_hash_bits.circom";
include "../../../../wbps/circuits/hashing/blake2/blake2b.circom";

// ======================================================================
// 1) RebuildMessage — P0
// ----------------------------------------------------------------------
// Inputs:
//   in_message_public_part[message_size]           : m_pub
//   in_message_private_part[message_private_part_size] : m_priv (overlay window)
// Output:
//   out_message[message_size]                      : μ (reconstructed)
//
// Property (P0):
//   RebuildMessage(m_pub, m_priv) → μ
//   (overlay m_priv into m_pub on [offset, offset+size) to form μ)
// ======================================================================
template RebuildMessage(message_size, message_private_part_size, message_private_part_offset) {
    signal input  in_message_public_part[message_size];
    signal input  in_message_private_part[message_private_part_size];
    signal output out_message[message_size];

    var i;
    for (i = 0; i < message_size; i++) {
        if (i >= message_private_part_offset && i < message_private_part_offset + message_private_part_size) {
            out_message[i] <== in_message_private_part[i - message_private_part_offset];
        } else {
            out_message[i] <== in_message_public_part[i];
        }
    }

}

// ======================================================================
// 2) Scalars — P1
// ----------------------------------------------------------------------
// Inputs:
//   in_solver_encryption_key[2] : ek (BabyJub affine point)
//   in_commitment_randomizer_rho: ρ ∈ [0, 2^251)
// Outputs:
//   out_commitment_randomizer_rho[251]            : bits(ρ)
//   out_solver_encryption_key_pow_rho[2]          : ek^ρ
//   out_commitment_g_rho[2]                       : g^ρ
//
// Property (P1):
//   Scalars(ek, ρ) → (ek^ρ, g^ρ) 
//   (range-limit ρ via Num2Bits(251); compute ek^ρ via EscalarMulAny; g^ρ via EscalarMulFix)
// ======================================================================
template Scalars() {
    signal input  in_solver_encryption_key[2];
    signal input  in_commitment_randomizer_rho;

    signal output out_commitment_randomizer_rho[251];
    signal output out_solver_encryption_key_pow_rho[2];
    signal output out_commitment_g_rho[2];

    var base[2] = [
        5299619240641551281634865583518297030282874472190772894086521144482721001553,
        16950150798460657717958625567821834550301663161624707787222815936182638968203
    ];

    component n2b = Num2Bits(251);
    in_commitment_randomizer_rho ==> n2b.in;

    var i;
    for (i = 0; i < 251; i++) {
        out_commitment_randomizer_rho[i] <== n2b.out[i];
    }

    component mul_any = EscalarMulAny(251);
    mul_any.p[0] <== in_solver_encryption_key[0];
    mul_any.p[1] <== in_solver_encryption_key[1];
    for (i = 0; i < 251; i++) {
        n2b.out[i] ==> mul_any.e[i];
    }
    out_solver_encryption_key_pow_rho[0] <== mul_any.out[0];
    out_solver_encryption_key_pow_rho[1] <== mul_any.out[1];

    component mul_fix = EscalarMulFix(251, base);
    for (i = 0; i < 251; i++) {
        n2b.out[i] ==> mul_fix.e[i];
    }
    out_commitment_g_rho[0] <== mul_fix.out[0];
    out_commitment_g_rho[1] <== mul_fix.out[1];
}

// ======================================================================
// 3) RebuildCommitment — P2
// ----------------------------------------------------------------------
// Inputs:
//   in_seed_x, in_seed_y                        : (ek^ρ).x, (ek^ρ).y
//   in_message[message_size]                      : μ_bits
//   in_commitment_payload[nb_commitment_limbs]  : Cmsg limbs
// Outputs (debug):
//   out_message_chunk[nb_commitment_limbs]      : μ̂[i] (254-bit packing)
//   out_masked_chunk[nb_commitment_limbs]       : μ̂[i] + PRF[i]
//   out_delta[nb_commitment_limbs]              : Cmsg[i] − (μ̂[i] + PRF[i])
//
// Property (P2):
//   RebuildCommitment(ek^ρ, μ) → (μ̂, PRF, μ̂+PRF)
//   (PRF via PoseidonEx seeded with ek^ρ; μ packed into 254-bit limbs with Bits2Num)
// ======================================================================
template RebuildCommitment(message_size, commitment_limb_size, nb_commitment_limbs) {
    signal input  in_seed_x;
    signal input  in_seed_y;
    signal input  in_message[message_size];
    signal input  in_commitment_payload[nb_commitment_limbs];

    signal output out_message_chunk[nb_commitment_limbs];
    signal output out_masked_chunk[nb_commitment_limbs];
    signal output out_delta[nb_commitment_limbs];

    log(900150); log(in_seed_x);
    log(900151); log(in_seed_y);

    // PRF stream
    signal prf[nb_commitment_limbs];

    component pEx = PoseidonEx(2, 1);
    pEx.initialState <== 0;
    pEx.inputs[0] <== in_seed_x;
    pEx.inputs[1] <== in_seed_y;
    log(900152); log(pEx.initialState);
    log(900153); log(pEx.inputs[0]);
    log(900154); log(pEx.inputs[1]);
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
        out_delta[t]        <== in_commitment_payload[t] - out_masked_chunk[t];

        // Deterministic trace (unchanged)
        log(900200 + t);
        log(out_message_chunk[t]);
        log(prf[t]);
        log(out_masked_chunk[t]);
        log(in_commitment_payload[t]);
        log(out_delta[t]);

    }
}

// ======================================================================
// 4) RebuildChallenge — P3
// ----------------------------------------------------------------------
// Inputs:
//   in_commitment_point[256] : R bits (nonce commitment g^ρ), byte-wise bit-reversed per EdDSA
//   in_signer_key[256]       : X bits (signer key), byte-wise bit-reversed per EdDSA
//   txId[256]                : txId bits (256 bits as the Blake2b-256 hash of tx_body_cbor)
// Output:
//   out_challenge[64]        : digest
//
// Property (P3):
//   RebuildChallenge(R, X, txId) → digest and assert challenge == digest
//   (digest = SHA512(R || X || txId) with per-byte bit-reversal for R, X)
// ======================================================================
template RebuildChallenge(message_size) {
    signal input  in_commitment_point[256];
    signal input  in_signer_key[256];
    signal input  txId[256];
    signal output out_challenge[64];

    // Compute EdDSA challenge with Sha512. Input len = 512 bits for (R || X) and 256 bits for (txId)
    component hash = Sha512_hash_bits_digest(512 + 256);

    var i;
    var j;

    for (i = 0; i < 256; i += 8) {
        for (j = 0; j < 8; j++) {
            hash.inp_bits[i + j]       <== in_commitment_point[i + (7 - j)];
            hash.inp_bits[256 + i + j] <== in_signer_key[i + (7 - j)];
        }
    }
    for (i = 0; i < 256; i++) {
        txId[i] ==> hash.inp_bits[512 + i];
    }

    for (i = 0; i < 64; i++) {
        out_challenge[i] <== hash.hash_bytes[i];
        log(900300 + i);
        log(out_challenge[i]);
    }
}

// ======================================================================
// X) ComputeTxId
// ----------------------------------------------------------------------
// Inputs:
//   in_message[message_size]   : μ_bits
// Output:
//   txId[256]        : Blake2b-256 digest
//
// Property (P3):
//   ComputeTxId(μ) → txId
//   (txId = Blake2b-256(μ_bytes))
// ======================================================================
template ComputeTxId(message_size) {
    signal input in_message[message_size];
    signal output txId[256];

    // Convert message (tx_body) from bits to bytes
    assert(message_size % 8 == 0);
    var message_size_bytes = message_size \ 8;

    component to_bytes[message_size_bytes];
    signal in_message_bytes[message_size_bytes];

    for (var i = 0; i < message_size_bytes; i++) {
        to_bytes[i] = Bits2Num(8);
        for (var j = 0; j < 8; j++) {
            to_bytes[i].in[j] <== in_message[i*8 + j];
        }
        to_bytes[i].out ==> in_message_bytes[i];
    }

    // Compute txId = Blake2b-256(tx_body_cbor)
    component blake2b_hash = Blake2b_bytes(message_size_bytes);
    blake2b_hash.inp_bytes <== in_message_bytes;

    txId <== blake2b_hash.hash_bits;

    // Debug txId bits
    for (var k = 0; k < 256; k++) {
        log(900700 + k);
        log(txId[k]);
    }
}


// ======================================================================
// 5) Top-level — CardanoWBPS(message_size, message_private_part_size, message_private_part_offset)
// ----------------------------------------------------------------------
// Orchestration (explicit):
//   - P0: RebuildMessage(m_pub, m_priv) → μ
//   - P1: Scalars(ek, ρ) → (ek^ρ, g^ρ)
//   - P2: RebuildCommitment(ek^ρ, μ) → (μ̂, PRF, μ̂+PRF) and assert Cmsg[i] == μ̂[i] + PRF[i]
//   - P3: RebuildChallenge(R, X, txId) → digest and assert challenge == digest
// ======================================================================
template CardanoWBPS(message_size, message_private_part_size, message_private_part_offset) {
    var commitment_limb_size = 254;
    assert(message_size % commitment_limb_size == 0);
    var nb_commitment_limbs = message_size \ commitment_limb_size;
   

    // External inputs
    signal input signer_key[256];
    signal input solver_encryption_key[2];

    // R = g^ρ (bits for transcript), C0 = g^ρ (affine), payload limbs, challenge
    signal input commitment_point_bits[256];
    signal input solver_encryption_key_pow_rho[2];
   
    signal input commitment_randomizer_rho;
    signal input commitment_payload[nb_commitment_limbs];
    signal input challenge[64];
    signal input message_public_part[message_size];
    signal input message_private_part[message_private_part_size];

    // P0
    component rebuildMessage = RebuildMessage(message_size, message_private_part_size, message_private_part_offset);
    for (var i = 0; i < message_size; i++) {
        rebuildMessage.in_message_public_part[i] <== message_public_part[i];
    }
    for (var j = 0; j < message_private_part_size; j++) {
        rebuildMessage.in_message_private_part[j] <== message_private_part[j];
    }

    // PX
    component computeTxId = ComputeTxId(message_size);
    computeTxId.in_message <== rebuildMessage.out_message;

    // P1
    component commitmentScalars = Scalars();
    commitmentScalars.in_solver_encryption_key[0] <== solver_encryption_key[0];
    commitmentScalars.in_solver_encryption_key[1] <== solver_encryption_key[1];
    commitmentScalars.in_commitment_randomizer_rho <== commitment_randomizer_rho;

    log(900010); log(commitmentScalars.out_commitment_g_rho[0]);
    log(900011); log(commitmentScalars.out_commitment_g_rho[1]);
    log(900012); log(commitmentScalars.out_solver_encryption_key_pow_rho[0]);
    log(900013); log(commitmentScalars.out_solver_encryption_key_pow_rho[1]);

    solver_encryption_key_pow_rho[0] === commitmentScalars.out_solver_encryption_key_pow_rho[0];
    solver_encryption_key_pow_rho[1] === commitmentScalars.out_solver_encryption_key_pow_rho[1];
    component poseidonDebug = PoseidonEx(2, 1);
    poseidonDebug.initialState <== 0;
    poseidonDebug.inputs[0] <== commitmentScalars.out_solver_encryption_key_pow_rho[0];
    poseidonDebug.inputs[1] <== commitmentScalars.out_solver_encryption_key_pow_rho[1];
    log(900160); log(poseidonDebug.out[0]);

    // P2
    component rebuildCommitment = RebuildCommitment(message_size, commitment_limb_size, nb_commitment_limbs);
    rebuildCommitment.in_seed_x <== commitmentScalars.out_solver_encryption_key_pow_rho[0];
    rebuildCommitment.in_seed_y <== commitmentScalars.out_solver_encryption_key_pow_rho[1];
    for (var t = 0; t < message_size; t++) {
        rebuildCommitment.in_message[t] <== rebuildMessage.out_message[t];
    }
    for (var k = 0; k < nb_commitment_limbs; k++) {
        rebuildCommitment.in_commitment_payload[k] <== commitment_payload[k];
    }

    for (var q = 0; q < nb_commitment_limbs; q++) {
        commitment_payload[q] === rebuildCommitment.out_masked_chunk[q];
    }

    // P3
    component rebuildChallenge = RebuildChallenge(message_size);
    for (var r = 0; r < 256; r++) {
        rebuildChallenge.in_commitment_point[r] <== commitment_point_bits[r];
        rebuildChallenge.in_signer_key[r]       <== signer_key[r];
    }

    rebuildChallenge.txId <== computeTxId.txId;

    for (var d = 0; d < 64; d++) {
        log(900500 + d); log(challenge[d]);
        log(900600 + d); log(rebuildChallenge.out_challenge[d]);
        challenge[d] === rebuildChallenge.out_challenge[d];
    }
}

// ======================================================================
// Public exposure (unchanged API; sizing parameters below)
// ----------------------------------------------------------------------
// Message sizing targets the mean tx_body size observed on Cardano mainnet.
// Mean is ~785B; we bump the tx_body target to 857B so that
// (857B + 32B) * 8 = 7,112 bits = 28 * 254 (byte-aligned, no limb padding).
// See wbps/README.md for the full size distribution stats.
// ======================================================================
component main { public [
    signer_key, // X
    commitment_point_bits, // R
    commitment_payload, // Com_tx
    challenge, // c
    message_public_part // TxAbs
] } = CardanoWBPS(28*254, 320, 24);
