#!/usr/bin/env python3
import json
import random
import sys
from pathlib import Path

# Optional CLI arg: output path (defaults to ./circ_eddsa_cardano_input.json)
out_path = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("circ_eddsa_cardano_input.json")
out_path.parent.mkdir(parents=True, exist_ok=True)  # ensure parent exists

# In the Cavefish scheme, the prover (Service Provider, SP) proves a statement θ and a witness ω:
#   θ = (X, R, com_tx, TxAbs, c, int_post)      ← "known statement"
#   ω = (m, ρ)                                   ← "witness" (encrypted message and its randomness)
# Paper mapping: X (public key), R (nonce point), com_tx (commitment to tx),
# TxAbs = mkAbs(tx) (transaction abstract), c (Schnorr challenge), int_post (intent),
# m = tx || aux_nt (full message, includes randomized notes field), ρ (PKE randomness).

data = {
    # A[256] —> X (the verifier’s/LC’s Schnorr public key inside vk = (par, X))
    # Stored here as 256 bits (implementation detail for the curve representation).
    # Used by Ver to check gs = R · X^c with c = H(R, X, m).
    "A": [random.randint(0, 1) for _ in range(256)],

    # R8[256] —> R (the Schnorr commitment/nonce point returned by the signer in round 1).
    # Also stored as 256 bits for the curve representation; appears in σ = (R, s).
    "R8": [random.randint(0, 1) for _ in range(256)],

    # ek[2] —> ek (public key of the public-key encryption scheme PKE used to encrypt m = tx || aux_nt).
    # Part of parameters used by the user side when forming the proof π about correct c and policy compliance.
    "ek": ["3421554635436", "8764750980"],

    # C0[2] —> First component of the ciphertext C (e.g., ElGamal-style (C0, Cmsg...)).
    # The paper denotes the whole ciphertext as C = PKE.Enc(m; ρ); many implementations expose (C0, Cmsg).
    "C0": ["6542455432", "1109876543"],

    # Cmsg[9] —> Remaining component(s) of the ciphertext C containing the encryption of m = tx || aux_nt).
    # In-circuit, the message is chunked into field elements; here we mock 9 limbs for illustration.
    # The proof checks that C encrypts m under ek with randomness ρ.
    "Cmsg": [random.randint(0, 250023232) for _ in range(9)],

    # c_schnorr[64] —> c (the Schnorr challenge c = H(R, X, m), split into 64 field-limb chunks).
    # The proof π shows c was computed correctly (binding it to the ciphertext C and the abstract TxAbs/int_post).
    "c_schnorr": [random.randint(0, 250023232) for _ in range(64)],

    # msg_pub[254*9] —> public projection of m used in θ (e.g., fixed-size bitstring chunks exposed as "public message bits").
    # In Cavefish, the verifier checks policy compliance against the transaction abstract:
    #   chkSpec(int_post, mkAbs(tx))  and binds c to m = tx || aux_nt.
    # These bits correspond to the public parts that the circuit exposes while keeping (m, ρ) as witness.
    "msg_pub": [random.randint(0, 1) for _ in range(254 * 9)],

    # rho —> ρ (the PKE encryption randomness for C = PKE.Enc(m; ρ)).
    # This is part of the witness ω and never revealed; it links the ciphertext C to the cleartext m inside the proof π.
    "rho": "2389324445",

    # msg_priv[333] —> private components of m (the full message m = tx || aux_nt split into private limb bits).
    # In the relation RCavefish, (m, ρ) is the witness. The circuit also uses TxAbs = mkAbs(tx) on the public side:
    #   it checks chkSpec(int_post, TxAbs) = 1 and binds c = H(R, X, m), ensuring the signed tx satisfies the intent.
    "msg_priv": [random.randint(0, 1) for _ in range(333)]
}

with out_path.open("w") as json_file:
    json.dump(data, json_file, indent=4)

print(f"{out_path} created (fields mapped to θ and ω per Cavefish/WBPS).")
