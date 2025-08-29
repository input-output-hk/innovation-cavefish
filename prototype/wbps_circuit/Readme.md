# Cavefish – WBPS Circuit (EdDSA Cardano)

This folder contains **Circom circuits** and **Python helpers** used in the Cavefish prototype for validating the WBPS (Weakly Blind Predicate Signatures) transaction-building flow. The primary circuit is an **EdDSA (Cardano variant)**; Schnorr experiments are archived under `legacy/`.

> ⚠️ Research prototype only. Not audited. Do not reuse keys/artifacts for production.


## Layout

```

wbps\_circuit/
├─ circ\_eddsa\_cardano.circom             # main EdDSA circuit (Cardano variant)
├─ circ\_eddsa\_cardano\_predicate.circom   # optional predicate-split variant
├─ circ\_BN256\_blake.circom               # helper gadget
├─ circ\_eddsa\_cardano.py                 # Python helpers / input generator
├─ vectors/
│  └─ circ\_eddsa\_cardano\_input.json      # curated input (bits for A, R, S, msg)
└─ legacy/
├─ circ\_schnorr\_bitcoin\*.circom
└─ circ\_schnorr\_bitcoin\*.py

````

## Prerequisites

- [circom](https://docs.circom.io/getting-started/installation/) **2.2.x**  
- [snarkjs](https://github.com/iden3/snarkjs) **0.7.x** (`npm i -g snarkjs@0.7.x`)  
- **Node.js** ≥ 18  
- **Python 3.10+** (for helpers)

This repository already tracks **circomlib** as a submodule at `prototype/circomlib`.

Clone with submodules (recommended):

```bash
git clone --recurse-submodules <repo-url>
````

If you already cloned without `--recurse-submodules`, run:

```bash
git submodule update --init --recursive
```

## Quick Start (recommended)

Use the Makefile to run the whole flow:

```bash
cd prototype/wbps_circuit

# ensure input vector is linked once
[ -f circ_eddsa_cardano_input.json ] || ln -s vectors/circ_eddsa_cardano_input.json circ_eddsa_cardano_input.json

make    # compile → witness → setup → prove → verify
```

At the end, you should see:

```
[INFO]  snarkJS: OK!
```

## Manual Flow (for debugging)

```bash
circom circ_eddsa_cardano.circom --r1cs --wasm --sym \
  -l ../circomlib \
  -l ../hashing_circuits \
  -l .

snarkjs wtns calculate circ_eddsa_cardano_js/circ_eddsa_cardano.wasm \
  circ_eddsa_cardano_input.json \
  circ_eddsa_cardano.wtns

snarkjs wtns export json circ_eddsa_cardano.wtns circ_eddsa_cardano_public.json
snarkjs powersoftau new bn128 19 powersoftau.ptau
snarkjs powersoftau prepare phase2 powersoftau.ptau pot_final.ptau
snarkjs groth16 setup circ_eddsa_cardano.r1cs pot_final.ptau circ_eddsa_cardano.zkey
snarkjs zkey export verificationkey circ_eddsa_cardano.zkey circ_eddsa_cardano_verifkey.json
snarkjs groth16 prove circ_eddsa_cardano.zkey circ_eddsa_cardano.wtns circ_eddsa_cardano_proof.json circ_eddsa_cardano_public.json
snarkjs groth16 verify circ_eddsa_cardano_verifkey.json circ_eddsa_cardano_public.json circ_eddsa_cardano_proof.json
```

## Signals

* `A[]` – public key bits
* `R[]` – signature nonce point bits
* `S[]` – signature scalar bits
* `msg[]` – message bits

Curated vectors are in `vectors/`.
Use the Python helper to materialize inputs correctly.

## Generating your own vectors

You can create valid input JSONs from a Cardano-style EdDSA key, signature, and message using the included Python helper.

Example: `circ_eddsa_cardano.py`:

```python
#!/usr/bin/env python3
import json

def bits_from_bytes(b: bytes) -> list[int]:
    return [(byte >> i) & 1 for byte in b for i in range(8)]

def make_vector(pk_hex: str, R_hex: str, S_hex: str, msg_hex: str):
    A = bits_from_bytes(bytes.fromhex(pk_hex))
    R = bits_from_bytes(bytes.fromhex(R_hex))
    S = bits_from_bytes(bytes.fromhex(S_hex))
    msg = bits_from_bytes(bytes.fromhex(msg_hex))
    return {"A": A, "R": R, "S": S, "msg": msg}

if __name__ == "__main__":
    pk  = "…"   # 32-byte hex public key
    R   = "…"   # 32-byte hex R component of signature
    S   = "…"   # 32-byte hex S component of signature
    msg = "…"   # message in hex

    vector = make_vector(pk, R, S, msg)
    with open("circ_eddsa_cardano_input.json", "w") as f:
        json.dump(vector, f)
    print("Wrote circ_eddsa_cardano_input.json")
```

Run it:

```bash
python3 circ_eddsa_cardano.py
```

This produces `circ_eddsa_cardano_input.json`, ready for `make witness`.

## Legacy

The `legacy/` folder contains Schnorr circuits and scripts from earlier experiments. They are kept for reference but are not part of the validated flow.

