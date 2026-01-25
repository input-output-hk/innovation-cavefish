# WBPS — Cardano Encryption & Schnorr Binding Circuit

This folder contains the **Circom circuits** and **Node.js tooling** used in the **Cavefish WBPS (Weakly Blind Predicate Signatures)** prototype.  
The main circuit, `wbps_cardano.circom`, implements **Cardano-style ElGamal encryption** and **SHA-512 Schnorr challenge binding** under the **BabyJubJub** curve.

> ⚠️ **Research prototype only.** Not audited.  
> Do not reuse proving or verification keys in production.

---

## 📁 Layout

```
wbps/
├─ flake.nix                       # Nix shell, development environment
├─ circuits/
│  └─ wbps_cardano.circom          # Main WBPS encryption + Schnorr challenge circuit
│
├─ examples/
│  └─ wbps_cardano_input.json      # Curated example input (public + private bits)
│
├─ tooling/
│  └─ inputgen/
│     ├─ gen_wbps_input.js         # Optional input generator (PoseidonEx-based)
│     └─ package.json
│
├─ vendor/
│  ├─ circomlib/                   # Poseidon, BabyJub, bitify (auto-cloned)
│  └─ hashing_circuits/            # SHA-512 bitwise (for EdDSA-style hashes)
│
├─ Makefile                        # Build automation: compile → witness → setup → prove → verify
└─ build/                          # Generated artifacts (r1cs, wasm, wtns, zkey, etc.)
```

---

## ⚙️ Prerequisites

- Using [NixOS](https://nixos.org/)
- No NixOS

###  Using NixOS

All dependencies are managed via Nix flakes. To enter the development environment, run:

```shell
nix develop
```
This will drop you into a shell where [circom](https://docs.circom.io/), [snarkjs](https://github.com/iden3/snarkjs), Node.js, and Python are available.

### No NixOS
Install the following:

- **circom** ≥ 2.1.0  
  Install globally:
  ```bash
  npm install -g circom
  ```
- **snarkjs** ≥ 0.7.0  
  ```bash
  npm install -g snarkjs
  ```
- **Node.js** ≥ 18  
- **Optional:** Python 3.10+ (for analytics or debugging)

---

## 🚀 Quick Start

From the `wps` folder, you can build and verify the full proof in one command:

```bash
make
```

This performs:
```
1. Clone circomlib into vendor/
2. Compile wbps_cardano.circom → R1CS + WASM 
3. Use example input (examples/wbps_cardano_input.json)
4. Compute witness
5. Setup proving keys (Powers of Tau)
6. Generate and verify Groth16 proof
```

At the end you should see:

```
✅ [Verify] Checking proof
snarkJS: OK!
```

---

## 🧉 Input Handling

The project supports **two input modes**:

| Mode | Command | Description |
|------|----------|-------------|
| **Example Input (default)** | `make` | Uses curated `examples/wbps_cardano_input.json` |
| **Generated Input** | `make with-generated-input` | Rebuilds input via Poseidon-based JS generator |

To inspect generator output:
```bash
make gen-input-debug
```

This logs step-by-step Poseidon and encryption parameters used to produce ciphertext limbs.

---

## Message sizing (tx_body)

We size the circuits around a representative tx_body length derived from mainnet
history. The mean is ~785 bytes, which we round to 800 bytes for sizing, add a 32B
nonce, and then pad to 254-bit limbs:

- tx_body target: 800 bytes (mean ~785 bytes)
- payload: 800B + 32B nonce = 832B = 6,656 bits
- limb packing: 27 * 254 = 6,858 bits (202 bits of zero padding)

Observed tx_body distribution (mainnet history):

- Min: 84 bytes
- Max: 15036 bytes
- Mean: 785 bytes
- Median: 494 bytes
- p75: 906 bytes
- p90: 1326 bytes
- p95: 2512 bytes
- p99: 5983 bytes

Average tx parts (bytes / share):

- Body: 785 bytes (39.4%)
- Witnesses: 1089 bytes (54.6%)
- Aux data: 117 bytes (5.9%)
- Other: 3 bytes (0.1%)

---

## Circuit changelog (recent)

- Challenge input now uses `txId = Blake2b-256(tx_body)` and hashes `R || X || txId` instead of raw `mu` bits.
- Commitment payload still encrypts the full `mu` packed into 254-bit limbs (P2 unchanged).
- `solver_encryption_key_pow_rho` remains an input and is checked against the internally computed `ek^rho` value.
- Top-level `main` parameters remain `message_size = 27*254`, `message_private_part_size = 320`, `message_private_part_offset = 24`.

---

## 🏰 Manual Flow (Debug Mode)

If you prefer explicit commands:

```bash
# 1. Compile circuit
circom circuits/wbps_cardano.circom --r1cs --wasm --sym \
  -l vendor/circomlib -l vendor/hashing_circuits -l .

# 2. Compute witness
snarkjs wtns calculate build/wbps_cardano/wbps_cardano_js/wbps_cardano.wasm \
  examples/wbps_cardano_input.json \
  build/wbps_cardano/wbps_cardano.wtns

# 3. Export public inputs
snarkjs wtns export json build/wbps_cardano/wbps_cardano.wtns \
  build/wbps_cardano/wbps_cardano_public.json

# 4. Trusted setup (phases 1 & 2)
snarkjs powersoftau new bn128 19 build/keys/powersoftau.ptau
snarkjs powersoftau prepare phase2 build/keys/powersoftau.ptau build/keys/pot_final.ptau

# 5. Proving & verification keys
snarkjs groth16 setup build/wbps_cardano/wbps_cardano.r1cs build/keys/pot_final.ptau build/wbps_cardano/wbps_cardano.zkey
snarkjs zkey export verificationkey build/wbps_cardano/wbps_cardano.zkey build/wbps_cardano/wbps_cardano_verifkey.json

# 6. Proof generation & verification
snarkjs groth16 prove build/wbps_cardano/wbps_cardano.zkey \
  build/wbps_cardano/wbps_cardano.wtns \
  build/wbps_cardano/wbps_cardano_proof.json \
  build/wbps_cardano/wbps_cardano_public.json

snarkjs groth16 verify \
  build/wbps_cardano/wbps_cardano_verifkey.json \
  build/wbps_cardano/wbps_cardano_public.json \
  build/wbps_cardano/wbps_cardano_proof.json
```

---

## 🔍 Circuit Logs (numeric tags)

The circuit uses **numeric log tags** for human-traceable debugging.  
Here’s what they mean:

| Tag Range | Description |
|------------|--------------|
| `900010–900011` | `g^ρ` (fixed-base scalar multiplication result) |
| `900200+ t` | Per-limb encryption log (msg, rand, Cmsg, delta) |
| `900300+ i` | SHA-512 digest bytes (Cardano Schnorr challenge) |

These appear in witness-generation logs (`make witness`) for easier correlation with the generator output.

---

## 🧮 Generator Overview

The JS generator (`tooling/inputgen/gen_wbps_input.js`) produces reproducible Poseidon-based ciphertexts:
- Uses `PoseidonEx(2,1)` for initial seed.
- Expands into a stream with chained `PoseidonEx(1,2)` squeezes.
- Computes `Cmsg[i] = msg[i] + rand[i] mod q`.

Run standalone:
```bash
node tooling/inputgen/gen_wbps_input.js --out build/wbps_cardano/wbps_cardano_input.json --debug
```

---

## 🧠 Legacy Note

Older experiments based on `circ_eddsa_cardano.circom` (EdDSA path) have been superseded by this new **WBPS encryption + Schnorr** formulation.  
The current setup focuses on **security modeling**, **log consistency**, and **PoseidonEx correctness** for Cavefish research.
