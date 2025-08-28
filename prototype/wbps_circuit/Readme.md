# Cavefish Prototype – Light-Client ZK building blocks

This folder contains **prototype code** used to validate cryptographic primitives for the Cavefish light‑client initiative. It includes:

* **Circom circuits** (Groth16 on BN128) for Cardano‑style EdDSA (and some legacy Schnorr experiments)
* **Python scripts** used to generate/validate test vectors
* A small **command cookbook** for compiling circuits, generating witnesses, running a trusted setup, producing/validating proofs, and organizing artifacts

> ⚠️ This is **research prototype code**. It is **not** production‑hardened and has **known limitations** (no audited circuits, dev keys, single‑machine “Powers of Tau”, etc.). Use only for experimentation.

---

## 1) Folder layout

```
prototype/
├─ hashing_circuits/                # (placeholder) hash gadgets, Keccak/Blake2s etc.
├─ txhash/                          # (placeholder) transaction hashing experiments
└─ wbps_circuit/                    # Witness‑Based Proof System: Circom + Python
   ├─ legacy/                       # Older experiments kept for reference
   ├─ circ_BN256_blake.circom       # Blake gadget on BN256
   ├─ circ_eddsa_cardano.circom     # **Main**: EdDSA (Cardano variant) circuit
   ├─ circ_eddsa_cardano_predicate.circom  # Optional predicate/constraint split
   ├─ circ_eddsa_cardano.py         # Python helpers / test vectors
   ├─ circ_schnorr_bitcoin*.py      # Legacy Schnorr experiments (Bitcoin style)
   ├─ circ_schnorr_bitcoin*.circom  # Legacy circuits (may be removed later)
   ├─ commands.md                   # Original scratch commands (now codified below)
   └─ (generated) circ_eddsa_cardano_js/   # circom compiled JS + WASM + witness
```

---

## 2) Prerequisites

* **Node.js ≥ 18** and **npm**
* **circom** ≥ 2.1.x (CLI available in PATH)
* **snarkjs** ≥ 0.7.x (`npm i -g snarkjs`)
* **Python 3.10+** with `pip`
* (Optional) **Rust/cargo** for faster builds if you use circom‐rs variants

> Check versions: `circom --version`, `node -v`, `snarkjs -v`, `python3 --version`.

---

## 3) Quick start (EdDSA – Cardano)

All commands are executed from `prototype/wbps_circuit` unless noted otherwise.

### 3.1 Install JS deps (first time only)

```bash
npm init -y
npm i snarkjs
```

(If you installed snarkjs globally, you can skip the local install.)

### 3.2 Compile the circuit

```bash
# Generates .r1cs, .wasm and .sym into ./
circom circ_eddsa_cardano.circom --r1cs --wasm --sym
```

Artifacts produced:

* `circ_eddsa_cardano.r1cs` – Rank‑1 constraint system
* `circ_eddsa_cardano_js/` – WASM & JS wrapper for witness generation
* `circ_eddsa_cardano.sym` – Signal names (debugging)

### 3.3 Prepare a witness

Prepare an input JSON for the circuit (example fields depend on the circuit signals). Save as `circ_eddsa_cardano_input.json`.

Generate the witness using the auto‑generated runner:

```bash
node circ_eddsa_cardano_js/generate_witness.js \
  circ_eddsa_cardano_js/circ_eddsa_cardano.wasm \
  circ_eddsa_cardano_input.json \
  circ_eddsa_cardano.wtns
```

Export public signals (to be verified on‑chain/off‑chain):

```bash
snarkjs wtns export json circ_eddsa_cardano.wtns circ_eddsa_cardano_public.json
```

### 3.4 Trusted setup (Groth16 on BN128)

> For R\&D only. Do **not** reuse these keys for production.

Create/prepare Powers of Tau and circuit‑specific keys:

```bash
snarkjs powersoftau new bn128 19 powersoftau.ptau
snarkjs powersoftau prepare phase2 powersoftau.ptau pot_final.ptau
snarkjs groth16 setup circ_eddsa_cardano.r1cs pot_final.ptau circ_eddsa_cardano.zkey
```

(Optional) export verification key:

```bash
snarkjs zkey export verificationkey \
  circ_eddsa_cardano.zkey \
  circ_eddsa_cardano_verifkey.json
```

### 3.5 Prove & verify

Produce a proof (uses witness + zkey):

```bash
snarkjs groth16 prove \
  circ_eddsa_cardano.zkey \
  circ_eddsa_cardano.wtns \
  circ_eddsa_cardano_proof.json \
  circ_eddsa_cardano_public.json
```

Verify it:

```bash
snarkjs groth16 verify \
  circ_eddsa_cardano_verifkey.json \
  circ_eddsa_cardano_public.json \
  circ_eddsa_cardano_proof.json
```

If verification prints `OK`, the proof is valid for the provided public inputs.

---

## 4) Python utilities

* `circ_eddsa_cardano.py` – convenience functions to:

  * generate EdDSA keys & signatures using Cardano parameters
  * materialize circuit inputs (`*_input.json`)
  * cross‑check signatures against circuit constraints
* `circ_schnorr_bitcoin_*.py` – legacy experiments for Bitcoin Schnorr; **not used** in the Cardano EdDSA flow.

> Use `python3 -m venv .venv && source .venv/bin/activate && pip install -r requirements.txt` (a minimal `requirements.txt` is provided in this draft; update as needed).

Example (pseudo‑code):

```bash
python3 circ_eddsa_cardano.py \
  --msg ./vectors/msg.bin \
  --pk ./vectors/pubkey.hex \
  --sig ./vectors/sig.hex \
  --out circ_eddsa_cardano_input.json
```

---

## 5) Makefile targets (optional)

A developer‑friendly Makefile is provided to codify the above steps:

```Makefile
# Makefile (wbps_circuit)
CIRCUIT := circ_eddsa_cardano
PTAU_BITS := 19

all: compile witness setup prove verify

compile:
	circom $(CIRCUIT).circom --r1cs --wasm --sym

witness:
	node $(CIRCUIT)_js/generate_witness.js \
		$(CIRCUIT)_js/$(CIRCUIT).wasm \
		$(CIRCUIT)_input.json \
		$(CIRCUIT).wtns
	snarkjs wtns export json $(CIRCUIT).wtns $(CIRCUIT)_public.json

setup:
	snarkjs powersoftau new bn128 $(PTAU_BITS) powersoftau.ptau
	snarkjs powersoftau prepare phase2 powersoftau.ptau pot_final.ptau
	snarkjs groth16 setup $(CIRCUIT).r1cs pot_final.ptau $(CIRCUIT).zkey
	snarkjs zkey export verificationkey $(CIRCUIT).zkey $(CIRCUIT)_verifkey.json

prove:
	snarkjs groth16 prove $(CIRCUIT).zkey $(CIRCUIT).wtns $(CIRCUIT)_proof.json $(CIRCUIT)_public.json

verify:
	snarkjs groth16 verify $(CIRCUIT)_verifkey.json $(CIRCUIT)_public.json $(CIRCUIT)_proof.json

clean:
	rm -rf $(CIRCUIT)_js *.r1cs *.sym *.wtns *.zkey *.ptau *_public.json *_proof.json *_verifkey.json
```

Run: `make`, or individual targets like `make compile` / `make verify`.

---

## 6) File naming & artifacts

* Inputs: `circ_eddsa_cardano_input.json`
* Witness: `circ_eddsa_cardano.wtns`
* Public outputs: `circ_eddsa_cardano_public.json`
* Proving key: `circ_eddsa_cardano.zkey`
* Verification key: `circ_eddsa_cardano_verifkey.json`
* Proof: `circ_eddsa_cardano_proof.json`

---

## 7) .gitignore (recommended)

```
# Circom / snarkjs artifacts
*.ptau
*.json
*.js
*.wasm
*.zkey
*.wtns
*.r1cs
*.sym
*.vtc

# Keep inputs only if explicitly versioned
!circ_eddsa_cardano_input.json

# Circom build output dir
wbps_circuit/circ_eddsa_cardano_js/

# Python venv
.venv/
__pycache__/
```

> If you need to keep certain JSON files under version control (e.g., curated test vectors), negate them with `!path/to/file.json` after the generic `*.json` rule.

---

## 8) Reproducibility & security notes

* **Deterministic builds:** record the exact versions of `circom`/`snarkjs`. Consider pinning them in `package.json`.
* **Trusted setup:** never reuse `.zkey`/`.ptau` from this repo for production. For real deployments, run a multi‑party ceremony.
* **Circuit audits:** this circuit is **not audited**. It is suitable only for lab experiments.

---

## 9) Troubleshooting

* `Error: Signal already set` → input JSON mismatches circuit signal names.
* `Invalid proof` → witness/public signals mismatch or wrong zkey.
* `ENOMEM` during compile → pass `--O1` or `--O0` to circom, or increase Node heap (`node --max-old-space-size=8192`).
* `Unknown curve` → ensure `powersoftau new bn128 ...` (BN128) matches Groth16 defaults.

---

## 10) Next steps

* Replace the ad‑hoc inputs with scripted generation via `circ_eddsa_cardano.py`.
* Add CI to compile, prove, and verify on each PR.
* Migrate legacy Schnorr experiments out of the main flow or delete.
* Add minimal test vectors under `wbps_circuit/vectors/`.

---

**Maintainers:** Innovation / Cavefish R\&D

**License:** Apache‑2.0 (proposed; update if different)

---

## Appendix A) Ready-to-paste **Makefile** (in `prototype/wbps_circuit/`)

```makefile
# Cavefish prototype – ZK workflow for circ_eddsa_cardano
CIRCUIT := circ_eddsa_cardano
PTAU_BITS := 19

.PHONY: all compile witness setup prove verify clean

all: compile witness setup prove verify

compile:
	circom $(CIRCUIT).circom --r1cs --wasm --sym

witness:
	node $(CIRCUIT)_js/generate_witness.js \
		$(CIRCUIT)_js/$(CIRCUIT).wasm \
		$(CIRCUIT)_input.json \
		$(CIRCUIT).wtns
	snarkjs wtns export json $(CIRCUIT).wtns $(CIRCUIT)_public.json

setup:
	snarkjs powersoftau new bn128 $(PTAU_BITS) powersoftau.ptau
	snarkjs powersoftau prepare phase2 powersoftau.ptau pot_final.ptau
	snarkjs groth16 setup $(CIRCUIT).r1cs pot_final.ptau $(CIRCUIT).zkey
	snarkjs zkey export verificationkey $(CIRCUIT).zkey $(CIRCUIT)_verifkey.json

prove:
	snarkjs groth16 prove $(CIRCUIT).zkey $(CIRCUIT).wtns $(CIRCUIT)_proof.json $(CIRCUIT)_public.json

verify:
	snarkjs groth16 verify $(CIRCUIT)_verifkey.json $(CIRCUIT)_public.json $(CIRCUIT)_proof.json

clean:
	rm -rf $(CIRCUIT)_js *.r1cs *.sym *.wtns *.zkey *.ptau *_public.json *_proof.json *_verifkey.json
```

---

## Appendix B) `requirements.txt` (Python helpers)

```text
# Minimal deps for Ed25519 / CLI utilities / tests
pynacl>=1.5.0
click>=8.1
pytest>=8.0
```

Optional (unpin if you really need them):

* `requests` (downloading vectors)
* `dataclasses-json` (structured IO)

---

## Appendix C) `package.json` (pin `snarkjs` locally)

Place in `prototype/wbps_circuit/` if you prefer local installs.

```json
{
  "name": "cavefish-wbps-circuit",
  "private": true,
  "type": "module",
  "devDependencies": {
    "snarkjs": "^0.7.3"
  },
  "scripts": {
    "compile": "circom circ_eddsa_cardano.circom --r1cs --wasm --sym",
    "witness": "node circ_eddsa_cardano_js/generate_witness.js circ_eddsa_cardano_js/circ_eddsa_cardano.wasm circ_eddsa_cardano_input.json circ_eddsa_cardano.wtns",
    "export": "snarkjs wtns export json circ_eddsa_cardano.wtns circ_eddsa_cardano_public.json",
    "setup": "snarkjs powersoftau new bn128 19 powersoftau.ptau && snarkjs powersoftau prepare phase2 powersoftau.ptau pot_final.ptau && snarkjs groth16 setup circ_eddsa_cardano.r1cs pot_final.ptau circ_eddsa_cardano.zkey && snarkjs zkey export verificationkey circ_eddsa_cardano.zkey circ_eddsa_cardano_verifkey.json",
    "prove": "snarkjs groth16 prove circ_eddsa_cardano.zkey circ_eddsa_cardano.wtns circ_eddsa_cardano_proof.json circ_eddsa_cardano_public.json",
    "verify": "snarkjs groth16 verify circ_eddsa_cardano_verifkey.json circ_eddsa_cardano_public.json circ_eddsa_cardano_proof.json"
  }
}
```

> You still need `circom` in PATH. Easiest in CI is to use the Docker image `iden3/circom` for the compile step.

---

## Appendix D) GitHub Actions CI (`.github/workflows/zk-ci.yml`)

This workflow compiles the circuit using the `iden3/circom` Docker image, then runs snarkjs on Node.

```yaml
name: ZK prototype CI

on:
  pull_request:
    paths:
      - 'prototype/wbps_circuit/**'
  push:
    branches: [ main ]

jobs:
  build-test:
    runs-on: ubuntu-latest
    defaults:
      run:
        working-directory: prototype/wbps_circuit

    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Ensure artifacts dir
        run: mkdir -p circ_eddsa_cardano_js

      - name: Compile circuit (circom via Docker)
        uses: addnab/docker-run-action@v3
        with:
          image: iden3/circom:latest
          options: -v ${{ github.workspace }}/prototype/wbps_circuit:/work
          run: |
            cd /work
            circom circ_eddsa_cardano.circom --r1cs --wasm --sym

      - name: Setup Node
        uses: actions/setup-node@v4
        with:
          node-version: '20'

      - name: Install snarkjs
        run: |
          npm i -g snarkjs@0.7.x

      - name: Prove & verify (dummy inputs)
        run: |
          if [ ! -f circ_eddsa_cardano_input.json ]; then
            echo '{}' > circ_eddsa_cardano_input.json
          fi
          node circ_eddsa_cardano_js/generate_witness.js \
            circ_eddsa_cardano_js/circ_eddsa_cardano.wasm \
            circ_eddsa_cardano_input.json \
            circ_eddsa_cardano.wtns || true # allow failure until real vectors exist

          snarkjs wtns export json circ_eddsa_cardano.wtns circ_eddsa_cardano_public.json || true
          snarkjs powersoftau new bn128 19 powersoftau.ptau
          snarkjs powersoftau prepare phase2 powersoftau.ptau pot_final.ptau
          snarkjs groth16 setup circ_eddsa_cardano.r1cs pot_final.ptau circ_eddsa_cardano.zkey
          snarkjs zkey export verificationkey circ_eddsa_cardano.zkey circ_eddsa_cardano_verifkey.json

          # These will pass once valid inputs are provided in repo
          snarkjs groth16 prove circ_eddsa_cardano.zkey circ_eddsa_cardano.wtns circ_eddsa_cardano_proof.json circ_eddsa_cardano_public.json || true
          snarkjs groth16 verify circ_eddsa_cardano_verifkey.json circ_eddsa_cardano_public.json circ_eddsa_cardano_proof.json || true
```

> Replace the dummy `{}` input with a real `circ_eddsa_cardano_input.json` or generate it in CI once the Python helper is wired.

---

## Appendix E) Suggested project clean‑up

* Move legacy Schnorr code into `wbps_circuit/legacy/` (already partially done).
* Keep only **one** blessed EdDSA circuit (`circ_eddsa_cardano.circom`) in the main flow.
* Add a `vectors/` folder with at least one valid input + expected public outputs.
* Gate PRs with the CI above.

---


circom circ_eddsa_cardano.circom --r1cs --wasm --sym

// create circ_eddsa_cardano_input.json by hand

node circ_eddsa_cardano_js/generate_witness.js circ_eddsa_cardano_js/circ_eddsa_cardano.wasm circ_eddsa_cardano_input.json circ_eddsa_cardano.wtns

snarkjs wtns export json circ_eddsa_cardano.wtns circ_eddsa_cardano_public.json

snarkjs powersoftau new bn128 19 powersoftau.ptau

snarkjs powersoftau prepare phase2 powersoftau.ptau pot_final.ptau

snarkjs groth16 setup circ_eddsa_cardano.r1cs pot_final.ptau circ_eddsa_cardano.zkey

snarkjs groth16 prove circ_eddsa_cardano.zkey circ_eddsa_cardano.wtns circ_eddsa_cardano_proof.json circ_eddsa_cardano_public.json

snarkjs zkey export verificationkey circ_eddsa_cardano.zkey circ_eddsa_cardano_verifkey.json

snarkjs groth16 verify circ_eddsa_cardano_verifkey.json circ_eddsa_cardano_public.json circ_eddsa_cardano_proof.json 
