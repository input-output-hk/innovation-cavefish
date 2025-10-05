<h1 align="center">Cavefish</h1>
<h3 align="center">Communication-Optimal Light Client Protocol for UTxO Ledgers</h3>

Cavefish is a **light client protocol** for UTxO-based blockchains like Cardano and Bitcoin.  
It enables resource-constrained clients to construct and submit transactions **without downloading or parsing the full ledger**,  
using a two-party protocol between a **Light Client (LC)** and a **Service Provider (SP)**.

---

## 📘 Repository Structure

This repository includes both the **academic specification** and the **prototype implementation** of the Cavefish protocol.

```
cavefish/
├─ paper/              # LaTeX sources for the academic paper
├─ publication/        # Timestamped PDF builds of the paper
├─ wbps/               # Circom circuits, tooling, and Makefile for the WBPS prototype
│  ├─ circuits/        # Cardano WBPS (ElGamal + Schnorr binding)
│  ├─ examples/        # Example input vectors (JSON)
│  ├─ tooling/         # Input generator (Node.js, PoseidonEx-based)
│  ├─ vendor/          # circomlib + hashing circuits (auto-cloned)
│  └─ build/           # Generated artifacts (r1cs, wasm, wtns, zkey, etc.)
└─ README.md           # This document
```

The **WBPS (Weakly Blind Predicate Signatures)** component implements Cardano-style encryption and Schnorr challenge binding over the BabyJubJub curve.  
It provides the cryptographic foundation for Cavefish’s transaction validation flow.

---

## 🧩 WBPS Prototype

The [wbps/](./wbps/) folder is the active research prototype.  
It includes everything needed to compile, prove, and verify the WBPS circuit with **Circom** and **SnarkJS**.

### Quick Start

```bash
git clone --recurse-submodules <repo-url>
cd cavefish/wbps
make
```

This runs the full flow:
1. Clones **circomlib** and **hashing_circuits** into `vendor/`
2. Compiles `wbps_cardano.circom` → R1CS + WASM
3. Uses the curated input (`examples/wbps_cardano_input.json`)
4. Computes witness and public inputs
5. Runs Groth16 setup, proof generation, and verification

Output example:
```
✅ [Verify] Checking proof
snarkJS: OK!
```

### Input Modes

| Mode | Command | Description |
|------|----------|--------------|
| Example (default) | `make` | Uses `examples/wbps_cardano_input.json` |
| Generated | `make with-generated-input` | Creates new input with PoseidonEx generator |

To inspect logs while generating:
```bash
make gen-input-debug
```

---

## 📖 Paper Build Instructions

To build the academic paper yourself, see [`paper/README.md`](./paper/README.md).  
It explains how to compile LaTeX sources and generate timestamped releases in `publication/`.

---

## 🧠 Legacy Components

Older experiments (e.g., `circ_eddsa_cardano.circom`, `circ_schnorr_bitcoin.circom`) have been migrated or archived.  
The **WBPS formulation** now serves as the canonical circuit for Cavefish cryptographic validation.

---

## 🧩 Research Note

This repository is a **research artifact** maintained by Input Output Global (IOG).  
It is part of the **Cavefish Light Client** initiative under the broader Cardano cryptography R&D program.

> ⚠️ Not audited. Experimental use only.

