<h1 align="center">Cavefish</h1>
<h3 align="center">Communication-Optimal Light Client Protocol for UTxO Ledgers</h3>

Cavefish is a **light client protocol** for UTxO-based blockchains like Cardano and Bitcoin.  
It enables resource-constrained clients to construct and submit transactions **without downloading or parsing the full ledger**,  
using a simple two-party protocol between a **Light Client (LC)** and a **Service Provider (SP)**.


## What’s in this repository?

This repository hosts both the **academic specification** of Cavefish and the **prototype implementation work**.

- 📂 [`paper/`](./paper/) — LaTeX sources of the Cavefish paper.  
- 📂 [`publication/`](./publication/) — timestamped PDF builds of the paper.  
- 📂 [`prototype/`](./prototype/) — prototype code and circuits used to validate the protocol.  
  - 🧩 [`wbps_circuit/`](./prototype/wbps_circuit/) — Circom circuits & Python helpers for WBPS (EdDSA Cardano variant).  
  - 🔑 [`hashing_circuits/`](./prototype/hashing_circuits/) — cryptographic hash gadgets (Poseidon, Blake2, Keccak, etc).  
  - 📦 [`txhash/`](./prototype/txhash/) — experiments with transaction hashing circuits.  


## How to Build the Paper

If you’d like to compile the paper yourself, please see the [README inside `paper/`](./paper/README.md).  
It contains full build instructions and explains the folder layout.


## How to Run the Prototype

The main entrypoint is [`prototype/wbps_circuit/`](./prototype/wbps_circuit/).  
It contains Circom circuits and Python utilities for the **Weakly Blind Predicate Signatures (WBPS)** primitive that Cavefish relies on.

Recommended workflow:

```bash
git clone --recurse-submodules <repo-url>
cd cavefish/prototype/wbps_circuit
make    # compile → witness → setup → prove → verify
