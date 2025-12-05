# Cavefish

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

> ⚠️ **Important Disclaimer & Acceptance of Risk**  
> This is a proof-of-concept implementation that has not undergone security auditing. This code is provided "as is" for research and educational purposes only. It has not been subjected to a formal security review or audit and may contain vulnerabilities. **Do not use this code in production systems or any environment where security is critical without conducting your own thorough security assessment.** By using this code, you acknowledge and accept all associated risks, and our company disclaims any liability for damages or losses.

## Mission

Cavefish is a communication-optimal light client protocol for UTxO-based blockchains like Cardano and Bitcoin. It enables resource-constrained clients (e.g., mobile wallets) to construct and submit transactions without downloading or parsing the full ledger, using a secure two-party protocol between a Light Client (LC) and a Service Provider (SP). The project aims to provide a trust-minimized, efficient way for users to interact with blockchains, minimizing reliance on centralized services while incorporating incentives for SPs and novel cryptographic primitives like weakly blind predicate signatures. This research artifact supports broader goals of enhancing blockchain accessibility, privacy, and decentralization in the Cardano ecosystem.

## Getting Started

To get started with Cavefish, clone the repository and explore the prototypes or build the academic paper.

1. **Clone the Repository**:
   ```bash
   git clone --recurse-submodules https://github.com/input-output-hk/cavefish.git
   cd cavefish
   ```

2. **Build and Run the WBPS Prototype** (Cryptographic Core):
   The WBPS (Weakly Blind Predicate Signatures) component implements Cardano-style encryption and Schnorr challenge binding over the BabyJubJub curve.
   ```bash
   cd wbps
   make
   ```
   This will:
   - Clone dependencies (circomlib and hashing_circuits).
   - Compile the circuit to R1CS and WASM.
   - Use example inputs to generate and verify a proof.
   
   For generated inputs:
   ```bash
   make with-generated-input
   ```
   Or debug mode:
   ```bash
   make gen-input-debug
   ```

3. **Build the Academic Paper**:
   ```bash
   cd paper
   ```
   Follow instructions in `paper/README.md` to compile LaTeX sources and generate a timestamped PDF in `publication/`.

4. **Run the Haskell SP Prototype**:
   The `cavefish-server/` directory contains a Haskell-based prototype of the Service Provider. Install Haskell (via GHC or Stack), then:
   ```bash
   cd cavefish-server
   stack build
   stack exec cavefish-server
   ```
   Refer to inline comments for configuration.

Prerequisites: Node.js (for tooling), Circom and SnarkJS (for circuits), LaTeX (for paper), Haskell/Stack (for server).

## Further Information

### Repository Structure
```
cavefish/
├─ cavefish-server/    # Haskell prototype of SP, Service Provider
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

### Features and Use Cases
- **Intents-Based Transactions**: LCs specify high-level intents (e.g., "send 10 ADA"), and SPs handle ledger queries and construction.
- **Blind Signatures**: Uses a novel weakly blind predicate signature scheme for privacy until on-chain posting.
- **Compatibility**: Works with Schnorr-based UTxO ledgers (Cardano, Bitcoin); integrates with HD wallets and payment channels.
- **Efficiency**: Minimal communication (2-3 rounds), low computation; benchmarks show practical proving/verification times.
- **Incentives**: SPs compensated via transaction fees or channels.

For detailed design, read the academic paper in `publication/`. Key trade-offs include relaxing full unlinkability for efficiency, assuming "private until posted" for transactions.

FAQ:
- **Is this production-ready?** No, it's experimental—see disclaimer.
- **What blockchains are supported?** Primarily Cardano; Bitcoin circuits are legacy but available.
- **How does it differ from Mithril?** Cavefish focuses on transaction construction/submission; complements Mithril for chain syncing.

Legacy components (e.g., older circuits like `circ_eddsa_cardano.circom`) are archived; WBPS is the canonical implementation.

## Getting Help

For questions or issues:
- Open a GitHub issue in this repository.
- Discuss on the Cardano Forum: [forum.cardano.org](https://forum.cardano.org) (search for "Cavefish").
- Join Cardano community channels on Discord or Telegram for general blockchain support.

## Getting Involved

We welcome community input! Engage via:
- GitHub discussions or issues for feature ideas, bugs, or questions.
- Cardano Improvement Proposals (CIPs) process for protocol-level suggestions.
- Follow IOG's research updates on [iohk.io/blog](https://iohk.io/en/blog/).

As part of IOG's mid-year research progress (August 2025), Cavefish is in its inception phase—share feedback to shape milestones like the upcoming CIP.

## Who We Are

Cavefish is developed by the Research & Development Innovation group at Input Output Global (IOG), the engineering team behind Cardano. Key contributors include researchers in cryptography and protocol design. Being a contributor means aligning with open-source principles, focusing on secure, peer-reviewed innovations for decentralized systems.

For more on IOG: [iohk.io](https://iohk.io/).

## Contributing

We encourage contributions! This project follows standard GitHub workflows.

### Finding the Source Code
- The main repo: [github.com/input-output-hk/cavefish](https://github.com/input-output-hk/cavefish).

### Issues to Help With
- Check the [Issues tab](https://github.com/input-output-hk/cavefish/issues) for open tasks (technical: circuits, Haskell; non-technical: docs, benchmarks).
- Labels: "good first issue" for beginners.

### Documentation Links
- Architecture: See the academic paper for protocol details.
- Coding Conventions: Follow Haskell style (brittany), Circom best practices; use descriptive commits.
- Testing: Run `make` in wbps/ for circuit tests; add unit tests for Haskell.
- Cavefish on the [IOG Blog](https://iohk.io/en/blog/posts/2025/12/03/presenting-cavefish/)

### Making Changes
1. Fork the repo and create a branch.
2. Install prerequisites: Haskell (Stack), Node.js, Circom, SnarkJS, LaTeX.
3. Build/test locally (see Getting Started).
4. For circuits: Add/modify in `wbps/circuits/`; test with `make`.
5. For server: Modify in `cavefish-server/`; use Stack for building.

### Submitting Changes
- Preferred process: Open an issue first to discuss (e.g., for major changes like new features).
- Submit a Pull Request (PR) with clear description, linking to issue.
- Review focuses on security, efficiency, and alignment with UTxO model.

Design values: Prioritize minimal trust, communication efficiency, and real-world deployability. Trade-offs resolved via peer-reviewed crypto primitives.
