# WBPS Input Generator

Generates **consistent** inputs for `circ_eddsa_cardano.circom` without running the circuit.

It reproduces the circuit math:
- `C0 = g^rho` on BabyJub (same base point constants as the circuit)
- `ek^rho` → PoseidonEx-like keystream → `Cmsg[i] = limb[i] + rand[i]`
- `c_schnorr = SHA512(R_bits || A_bits || msg_bits)` with the per-byte bit flip used inside the circuit

## Quick start

```bash
# install deps once
cd tooling/inputgen
npm ci

# generate file at examples/circ_eddsa_cardano_input.json
cd ../..
node tooling/inputgen/gen_input.js --out examples/circ_eddsa_cardano_input.json
