#!/usr/bin/env bash
set -euo pipefail

# Tiny helper to build a BabyJubJub keygen binary (using arnaucube/babyjubjub-rs)
# and print a fresh keypair as JSON. The resulting binary is copied to $DEST (or
# ./babyjubjub-keygen by default) so it can be reused without re-cloning.

DEST="${DEST:-$(pwd)/babyjubjub-keygen}"

command -v cargo >/dev/null 2>&1 || {
  echo "cargo is required to build the keygen binary" >&2
  exit 1
}

ARCH="$(uname -m)"

TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT

echo "[1/4] Cloning babyjubjub-rs into $TMPDIR"
git clone https://github.com/arnaucube/babyjubjub-rs.git "$TMPDIR/babyjubjub-rs" >/dev/null

echo "[2/4] Writing minimal Cargo project"
mkdir -p "$TMPDIR/keygen"
if [[ "$ARCH" == "aarch64" ]]; then
  BABYJUB_DEP='babyjubjub-rs = { path = "../babyjubjub-rs", default-features = false, features = ["aarch64"] }'
else
  BABYJUB_DEP='babyjubjub-rs = { path = "../babyjubjub-rs" }'
fi

cat >"$TMPDIR/keygen/Cargo.toml" <<EOF
[package]
name = "babyjubjub-keygen"
version = "0.1.0"
edition = "2021"

[dependencies]
$BABYJUB_DEP
ff = { package = "ff_ce", version = "0.11" }
num-bigint = "0.4"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
rand = "0.8"
EOF

mkdir -p "$TMPDIR/keygen/src"
cat >"$TMPDIR/keygen/src/main.rs" <<'EOF'
use babyjubjub_rs::{Fr, PrivateKey};
use ff::to_hex;
use num_bigint::BigInt;
use serde::Serialize;
use rand::rngs::OsRng;
use rand::RngCore;

#[derive(Serialize)]
struct Keypair {
    ek: [String; 2],
    dk: String,
}

fn fr_to_decimal(fr: &Fr) -> String {
    let hex = to_hex(fr);
    BigInt::parse_bytes(hex.as_bytes(), 16)
        .expect("invalid field element")
        .to_string()
}

fn main() {
    let mut rng = OsRng;
    let mut seed = [0u8; 32];
    rng.fill_bytes(&mut seed);

    let sk = PrivateKey::import(seed.to_vec()).expect("failed to create private key");
    let pk = sk.public();
    let dk = sk.scalar_key();

    let out = Keypair {
        ek: [fr_to_decimal(&pk.x), fr_to_decimal(&pk.y)],
        dk: dk.to_string(),
    };

    println!("{}", serde_json::to_string_pretty(&out).unwrap());
}
EOF

echo "[3/4] Building key generator (release)"
(
  cd "$TMPDIR/keygen"
  cargo build --release >/dev/null
)

mkdir -p "$(dirname "$DEST")"
cp "$TMPDIR/keygen/target/release/babyjubjub-keygen" "$DEST"
chmod +x "$DEST"

echo "[4/4] Binary ready at $DEST"
echo
echo "Sample keypair (fresh run):"
"$DEST"
