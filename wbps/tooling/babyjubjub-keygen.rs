#!/usr/bin/env rust-script
//! ```cargo
//! [dependencies]
//! babyjubjub-rs = "0.0.11"
//! ff_ce = "0.11"
//! num-bigint = "0.4"
//! serde = { version = "1.0", features = ["derive"] }
//! serde_json = "1.0"
//! rand = "0.8"
//! ```

use babyjubjub_rs::{Fr, PrivateKey};
use ff_ce::to_hex;
use num_bigint::BigInt;
use serde::Serialize;
use rand::rngs::OsRng;
use rand::RngCore;
// use serde_json ;

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
