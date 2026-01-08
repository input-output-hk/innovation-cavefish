#!/usr/bin/env python3
"""
generate_input_json.py

Usage:
    python3 generate_input_json.py <name> <input_len>

Example:
    python3 generate_input_json.py main_blake 128
"""

import sys
import json
import random
from pathlib import Path

BN254_MOD = 21888242871839275222246405745257275088696311157297823662689037894645226208583

def generate_input(instance, config):

    if instance == "blake2b":
        inputs = [random.randint(0, 255) for _ in range(int(config[0]))]
        data = {"in": inputs}
    elif instance == "sha256_bytes":
        inputs = [random.randint(0, 255) for _ in range(int(config[0]))]
        data = {"in": inputs}
    elif instance == "sha512_bytes":
        inputs = [random.randint(0, 255) for _ in range(int(config[0]))]
        data = {"in": inputs}
    elif instance == "sha256_bits":
        inputs = [random.randint(0, 1) for _ in range(int(config[0]))]
        data = {"in": inputs}
    elif instance == "poseidon2_rate1":
        inputs = [random.randint(0, BN254_MOD) for _ in range(int(config[0]))]
        data = {"inp": inputs}
    elif instance == "scalarmulany":
        scalar_rho = 2389324445
        ek_point_P = [3421554635436, 8764750980]
        data = {"point": ek_point_P, "scalar": scalar_rho}
    elif instance == "scalarmulfix":
        scalar_rho = 2389324445
        data = {"scalar": scalar_rho}
    elif instance == "blake2bvarlen_quad" or instance == "blake2bvarlen_linear":
        n_blocks = int(config[0])
        in_bytes = [random.randint(0, 255) for _ in range(n_blocks * 128)]
        in_sel = [0 for _ in range(n_blocks)]
        in_sel[0] = 1 # Hardcoded: always select hash 1
        data = {"in": in_bytes, "sel": in_sel}
    elif instance == "blake2bcompress":
        h = [random.randint(0, 255) for _ in range(8)]
        m = [random.randint(0, 255) for _ in range(16)]
        data = {"h": h, "m": m}
    elif instance == "poseidon_prf_chained" or instance == "poseidon_prf_oneshot":
        seed = [random.randint(0, BN254_MOD), random.randint(0, BN254_MOD)]
        data = {"in_seed_x": seed[0], "in_seed_y": seed[1]}
    elif instance == "bytestrcomp":
        inputs = [random.randint(0, 255) for _ in range(int(config[0]))]
        data = {"a": inputs, "b": inputs}
    else:
        print(f"[Generate inputs] Invalid circuit '{instance}'.", file=sys.stderr)
        sys.exit(1)
    
    return data