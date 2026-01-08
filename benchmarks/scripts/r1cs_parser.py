#!/usr/bin/env python3
import json
import re
import sys
import subprocess
import math
from pathlib import Path

#####################################################
#   DISCLAIMER: Please note that this script        #
#       is very conditioned by the output format    #
#       provided by "snarkjs".                      #
#####################################################

ANSI_RE = re.compile(rb'\x1b\[[0-9;]*m')

def strip_ansi_bytes(b: bytes) -> str:
    """Remove ANSI escape codes from a bytes string."""
    clean = ANSI_RE.sub(b'', b)
    return clean.decode('utf-8', errors='replace')

def run_snarkjs_inspect(r1cs_path: Path) -> str:
    """
    Executes: snarkjs r1cs info <file>
    Returns stdout as a string.
    """

    cmd = ["snarkjs", "r1cs", "info", str(r1cs_path)]
    # If you prefer shorthand: cmd = ["snarkjs", "ri", str(r1cs_path)]

    try:
        result = subprocess.run(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            check=True,
        )
    except subprocess.CalledProcessError as e:
        print("ERROR: snarkjs execution failed.")
        print("Command:", " ".join(cmd))
        print("stderr:")
        print(e.stderr)
        sys.exit(1)

    return result.stdout


def parse_inspect_output(text: str) -> dict:
    results = {}

    for line in text.splitlines():
        if "snarkJS:" not in line:
            continue

        _, after = line.split("snarkJS:", 1)
        after = after.strip()

        if ":" not in after:
            continue

        key, value = after.split(":", 1)
        key = key.strip()
        value = value.strip()

        # Normalize key
        key_norm = (
            key.lower()
               .replace("# of ", "")
               .replace(" ", "_")
               .replace("-", "_")
        )

        if value.isdigit():
            value = int(value)

        results[key_norm] = value

        if key_norm == "constraints":
            # TODO: (fix) The actual value in SnarkJS is:
            # [log2(nConstraints + nPubInputs + nOutputs) + 1] < expected_power
            # otherwise outputs a compilation error
            exp = math.ceil(math.log2(value))
            results["setup_size"] = exp

    return results

def r1cs_parse(r1cs_file_path, output_file_path):

    if not r1cs_file_path.is_file():
        exit(f"Error: r1cs file {r1cs_file_path} not found")

    raw_output = run_snarkjs_inspect(r1cs_file_path).encode("utf-8")
    parsed_json = parse_inspect_output(strip_ansi_bytes(raw_output))

    output_file_path.write_text(json.dumps(parsed_json, indent=2), encoding="utf-8")
    print(f"[OK] JSON written to {output_file_path}")