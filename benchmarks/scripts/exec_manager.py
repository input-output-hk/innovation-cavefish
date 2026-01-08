import json
from pathlib import Path
import wget
import subprocess
import time

CEREMONY_PATH = Path("ceremonies")

# Ceremony manager
# - generates or downloads the corresponding phase 1 crs
def __read_setup_size(path):
    key = "setup_size"
    with path.open("r", encoding="utf-8") as f:
        data = json.load(f)
    if key not in data:
        raise KeyError(f"Key '{key}' not foun in {path}")
    return data[key]

def download_ceremony(path):
    size = __read_setup_size(path)
    # Update to url format
    if size < 8:
        size = "08"
    if size == 8:
        size = "08"
    if size == 9:
        size = "09"

    if (CEREMONY_PATH / f"powersOfTau28_hez_final_{size}.ptau").is_file():
        print(f"Ceremony file of size {size} already exists")
    else:
        print(f"Downloading ceremony of size 2^{size}...")
        url = f"https://storage.googleapis.com/zkevm/ptau/powersOfTau28_hez_final_{size}.ptau"
        filename = wget.download(url, out=str(CEREMONY_PATH))

def generate_ceremony():
    # TODO: implement
    print("generate...")

# Groth16 setup
# - generates the keys (setup phase 2)
def specific_setup(instance_path, index):
    size = __read_setup_size(instance_path / "info.json")
    # Update to url format
    if size < 8:
        size = "08"
    if size == 8:
        size = "08"
    if size == 9:
        size = "09"
        
    ceremony_path = CEREMONY_PATH / f"powersOfTau28_hez_final_{size}.ptau"
    r1cs_path = instance_path / "main.r1cs"
    empty_key_path = instance_path / "empty_key.zkey"
    proving_key_path = instance_path / "proving_key.zkey"
    verification_key_path = instance_path / "verification_key.json"

    print(f"Running groth16 circuit-specific setup ({index}) for instance: {instance_path}")

    st_setup = time.time()
    # snarkjs groth16 setup
    cmd = ["snarkjs", "groth16", "setup", str(r1cs_path), str(ceremony_path), str(empty_key_path)]
    subprocess.run(cmd)

    # snarkjs zkey contribute
    cmd = ["snarkjs", "zkey", "contribute", str(empty_key_path), str(proving_key_path), "--name=\"first\"", "-e=\"random entropy\""]
    subprocess.run(cmd)

    # snarkjs zkey export verificationkey
    cmd = ["snarkjs", "zkey", "export", "verificationkey", str(proving_key_path), str(verification_key_path)]
    subprocess.run(cmd)
    et_setup = time.time()

    return (et_setup - st_setup)

# Groth16 prover
# - runs the prover
def prover(instance_path, index):

    if not __there_is_proving_key(instance_path):
        print(f"No specific setup has been executed in path {instance_path}")
        print(f"Executing setup once...")
        specific_setup(instance_path, "-")

    print(f"Running groth16 prover ({index}) for instance: {instance_path}")

    proving_key_path = instance_path / "proving_key.zkey"
    witness_path = instance_path / "witness.wtns"
    proof_path = instance_path / "proof.json"
    pub_input_path = instance_path / "public.json"

    cmd = ["snarkjs", "groth16", "prove", str(proving_key_path), str(witness_path), str(proof_path), str(pub_input_path)]

    st_prover = time.time()
    subprocess.run(cmd)
    et_prover = time.time()
    return (et_prover - st_prover)

# Groth16 verifier
# - runs the verifier
def verifier(instance_path, index):

    if not __there_is_proof(instance_path):
        print(f"No prover has been executed in path {instance_path}")
        print(f"Executing prover once...")
        prover(instance_path, "-")

    print(f"Running groth16 verifier ({index}) for instance: {instance_path}")

    verification_key_path = instance_path / "verification_key.json"
    proof_path = instance_path / "proof.json"
    pub_input_path = instance_path / "public.json"

    cmd = ["snarkjs", "groth16", "verify", str(verification_key_path), str(pub_input_path), str(proof_path)]
    
    st_verifier = time.time()
    subprocess.run(cmd)
    et_verifier = time.time()
    return (et_verifier - st_verifier)

# Helper functions
def __there_is_proof(path):
    proof_path = path / "proof.json"
    return proof_path.is_file()

def __there_is_proving_key(path):
    proving_key_path = path / "proving_key.zkey"
    return proving_key_path.is_file()