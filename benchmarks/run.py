import subprocess

def main():
    print("Running benchmarks...")

    base_len = 128 # bytes
    algorithm = "bytestrcomp"

    for mult in range(5, 10, 5):
        set_len = base_len * mult
        cmd = [
            "python3", "scripts/circom-benchmark.py",
            "-m", "full",
            "-i", algorithm,
            "-c", str(set_len),
            "-s", "1",
            "-p", "50",
            "-v", "0"
        ]
        subprocess.run(cmd)

if __name__ == "__main__":
    main()