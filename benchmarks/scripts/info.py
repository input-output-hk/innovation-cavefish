import argparse
from pathlib import Path
import json
import numpy

BUILD_DIR = Path("build")

def suffix_sort(path_list, prefix):
    prefix_len = len(prefix)
    return sorted(path_list, key=lambda s: str(s)[prefix_len:])

def get_instance_info(path):
    info_path = path / "info.json"
    
    with info_path.open("r", encoding="utf-8") as f:
        data = json.load(f)
    return data

def get_instance_times(path):
    samples_path = path / "log_plain.json"

    with samples_path.open("r", encoding="utf-8") as f:
        data = json.load(f)
    
    setup_results = None
    prover_results = None
    verifier_results = None

    # Compute setup stats
    samples = data["setup"]
    n_samples = len(samples)
    if n_samples == 0:
        setup_results = (n_samples, 0, 0)
    else:
        setup_results = (n_samples, numpy.average(samples), numpy.std(samples))

    # Compute prover stats
    samples = data["prover"]
    n_samples = len(samples)
    if n_samples == 0:
        prover_results = (n_samples, 0, 0)
    else:
        prover_results = (n_samples, numpy.average(samples), numpy.std(samples))

    # Compute verifier stats
    samples = data["verifier"]
    n_samples = len(samples)
    if n_samples == 0:
        verifier_results = (n_samples, 0, 0)
    else:
        verifier_results = (n_samples, numpy.average(samples), numpy.std(samples))

    # Return results
    return (setup_results, prover_results, verifier_results)

def main():
    parser = argparse.ArgumentParser(
        description="Benchmark info parser."
    )

    parser.add_argument(
        "-i", "--instance",
        required=True,
        default="all",
        help="Instance (circuit) to gather bench stats (print all by default)."
    )

    parser.add_argument(
        "-p", "--print",
        required=False,
        action="store_true",
        default=False,
        help="If true, prints all stats in the screen."
    )

    args = parser.parse_args()

    data = []

    if args.instance == "all":
        ipath_list = [f for f in BUILD_DIR.iterdir() if f.is_dir()]
        sorted_ipath_list = ipath_list.sort(key=lambda p: p.as_posix())
    else:
        # TODO: this can be refined to split instance/params and add them to json file
        ipath_list = [f for f in BUILD_DIR.iterdir() if (f.is_dir() and args.instance in str(f))]
        sorted_ipath_list = suffix_sort(ipath_list, args.instance)

    for ipath in sorted_ipath_list:
        instance = {}
        instance["instance"] = str(ipath.relative_to(BUILD_DIR))

        # Parse info
        iinfo = get_instance_info(ipath)
        instance["setup_size"] = iinfo["setup_size"]
        instance["constraints"] = iinfo["constraints"]

        # Parse run times
        setup, prover, verifier = get_instance_times(ipath)
        instance["setup"] = {
            "samples": setup[0],
            "avg": setup[1],
            "std": setup[2]
        }
        instance["prover"] = {
            "samples": prover[0],
            "avg": prover[1],
            "std": prover[2]
        }
        instance["verifier"] = {
            "samples": verifier[0],
            "avg": verifier[1],
            "std": verifier[2]
        }

        # Append all data
        data.append(instance)

        if args.print:
            print(f'''
            Instance: {instance["instance"]}
            Setup size: {instance["setup_size"]}
            Number of constraints: {instance["constraints"]}
            Setup samples: {instance["setup"]["samples"]}
            Setup average (s): {instance["setup"]["avg"]}
            Setup std dev (s): {instance["setup"]["std"]}
            Prover samples: {instance["prover"]["samples"]}
            Prover average (s): {instance["prover"]["avg"]}
            Prover std dev (s): {instance["prover"]["std"]}
            Verifier samples: {instance["verifier"]["samples"]}
            Verifier average (s): {instance["verifier"]["avg"]}
            Verifier std dev (s): {instance["verifier"]["std"]}
            --------------------------------------
            ''')

    # Output results in a file
    file_path = Path("./stats.json")
    with file_path.open("w") as f:
        json.dump(data, f, indent=2)

if __name__ == "__main__":
    main()