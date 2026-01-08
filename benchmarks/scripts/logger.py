import time
import numpy
import json
from pathlib import Path

class Logger:
    measurements = {}

    def __init__(self):
        self.measurements["setup"] = []
        self.measurements["prover"] = []
        self.measurements["verifier"] = []

    def time_append(self, label, time):
        if label in ["setup", "prover", "verifier"]:
            self.measurements[label].append(time)
        else:
            exit("Error: incorrect label for logger time_append")

    def get_stats(self, label="all"):
        if (label == "setup") and not (not self.measurements["setup"]):
            times = self.measurements["setup"]
            return (numpy.average(times), numpy.std(times))
        elif (label == "prover") and not (not self.measurements["prover"]):
            times = self.measurements["prover"]
            return (numpy.average(times), numpy.std(times))
        elif (label == "verifier") and not (not self.measurements["verifier"]):
            times = self.measurements["verifier"]
            return (numpy.average(times), numpy.std(times))
        elif (label == "all"):
            setup_times = self.measurements["setup"]
            prover_times = self.measurements["prover"]
            verifier_times = self.measurements["verifier"]
            results = {}
            results["setup"] = (0, 0) if not setup_times else (numpy.average(setup_times), numpy.std(setup_times))
            results["prover"] = (0, 0) if not prover_times else (numpy.average(prover_times), numpy.std(prover_times))
            results["verifier"] = (0, 0) if not verifier_times else (numpy.average(verifier_times), numpy.std(verifier_times))
            return results
    
    # TODO: edit write_plain to add values without rewriting
    def write_plain(self, path):
        file_path = path / "log_plain.json"
        #with file_path.open("w") as f:
        #    json.dump(self.measurements, f, indent=2)

        # Read data or initialize
        if file_path.exists():
            with file_path.open("r", encoding="utf-8") as fh:
                data = json.load(fh)
        else:
            data = {"setup": [], "prover": [], "verifier": []}

        # Append new measurements
        for key in ("setup", "prover", "verifier"):
            if key not in self.measurements:
                raise KeyError(f"Missing key '{key}' in measurements")
            data[key].extend(self.measurements[key])

        # Overwrite file
        with file_path.open("w", encoding="utf-8") as fh:
            json.dump(data, fh, indent=2)

    def write_stats(self, path):
        file_path = path / "log_stats.json"
        with file_path.open("w") as f:
            json.dump(self.get_stats(), f, indent=2)

    # Method to write the contents to a file
    # -- e.g., write_plain(path_to_file)
    # -- e.g., write_stats(path_to_file)