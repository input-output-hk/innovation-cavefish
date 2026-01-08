#!/usr/bin/env python3

import argparse
from pathlib import Path
from exec_manager import download_ceremony, specific_setup, prover, verifier
from config_manager import config_exists, config_remove, config_create, get_instance_dir
from logger import Logger

#############
# CONSTANTS #
#############

BUILD_DIR = Path("build")
TEMPL_DIR = Path("circuits/templ")

########
# MAIN #
########

def main():

    parser = argparse.ArgumentParser(
        description="Benchmark runner for a single circuit with specific parameters."
    )

    parser.add_argument(
        "-m", "--mode",
        required=False,
        default="full",
        help="Mode of execution (config, exec, full = config + exec)"
    )

    parser.add_argument(
        "-i", "--instance",
        required=True,
        help="Instance (circuit) to be executed"
    )

    # TODO: rename to args?
    parser.add_argument(
        "-c", "--config",
        required=True,
        nargs="*",
        help="List of arguments to be provided as inputs to the circuit template"
    )

    parser.add_argument(
        "-s", "--setup",
        required=False,
        default=0,
        help="Number of iters for circuit-specific setup (default=0)"
    )

    parser.add_argument(
        "-p", "--prover",
        required=False,
        default=1,
        help="Number of iters for prover (default=1)"
    )

    parser.add_argument(
        "-v", "--verifier",
        required=False,
        default=1,
        help="Number of iters for verifier (default=1)"
    )

    parser.add_argument(
        "-f", "--force",
        required=False,
        action="store_true",
        help="Regenerates the instance folder even if it already exists"
    )

    args = parser.parse_args()

    # Sanity verification
    if args.mode not in ("config", "exec", "full"):
        exit("Error: invalid mode -m (config, exec, full)")
    if int(args.prover) < 0:
        exit("Error: invalid prover -p (must be >= 0)")
    if int(args.verifier) < 0:
        exit("Error: invalid verifier -v (must be >= 0)")
    if int(args.setup) < 0:
        exit("Error: invalid setup -s (must be >= 0)")

    path = get_instance_dir(args.instance, args.config)
    iters_tuple = (int(args.setup), int(args.prover), int(args.verifier))

    print(f'''
    ##########################################
    - Running benchmark instance with options:
    - Mode (-m): {args.mode}
    - Instance (-i): {args.instance}
    - Config (-c): {args.config}
    - Force (-f): {args.force}
    - Setup iters (-s): {args.setup}
    - Prover iters (-p): {args.prover}
    - Verifier iters (-v): {args.verifier}
    - Path: {path}
    ##########################################
    ''')

    # Init logger
    logger = Logger()

    # Dispatch logic
    if args.mode == "config":
        only_config(args.force, args.instance, args.config)
    elif args.mode == "exec":
        only_exec(args.instance, args.config, iters_tuple, logger)
    elif args.mode == "full":
        only_config(args.force, args.instance, args.config)
        only_exec(args.instance, args.config, iters_tuple, logger)
    else:
        raise ValueError(f"Unknown mode: {args.mode}")

    print(f"Logger stats: {logger.get_stats()}")
    logger.write_plain(path)
    logger.write_stats(path)

######################
# DISPATCH FUNCTIONS #
######################

def only_config(force, instance, config):
    print(f"### Running only_config: {instance}, {config} with force mode {force}")

    # Verify if config already exists
    ce = config_exists(instance, config)
    if ce and not force:
        exit(f"The configuration {instance}_{config} already exists")
    if ce and force:
        # Delete previous instance
        config_remove(instance, config)

    # Create a new config
    print(f"Creating new config...")
    config_create(instance, config)

    # Handle phase 1 (universal) setup
    path = get_instance_dir(instance, config)
    size_path = path / "info.json"
    # TODO: handle the option to select download/generate ceremony
    download_ceremony(size_path)

    # Handle phase 2 (circuit-specific setup)
    specific_setup(path, "-")

def only_exec(instance, config, iters_tuple, logger):
    iters_setup = iters_tuple[0]
    iters_prover = iters_tuple[1]
    iters_verifier = iters_tuple[2]
    print(f"### Running only_exec: {instance}, {config}, {iters_tuple}")

    time = 0
    
    # Verify if config already exists
    ce = config_exists(instance, config)
    if not ce:
        exit(f"The configuration {instance}_{config} does not exist")
    
    path = get_instance_dir(instance, config)

    # Handle the setup procedure
    if iters_setup > 0:
        for index in range(1, iters_setup + 1):
            # Phase 2 (circuit-specific setup) is executed "iters_setup" times
            time = specific_setup(path, index)
            logger.time_append("setup", time)

    # Test correct verification
    prover(path, "-")
    verifier(path, "-")

    # Benchmark the prover "iters_prover" times
    for index in range(1, int(iters_prover) + 1):
        time = prover(path, index)
        logger.time_append("prover", time)

    # Benchmark the verifier "iters_verifier" times
    for index in range(1, int(iters_verifier) + 1):
        time = verifier(path, index)
        logger.time_append("verifier", time)

#############
# MAIN EXEC #
#############

if __name__ == "__main__":
    main()
