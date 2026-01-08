from pathlib import Path
import json
import subprocess
#from gen_circom_entrypoint import circom_str_main
from gen_input import generate_input
from r1cs_parser import r1cs_parse

BUILD_DIR = Path("build")
TEMPL_DIR = Path("circuits/templ")
CIRCOMLIB_DIR = Path("circuits/core/circomlib/circuits")
CIRCOM_ENTRYPOINTS_FILE = Path("circom_entrypoints.json")

def __parse_config(config):
    if not isinstance(config, list):
        exit("Error: config must be a list of arguments (-c arg1 arg2 ...)")
    return "_".join(config)

def get_instance_dir(instance, config):
    str_config = __parse_config(config)
    path = BUILD_DIR / f"{instance}_{str_config}"
    return path

def config_exists(instance, config):
    path = get_instance_dir(instance, config)
    print(f"Checking if {path} exists...")
    return path.is_dir()

def config_remove(instance, config):
    path = get_instance_dir(instance, config)
    print(f"Removing folder {path}")
    subprocess.run(["rm", "-r", str(path)])

def config_create(instance, config):

    path = get_instance_dir(instance, config)
    print(f"Creating {path}...")
    path.mkdir(parents=True, exist_ok=False)

    # Generate config file
    __generate_json_config(path, instance, config)
    
    # Generate circom entrypoint
    circom_content = __circom_str_main(instance, config)
    if circom_content == None:
        exit("Error: Circom entrypoint could not be created")
    circom_main_file = path / "main.circom"
    with circom_main_file.open("w") as f:
        f.write(circom_content)
    print(f"Generated circom main file: {circom_main_file}")
    
    # Generate synthetic data
    __generate_input_file(path, instance, config)

    # Compile circom
    # TODO: add parametrizable circom opts (O0/O1/O2)
    print("Compiling circom...")
    cmd = ["circom", str(circom_main_file), "--r1cs", "--wasm", "--O1", "-l", str(TEMPL_DIR), "-l", str(CIRCOMLIB_DIR), "-o", str(path)]
    print(cmd)
    subprocess.run(cmd)

    # Parse circuit information
    print("Parsing circuit information...")
    circuit_r1cs_path = path / "main.r1cs"
    circuit_info_path = path / "info.json"
    r1cs_parse(circuit_r1cs_path, circuit_info_path)


    # Generate witness
    # TODO: reorganize in some function
    print("Generating witness...")
    js_exec_path = path / "main_js/generate_witness.js"
    wasm_path = path / "main_js/main.wasm"
    input_path = path / "input.json"
    witness_path = path / "witness.wtns"
    cmd = ["node", str(js_exec_path), str(wasm_path), str(input_path), str(witness_path)]
    subprocess.run(cmd)

def __generate_json_config(path, instance, config):
    data = {
        "name": instance
    }
    for index, arg in enumerate(config):
        data["arg"+str(index+1)] = arg
    str_config = __parse_config(config)
    output_file = path / f"{instance}_{str_config}.json"
    with output_file.open("w") as f:
        json.dump(data, f, indent=2)
    print(f"Generated config file: {output_file}")

def __generate_input_file(path, instance, config):
    input_dict = generate_input(instance, config)
    output_file = path / "input.json"
    with output_file.open("w") as f:
        json.dump(input_dict, f, indent=2)
    print(f"Generated input file: {output_file}")

def __circom_str_main(target, config):
    # Verify that the target circom file exists
    path = TEMPL_DIR / f"{target}_templ.circom"
    if not path.is_file():
        exit(f"The template {path} does not exist")
    # Generate the circom main content
    with CIRCOM_ENTRYPOINTS_FILE.open("r", encoding="utf-8") as f:
        av_templ = json.load(f)
    if target not in av_templ:
        exit(f"Error: specified {target} is not available in {CIRCOM_ENTRYPOINTS_FILE}")
    return av_templ[target].format(str_config=__parse_config(config))

def __parse_config(config):
    if not isinstance(config, list):
        exit("Error: config must be a list of arguments (-c arg1 arg2 ...)")
    return ", ".join(config)