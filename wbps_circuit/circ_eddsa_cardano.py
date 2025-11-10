import json
import random

# Sample content for the specified fields
data = {
    "A": [random.randint(0, 1) for _ in range(256)],
    "R8": [random.randint(0, 1) for _ in range(256)],
    "ek": ["3421554635436", "8764750980"],
    "C0": ["6542455432", "1109876543"],
    "Cmsg": [random.randint(0, 250023232) for _ in range(9)],
    "c_schnorr": [random.randint(0, 250023232) for _ in range(64)],
    "msg_pub": [random.randint(0, 1) for _ in range(254*9)],
    "rho": "2389324445",
    "msg_priv": [random.randint(0, 1) for _ in range(333)]
}

# Write the data to input.json
with open("circ_eddsa_cardano_input.json", "w") as json_file:
    json.dump(data, json_file, indent=4)

print("circ_eddsa_cardano_input.json file has been created with the specified fields.")