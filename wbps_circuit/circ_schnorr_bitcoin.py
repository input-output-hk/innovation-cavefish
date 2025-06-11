import json
import random

# Sample content for the specified fields
data = {
    "X": [ [random.randint(0, 250023232) for _ in range(4)], [random.randint(0, 250023232) for _ in range(4)] ],
    "R": [ [random.randint(0, 250023232) for _ in range(4)], [random.randint(0, 250023232) for _ in range(4)] ],
    "ek": ["3421554635436", "8764750980"],
    "C0": ["6542455432", "1109876543"],
    "Cmsg": random.randint(0, 250023232),
    "cc": [random.randint(0, 250023232) for _ in range(4)],
    "msgToSign": random.randint(0, 250023232),
    "rho": "2389324445",
    "witness": [random.randint(0, 250023232) for _ in range(8)],
    "pred": 1
}

# Write the data to input.json
with open("circ_schnorr_bitcoin_input.json", "w") as json_file:
    json.dump(data, json_file, indent=4)

print("circ_schnorr_bitcoin_input.json file has been created with the specified fields.")
