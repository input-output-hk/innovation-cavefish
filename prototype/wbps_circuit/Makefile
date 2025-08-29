# Cavefish prototype – ZK workflow for circ_eddsa_cardano
CIRCUIT := circ_eddsa_cardano
PTAU_BITS := 19
INCLUDES := -l ../circomlib -l ../hashing_circuits -l .

.PHONY: all compile witness setup prove verify clean

all: compile witness setup prove verify

compile:
	circom $(CIRCUIT).circom --r1cs --wasm --sym $(INCLUDES)

witness: compile
	snarkjs wtns calculate \
		$(CIRCUIT)_js/$(CIRCUIT).wasm \
		$(CIRCUIT)_input.json \
		$(CIRCUIT).wtns
	snarkjs wtns export json $(CIRCUIT).wtns $(CIRCUIT)_public.json

setup:
	snarkjs powersoftau new bn128 $(PTAU_BITS) powersoftau.ptau
	snarkjs powersoftau prepare phase2 powersoftau.ptau pot_final.ptau
	snarkjs groth16 setup $(CIRCUIT).r1cs pot_final.ptau $(CIRCUIT).zkey
	snarkjs zkey export verificationkey $(CIRCUIT).zkey $(CIRCUIT)_verifkey.json

prove:
	snarkjs groth16 prove $(CIRCUIT).zkey $(CIRCUIT).wtns $(CIRCUIT)_proof.json $(CIRCUIT)_public.json

verify:
	snarkjs groth16 verify $(CIRCUIT)_verifkey.json $(CIRCUIT)_public.json $(CIRCUIT)_proof.json

clean:
	rm -rf $(CIRCUIT)_js *.r1cs *.sym *.wtns *.zkey *.ptau *_public.json *_proof.json *_verifkey.json
