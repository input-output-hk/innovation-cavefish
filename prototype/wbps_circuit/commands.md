circom circ_eddsa_cardano.circom --r1cs --wasm --sym

// create circ_eddsa_cardano_input.json by hand

node circ_eddsa_cardano_js/generate_witness.js circ_eddsa_cardano_js/circ_eddsa_cardano.wasm circ_eddsa_cardano_input.json circ_eddsa_cardano.wtns

snarkjs wtns export json circ_eddsa_cardano.wtns circ_eddsa_cardano_public.json

snarkjs powersoftau new bn128 19 powersoftau.ptau

snarkjs powersoftau prepare phase2 powersoftau.ptau pot_final.ptau

snarkjs groth16 setup circ_eddsa_cardano.r1cs pot_final.ptau circ_eddsa_cardano.zkey

snarkjs groth16 prove circ_eddsa_cardano.zkey circ_eddsa_cardano.wtns circ_eddsa_cardano_proof.json circ_eddsa_cardano_public.json

snarkjs zkey export verificationkey circ_eddsa_cardano.zkey circ_eddsa_cardano_verifkey.json

snarkjs groth16 verify circ_eddsa_cardano_verifkey.json circ_eddsa_cardano_public.json circ_eddsa_cardano_proof.json 
