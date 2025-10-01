# ======================================================================
# Cavefish prototype – ZK workflow for circ_eddsa_cardano (Circom + snarkjs)
# ======================================================================
# See README in repo root for usage.
# ======================================================================

SHELL := bash
.ONESHELL:

# --- Configuration ------------------------------------------------------

CIRCUIT   ?= circ_eddsa_cardano
PTAU_BITS ?= 19
CURVE     ?= bn128
INCLUDES  := -l ../circomlib -l ../hashing_circuits -l .
CIRCOMLIB_TAG ?= v1.0.6

# All build artifacts go under BUILD_DIR/<CIRCUIT>/
BUILD_DIR ?= build
ARTIFACT  := $(BUILD_DIR)/$(CIRCUIT)
# Keys (trusted setup) go here
KEYDIR    := $(BUILD_DIR)/keys

# Outputs (r1cs/wasm/sym/wtns/json/proof/keys) live under $(ARTIFACT)/
R1CS   := $(ARTIFACT)/$(CIRCUIT).r1cs
WASM   := $(ARTIFACT)/$(CIRCUIT)_js/$(CIRCUIT).wasm
SYM    := $(ARTIFACT)/$(CIRCUIT).sym
WIT    := $(ARTIFACT)/$(CIRCUIT).wtns
INPUT  ?= examples/$(CIRCUIT)_input.json
PUB    := $(ARTIFACT)/$(CIRCUIT)_public.json
PROOF  := $(ARTIFACT)/$(CIRCUIT)_proof.json
ZKEY   := $(ARTIFACT)/$(CIRCUIT).zkey
VK     := $(ARTIFACT)/$(CIRCUIT)_verifkey.json

# Ceremony files kept inside KEYDIR/
PTAU1  := $(KEYDIR)/powersoftau.ptau
PTAU2  := $(KEYDIR)/pot_final.ptau

# Input generator script
PYGEN := ./$(CIRCUIT).py

# --- Phony targets ------------------------------------------------------

.PHONY: all help compile witness setup prove verify info size-hint clean distclean clobber \
        deps deps-circomlib deps-hashing

all: compile witness setup prove verify

help:
	@echo "Cavefish ZK Makefile"
	@echo
	@echo "Usage: make [target] [CIRCUIT=name] [PTAU_BITS=N] [CURVE=bn128] [BUILD_DIR=build]"
	@echo
	@echo "Targets:"
	@echo "  all         Run compile → witness → setup → prove → verify"
	@echo "  compile     Compile $(CIRCUIT).circom to R1CS/WASM/SYM (under $(ARTIFACT))"
	@echo "  witness     Compute witness + export public signals (under $(ARTIFACT))"
	@echo "  setup       Run PoT (phase1/2), Groth16 setup, export VK (under $(BUILD_DIR))"
	@echo "  prove       Produce Groth16 proof JSON (under $(ARTIFACT))"
	@echo "  verify      Verify proof with VK and public signals"
	@echo "  info        Print constraint count and circuit stats"
	@echo "  size-hint   Suggest PTAU_BITS based on constraints"
	@echo "  clean       Remove build artifacts (keeps *.ptau, *.zkey)"
	@echo "  distclean   Remove ALL artifacts including *.ptau/*.zkey"
	@echo
	@echo "Variables:"
	@echo "  CIRCUIT=$(CIRCUIT)   PTAU_BITS=$(PTAU_BITS)   CURVE=$(CURVE)   CIRCOMLIB_TAG=$(CIRCOMLIB_TAG)"
	@echo "  BUILD_DIR=$(BUILD_DIR)  INPUT=$(INPUT)"

# --- First-run bootstrap ------------------------------------------------

deps: deps-circomlib deps-hashing

../circomlib/circuits/poseidon.circom:
	@echo "📥 [Deps] Cloning circomlib $(CIRCOMLIB_TAG)"
	if [ ! -d "../circomlib/.git" ]; then \
	  git clone https://github.com/iden3/circomlib.git ../circomlib; \
	  cd ../circomlib && git checkout $(CIRCOMLIB_TAG); \
	else \
	  echo "ℹ️  circomlib already present at ../circomlib"; \
	fi
	@test -f ../circomlib/circuits/poseidon.circom || { \
	  echo "❌ poseidon.circom not found in circomlib"; \
	  exit 1; \
	}

deps-circomlib: ../circomlib/circuits/poseidon.circom

../hashing_circuits:
	@echo "📂 [Deps] Creating ../hashing_circuits/"
	mkdir -p ../hashing_circuits

deps-hashing: ../hashing_circuits

# --- Build steps --------------------------------------------------------

# Ensure artifact dirs exist
$(ARTIFACT):
	@mkdir -p $(ARTIFACT)

$(KEYDIR):
	@mkdir -p $(KEYDIR)

compile: deps $(R1CS) $(WASM) $(SYM)

$(R1CS) $(WASM) $(SYM): $(CIRCUIT).circom | $(ARTIFACT)
	@echo "🔧 [Compile] Building constraint system + WASM for $(CIRCUIT).circom → $(ARTIFACT)/"
	circom $(CIRCUIT).circom --r1cs --wasm --sym $(INCLUDES) -o $(ARTIFACT)

witness: compile $(WIT) $(PUB)

$(INPUT): $(PYGEN)
	@set -euo pipefail
	@abs_in="$(abspath $@)"; \
	abs_dir="$$(dirname "$$abs_in")"; \
	echo "📝 [Init] Generating $$abs_in via $(PYGEN)"; \
	mkdir -p "$$abs_dir"; \
	python3 "$(abspath $(PYGEN))" "$$abs_in"; \
	echo "✅  Wrote $$abs_in"

$(WIT): $(WASM) $(INPUT) | $(ARTIFACT)
	@set -euo pipefail
	@[ -f "$(INPUT)" ] || { \
	  echo "❌ Missing input file: $(INPUT)"; \
	  exit 1; \
	}
	@echo "🧮 [Witness] Computing witness from $(INPUT) → $(WIT)"
	snarkjs wtns calculate $(WASM) $(INPUT) $(WIT)

$(PUB): $(WIT) | $(ARTIFACT)
	@set -euo pipefail
	@echo "📤 [Witness] Exporting public signals → $(PUB)"
	snarkjs wtns export json $(WIT) $(PUB)

setup: $(VK)

$(PTAU1): | $(KEYDIR)
	@set -euo pipefail
	@echo "🌍 [Setup/Phase1] Powers of Tau (curve=$(CURVE), size=2^$(PTAU_BITS)) → $(PTAU1)"
	snarkjs powersoftau new $(CURVE) $(PTAU_BITS) $(PTAU1)

$(PTAU2): $(PTAU1) | $(KEYDIR)
	@set -euo pipefail
	@echo "🔑 [Setup/Phase2] Specializing universal PoT → $(PTAU2)"
	snarkjs powersoftau prepare phase2 $(PTAU1) $(PTAU2)

$(ZKEY): $(R1CS) $(PTAU2) | $(ARTIFACT)
	@set -euo pipefail
	@echo "📦 [Setup/Groth16] Creating proving key for $(CIRCUIT) → $(ZKEY)"
	snarkjs groth16 setup $(R1CS) $(PTAU2) $(ZKEY)

$(VK): $(ZKEY) | $(ARTIFACT)
	@set -euo pipefail
	@echo "🔍 [Setup/Groth16] Exporting verification key → $(VK)"
	snarkjs zkey export verificationkey $(ZKEY) $(VK)

prove: $(PROOF)

$(PROOF): $(ZKEY) $(WIT) $(PUB) | $(ARTIFACT)
	@set -euo pipefail
	@echo "🛡️  [Prove] Generating Groth16 proof → $(PROOF)"
	snarkjs groth16 prove $(ZKEY) $(WIT) $(PROOF) $(PUB)

verify: $(VK) $(PUB) $(PROOF)
	@set -euo pipefail
	@echo "✅ [Verify] Checking proof with verification key"
	snarkjs groth16 verify $(VK) $(PUB) $(PROOF)

# --- Utilities ----------------------------------------------------------

info: $(R1CS)
	@echo "ℹ️  [Info] Circuit stats:"
	snarkjs r1cs info $(R1CS) || true

size-hint: $(R1CS)
	@set -euo pipefail
	@echo "📏 [Hint] PTAU sizing suggestion"
	N=$$(snarkjs r1cs info $(R1CS) 2>/dev/null | awk '/# of Constraints/{print $$NF}'); \
	if [ -z "$$N" ]; then echo "Could not read constraint count."; exit 0; fi; \
	REQ=$$(python3 - "$$N" <<'PY'\n\
import math,sys\n\
n=int(sys.argv[1])\n\
bits = math.ceil(math.log2(max(n,1)))+1\n\
print(bits)\n\
PY\n\
); \
	echo "Constraints: $$N"; \
	echo "Suggested PTAU_BITS: $$REQ (current: $(PTAU_BITS))"

# --- Cleanup ------------------------------------------------------------

.DELETE_ON_ERROR:

.PHONY: clean distclean clobber

clean:
	@echo ">> clean: removing build outputs"
	$(RM) -r $(BUILD_DIR)

distclean: clean
	@echo ">> distclean: removing zk artifacts"
	$(RM) -f $(ARTIFACT)/*.zkey $(ARTIFACT)/*.wtns $(ARTIFACT)/*.vkey \
	        $(ARTIFACT)/*proof.json $(ARTIFACT)/*public.json \
	        $(ARTIFACT)/*verifier.sol
	@find $(BUILD_DIR) -type f \( -name '*.zkey' -o -name '*.wtns' -o -name '*.vkey' \
	    -o -name 'proof.json' -o -name 'public.json' -o -name 'verifier.sol' \) -delete || true

clobber:
	@if [ "$$ALLOW_CLOBBER" != "1" ]; then \
	  echo "Refusing to delete *.ptau/*.tau without ALLOW_CLOBBER=1"; exit 2; \
	fi
	@echo ">> clobber: removing trusted setup files"
	$(RM) -f $(KEYDIR)/*.ptau $(KEYDIR)/*.tau
	$(RM) -f ./*.ptau ./*.tau

.PRECIOUS: $(R1CS) $(WASM) $(WIT) $(PTAU1) $(PTAU2) $(ZKEY)
