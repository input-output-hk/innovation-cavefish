# ======================================================================
# Cavefish prototype – ZK workflow (Circom + snarkjs)
# Structure:
#   circuits/          -> *.circom sources
#   tooling/inputgen/  -> gen_wbps_input.js (optional on-demand generator)
#   build/<name>/      -> per-circuit artifacts (incl. *_input.json)
#   build/keys/        -> trusted setup files (ptau)
#
# Default behavior:
#   - Uses example input (examples/<circuit>_input.json)
#   - Generator runs only when you call: make with-generated-input
# ======================================================================

SHELL := bash
.ONESHELL:
.DELETE_ON_ERROR:

# --- Configuration ------------------------------------------------------

CIRCUIT   ?= wbps_cardano
PTAU_BITS ?= 19
CURVE     ?= bn128

# Paths
CIRCUIT_DIR := circuits
BUILD_DIR   := build
ARTIFACT    := $(BUILD_DIR)/$(CIRCUIT)
KEYDIR      := $(BUILD_DIR)/keys

INCLUDES  := -l ../circomlib -l ../hashing_circuits -l .
CIRCOMLIB_TAG ?= v1.0.6

# --- I/O files ----------------------------------------------------------

CIRCUIT_FILE := $(CIRCUIT_DIR)/$(CIRCUIT).circom

R1CS   := $(ARTIFACT)/$(CIRCUIT).r1cs
WASM   := $(ARTIFACT)/$(CIRCUIT)_js/$(CIRCUIT).wasm
SYM    := $(ARTIFACT)/$(CIRCUIT).sym
WIT    := $(ARTIFACT)/$(CIRCUIT).wtns
INPUT  ?= $(ARTIFACT)/$(CIRCUIT)_input.json
PUB    := $(ARTIFACT)/$(CIRCUIT)_public.json
PROOF  := $(ARTIFACT)/$(CIRCUIT)_proof.json
ZKEY   := $(ARTIFACT)/$(CIRCUIT).zkey
VK     := $(ARTIFACT)/$(CIRCUIT)_verifkey.json

PTAU1  := $(KEYDIR)/powersoftau.ptau
PTAU2  := $(KEYDIR)/pot_final.ptau

# Example input (used by default)
EXAMPLE_INPUT := examples/$(CIRCUIT)_input.json

# ==== Node-based input generator (optional) =============================
GEN_DIR    := tooling/inputgen
GEN_BIN    := $(GEN_DIR)/gen_wbps_input.js
GEN_PKJ    := $(GEN_DIR)/package.json
GEN_NODEM  := $(GEN_DIR)/node_modules
GEN_DEBUG  ?= 0            # set to 1 or call `make gen-input-debug` to pass --debug
GEN_ARGS   := --out $(abspath $(INPUT)) $(if $(filter 1,$(GEN_DEBUG)),--debug,)

# --- Phony targets ------------------------------------------------------

.PHONY: all help with-example with-generated-input \
        compile witness setup prove verify info size-hint clean distclean clobber \
        deps deps-circomlib deps-hashing tree \
        use-example-input gen-input gen-input-debug public show-paths

# Default flow now uses the example input
all: with-example

help:
	@echo "ZK Makefile"
	@echo
	@echo "Default:"
	@echo "  make (or make with-example)  -> compile + example input + witness + setup + prove + verify"
	@echo
	@echo "Explicit generator:"
	@echo "  make with-generated-input     -> compile + generate input + witness + setup + prove + verify"
	@echo "  make gen-input                -> just (re)generate input file"
	@echo "  make gen-input-debug          -> same as gen-input with --debug"
	@echo
	@echo "Other targets: compile | witness | setup | prove | verify | info | size-hint | clean | distclean | clobber"
	@echo "Vars: CIRCUIT=$(CIRCUIT) PTAU_BITS=$(PTAU_BITS) CURVE=$(CURVE)"
	@echo "      INPUT=$(INPUT)"

tree:
	@printf "%s\n" "circuits/" "tooling/inputgen/" "build/<circuit>/" "build/keys/"

# -----------------------------------------------------------------------
# Entry-point flows
# -----------------------------------------------------------------------

with-example: compile use-example-input witness setup prove verify
with-generated-input: compile gen-input witness setup prove verify

# --- Deps ---------------------------------------------------------------

deps: deps-circomlib deps-hashing

../circomlib/circuits/poseidon.circom:
	@echo "📥 [Deps] Cloning circomlib $(CIRCOMLIB_TAG)"
	if [ ! -d "../circomlib/.git" ]; then \
	  git clone https://github.com/iden3/circomlib.git ../circomlib; \
	  cd ../circomlib && git checkout $(CIRCOMLIB_TAG); \
	else echo "ℹ️  circomlib present"; fi
	@test -f ../circomlib/circuits/poseidon.circom || { echo "❌ poseidon.circom missing"; exit 1; }

deps-circomlib: ../circomlib/circuits/poseidon.circom

../hashing_circuits:
	@echo "📂 [Deps] Creating ../hashing_circuits/"
	mkdir -p ../hashing_circuits

deps-hashing: ../hashing_circuits

# --- Build steps --------------------------------------------------------

$(ARTIFACT):
	@mkdir -p $(ARTIFACT)

$(KEYDIR):
	@mkdir -p $(KEYDIR)

compile: deps $(R1CS) $(WASM) $(SYM)

$(R1CS) $(WASM) $(SYM): $(CIRCUIT_FILE) | $(ARTIFACT)
	@echo "🔧 [Compile] $(CIRCUIT_FILE) → $(ARTIFACT)/"
	circom $(CIRCUIT_FILE) --r1cs --wasm --sym $(INCLUDES) -o $(ARTIFACT)

# -----------------------------------------------------------------------
# Input handling
#   Default: copy example → $(INPUT)
#   Optional: run generator to produce $(INPUT)
# -----------------------------------------------------------------------

use-example-input: $(INPUT)

$(INPUT): $(EXAMPLE_INPUT) | $(ARTIFACT)
	@cp $(EXAMPLE_INPUT) $(INPUT)
	@echo "📥 Using example input → $(INPUT)"

# Generator deps only when needed
$(GEN_NODEM): $(GEN_PKJ)
	@echo "📦 [Deps] Installing input generator deps in $(GEN_DIR)"
	cd $(GEN_DIR); \
	if [ ! -f package-lock.json ]; then npm install --no-fund --no-audit; \
	else npm ci --no-fund --no-audit; fi
	@echo "✅  deps installed"

gen-input: $(GEN_BIN) $(GEN_NODEM) | $(ARTIFACT)
	@set -euo pipefail
	@echo "📝 [Init] Generating $(abspath $(INPUT)) via $(GEN_BIN)"
	node "$(GEN_BIN)" $(GEN_ARGS)
	@echo "✅  Wrote $(abspath $(INPUT))"

gen-input-debug:
	@$(MAKE) gen-input GEN_DEBUG=1

# --- Witness & public export -------------------------------------------

witness: $(WIT) $(PUB)

# Log legend before witness generation
define WBPS_LOG_LEGEND
───────────────────────────────────────────────────────────────
🧭 WBPS CIRCUIT LOG LEGEND
───────────────────────────────────────────────────────────────
  900010 → g^ρ.x             (x-coordinate of generator^rho)
  900011 → g^ρ.y             (y-coordinate of generator^rho)

  900200 + i → per-limb diagnostics (for i-th 254-bit chunk):
      1st line → msg_i      (message limb)
      2nd line → rand_i     (Poseidon-derived random mask)
      3rd line → sum_i      (msg_i + rand_i)
      4th line → Cmsg[i]    (cipher limb from public input)
      5th line → delta_i    (Cmsg[i] - sum_i, must be 0 if valid)

  900300 + j → SHA-512 digest bytes (c_schnorr[j])
───────────────────────────────────────────────────────────────
endef
export WBPS_LOG_LEGEND

$(WIT): $(WASM) $(INPUT) | $(ARTIFACT)
	@echo "$$WBPS_LOG_LEGEND"
	@echo "🧮 [Witness] $(INPUT) → $(WIT)"
	snarkjs wtns calculate $(WASM) $(INPUT) $(WIT)

$(PUB): $(WIT) | $(ARTIFACT)
	@echo "📤 [Witness] Export public → $(PUB)"
	snarkjs wtns export json $(WIT) $(PUB)

# --- Setup / Prove / Verify --------------------------------------------

setup: $(VK)

$(PTAU1): | $(KEYDIR)
	@echo "🌍 [Setup/Phase1] curve=$(CURVE), size=2^$(PTAU_BITS) → $(PTAU1)"
	snarkjs powersoftau new $(CURVE) $(PTAU_BITS) $(PTAU1)

$(PTAU2): $(PTAU1) | $(KEYDIR)
	@echo "🔑 [Setup/Phase2] PoT phase2 → $(PTAU2)"
	snarkjs powersoftau prepare phase2 $(PTAU1) $(PTAU2)

$(ZKEY): $(R1CS) $(PTAU2) | $(ARTIFACT)
	@echo "📦 [Setup/Groth16] Proving key → $(ZKEY)"
	snarkjs groth16 setup $(R1CS) $(PTAU2) $(ZKEY)

$(VK): $(ZKEY) | $(ARTIFACT)
	@echo "🔍 [Setup/Groth16] Verification key → $(VK)"
	snarkjs zkey export verificationkey $(ZKEY) $(VK)

prove: $(PROOF)

$(PROOF): $(ZKEY) $(WIT) $(PUB) | $(ARTIFACT)
	@echo "🛡️  [Prove] Generating proof → $(PROOF)"
	snarkjs groth16 prove $(ZKEY) $(WIT) $(PROOF) $(PUB)

verify: $(VK) $(PUB) $(PROOF)
	@echo "✅ [Verify] Checking proof"
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
); echo "Constraints: $$N"; echo "Suggested PTAU_BITS: $$REQ (current: $(PTAU_BITS))"

public: $(PUB)

show-paths:
	@echo "Circuit:        $(CIRCUIT_FILE)"
	@echo "Build dir:      $(ARTIFACT)"
	@echo "WASM:           $(WASM)"
	@echo "R1CS:           $(R1CS)"
	@echo "SYM:            $(SYM)"
	@echo "Input (build):  $(INPUT)"
	@echo "Input (example):$(EXAMPLE_INPUT)"
	@echo "Witness:        $(WIT)"
	@echo "Public:         $(PUB)"
	@echo "Proof:          $(PROOF)"
	@echo "VK:             $(VK)"
	@echo "PTAU1:          $(PTAU1)"
	@echo "PTAU2:          $(PTAU2)"

# --- Cleanup ------------------------------------------------------------

clean:
	@echo ">> clean: removing build outputs for $(CIRCUIT)"
	$(RM) -r $(BUILD_DIR)/$(CIRCUIT)

distclean: clean
	@echo ">> distclean: removing zk artifacts (except *.ptau/*.tau)"
	@find $(BUILD_DIR) -type f \( -name '*.zkey' -o -name '*.wtns' -o -name '*.vkey' -o -name 'proof.json' -o -name '*_proof.json' -o -name 'public.json' -o -name '*_public.json' -o -name 'verifier.sol' \) -print -delete || true

clobber:
	@if [ "$$ALLOW_CLOBBER" != "1" ]; then echo "Refusing to delete *.ptau/*.tau without ALLOW_CLOBBER=1"; exit 2; fi
	@echo ">> clobber: removing trusted setup files"
	@find $(KEYDIR) -type f \( -name '*.ptau' -o -name '*.tau' \) -print -delete || true

.PRECIOUS: $(R1CS) $(WASM) $(WIT) $(PTAU1) $(PTAU2) $(ZKEY)