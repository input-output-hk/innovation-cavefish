# ======================================================================
# WBPS — Circom + SnarkJS workflow (example-or-generate input)
# - Vendorizes circomlib (shallow clone) under ./vendor
# - Default uses examples/<circuit>_input.json if present, else generates
# - Prints a LOG PREFACE right before witness to explain circuit logs
# ======================================================================

SHELL := bash
.ONESHELL:
.DELETE_ON_ERROR:

# ----------------------------------------------------------------------
# Config
# ----------------------------------------------------------------------
CIRCUIT   ?= wbps_cardano
PTAU_BITS ?= 19
CURVE     ?= bn128

# ----------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------
CIRCUIT_DIR := circuits
BUILD_DIR   := build
ARTIFACT    := $(BUILD_DIR)/$(CIRCUIT)
KEYDIR      := $(BUILD_DIR)/keys
VENDOR_DIR  := vendor

INCLUDES  := -l $(VENDOR_DIR)/circomlib -l $(VENDOR_DIR)/hashing_circuits -l .

# ----------------------------------------------------------------------
# Files
# ----------------------------------------------------------------------
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

# Example input (preferred if present)
EXAMPLE_INPUT := examples/$(CIRCUIT)_input.json

# ----------------------------------------------------------------------
# Node input generator (optional, used as fallback)
# ----------------------------------------------------------------------
GEN_DIR    := tooling/inputgen
GEN_BIN    := $(GEN_DIR)/gen_wbps_input.js
GEN_PKJ    := $(GEN_DIR)/package.json
GEN_NODEM  := $(GEN_DIR)/node_modules
GEN_DEBUG  ?= 0
GEN_ARGS   := --out $(abspath $(INPUT)) $(if $(filter 1,$(GEN_DEBUG)),--debug,)

# ----------------------------------------------------------------------
# Phonies
# ----------------------------------------------------------------------
.PHONY: all help deps deps-circomlib deps-hashing \
        compile witness setup prove verify info size-hint \
        clean distclean clobber \
        use-example-or-generate gen-input gen-input-debug \
        with-example with-generated-input tree show-paths

# ----------------------------------------------------------------------
# Entrypoints
# ----------------------------------------------------------------------
all: with-example

help:
	@echo "WBPS Circom/SnarkJS workflow"
	@echo "  make (with-example)      → compile + example-or-generate + witness + setup + prove + verify"
	@echo "  make with-generated-input → compile + generator + witness + setup + prove + verify"
	@echo "  make gen-input[-debug]    → only regenerate input"
	@echo "  make info | size-hint | clean | distclean | clobber"

tree:
	@printf "%s\n" "circuits/" "tooling/inputgen/" "build/<circuit>/" "build/keys/" "vendor/"

# ----------------------------------------------------------------------
# Deps
# ----------------------------------------------------------------------
deps: deps-circomlib deps-hashing

$(VENDOR_DIR):
	mkdir -p $(VENDOR_DIR)

# circomlib via shallow clone (depth=1)
$(VENDOR_DIR)/circomlib/circuits/poseidon.circom: | $(VENDOR_DIR)
	@echo "📥 [Deps] Cloning circomlib (shallow)"
	rm -rf $(VENDOR_DIR)/circomlib
	cd $(VENDOR_DIR) && git clone --depth=1 https://github.com/iden3/circomlib.git circomlib
	@test -f $(VENDOR_DIR)/circomlib/circuits/poseidon.circom || { echo "❌ poseidon.circom missing"; exit 1; }

deps-circomlib: $(VENDOR_DIR)/circomlib/circuits/poseidon.circom

$(VENDOR_DIR)/hashing_circuits: | $(VENDOR_DIR)
	mkdir -p $(VENDOR_DIR)/hashing_circuits
	@echo "📂 [Deps] Ensure hashing_circuits (place your sha512 bits impl here)"

deps-hashing: $(VENDOR_DIR)/hashing_circuits

# ----------------------------------------------------------------------
# Build / Compile
# ----------------------------------------------------------------------
$(ARTIFACT):
	mkdir -p $(ARTIFACT)

compile: deps $(ARTIFACT) $(R1CS) $(WASM) $(SYM)

$(R1CS) $(WASM) $(SYM): $(CIRCUIT_FILE) | $(ARTIFACT)
	@echo "🔧 [Compile] $(CIRCUIT_FILE) → $(ARTIFACT)/"
	circom $(CIRCUIT_FILE) --r1cs --wasm --sym $(INCLUDES) -o $(ARTIFACT)

# ----------------------------------------------------------------------
# Input: copy example if exists, otherwise generate
# ----------------------------------------------------------------------
with-example: compile use-example-or-generate witness setup prove verify
with-generated-input: compile gen-input witness setup prove verify

use-example-or-generate: | $(ARTIFACT)
	@echo "📥 Preparing input → $(INPUT)"
	@if [ -f "$(EXAMPLE_INPUT)" ]; then \
	  cp "$(EXAMPLE_INPUT)" "$(INPUT)"; \
	  echo "📥 Using example input → $(INPUT)"; \
	else \
	  echo "ℹ️ No example input at $(EXAMPLE_INPUT). Generating one..."; \
	  $(MAKE) gen-input GEN_DEBUG=$(GEN_DEBUG); \
	fi

# Generator deps only when needed
$(GEN_NODEM): $(GEN_PKJ)
	cd $(GEN_DIR); \
	if [ ! -f package-lock.json ]; then npm install --no-fund --no-audit; \
	else npm ci --no-fund --no-audit; fi
	@echo "✅ generator deps installed"

gen-input: $(GEN_BIN) $(GEN_NODEM) | $(ARTIFACT)
	@echo "📝 [Init] Generating $(abspath $(INPUT)) via $(GEN_BIN)"
	node "$(GEN_BIN)" $(GEN_ARGS)
	@echo "✅ Wrote $(abspath $(INPUT))"

gen-input-debug:
	@$(MAKE) gen-input GEN_DEBUG=1

# ----------------------------------------------------------------------
# Witness / Setup / Prove / Verify
# ----------------------------------------------------------------------
witness: $(WIT) $(PUB)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# LOG PREFACE (prints once per run, just before witness calculation)
define WBPS_LOG_PREFACE
──────────────────────── WBPS CIRCUIT LOGS ────────────────────────
These numeric tags/lines are printed by the circuit during witness:
  900010 → g^rho.x
  900011 → g^rho.y

For each ciphertext limb i (i=0..8), a block tagged 900200+i:
  line 1: (tag)            900200+i
  line 2: msg_limb[i]      (unpacked 254-bit message limb)
  line 3: rand[i]          (Poseidon-derived OTP for this limb)
  line 4: sum_i            = msg_limb[i] + rand[i]   (mod p)
  line 5: Cmsg[i]          (provided ciphertext limb)
  line 6: delta            = Cmsg[i] − sum_i   (should be 0)

Then 64 lines for SHA-512 digest bytes:
  900300..900363 → each line prints the corresponding digest byte.
────────────────────────────────────────────────────────────────────
endef
export WBPS_LOG_PREFACE
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

$(WIT): $(WASM) $(INPUT) | $(ARTIFACT)
	@echo "$$WBPS_LOG_PREFACE"
	@echo "🧮 [Witness] $(INPUT) → $(WIT)"
	snarkjs wtns calculate $(WASM) $(INPUT) $(WIT)

$(PUB): $(WIT) | $(ARTIFACT)
	@echo "📤 [Witness] Export public → $(PUB)"
	snarkjs wtns export json $(WIT) $(PUB)

setup: $(VK)

$(KEYDIR):
	mkdir -p $(KEYDIR)

$(PTAU1): | $(KEYDIR)
	@echo "🌍 [Setup/Phase1] curve=$(CURVE), size=2^$(PTAU_BITS)"
	snarkjs powersoftau new $(CURVE) $(PTAU_BITS) $(PTAU1)

$(PTAU2): $(PTAU1) | $(KEYDIR)
	@echo "🔑 [Setup/Phase2]"
	snarkjs powersoftau prepare phase2 $(PTAU1) $(PTAU2)

$(ZKEY): $(R1CS) $(PTAU2) | $(ARTIFACT)
	@echo "📦 [Setup/Groth16] Proving key"
	snarkjs groth16 setup $(R1CS) $(PTAU2) $(ZKEY)

$(VK): $(ZKEY) | $(ARTIFACT)
	@echo "🔍 [Setup/Groth16] Verification key"
	snarkjs zkey export verificationkey $(ZKEY) $(VK)

prove: $(PROOF)

$(PROOF): $(ZKEY) $(WIT) $(PUB) | $(ARTIFACT)
	@echo "🛡️  [Prove] Generating proof"
	snarkjs groth16 prove $(ZKEY) $(WIT) $(PROOF) $(PUB)

verify: $(VK) $(PUB) $(PROOF)
	@echo "✅ [Verify] Checking proof"
	snarkjs groth16 verify $(VK) $(PUB) $(PROOF)

# ----------------------------------------------------------------------
# Utilities
# ----------------------------------------------------------------------
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

# ----------------------------------------------------------------------
# Cleanup
# ----------------------------------------------------------------------
clean:
	@echo ">> clean: removing build outputs for $(CIRCUIT)"
	rm -rf $(BUILD_DIR)/$(CIRCUIT)

distclean: clean
	@echo ">> distclean: removing zk artifacts (except *.ptau/*.tau)"
	find $(BUILD_DIR) -type f \( -name '*.zkey' -o -name '*.wtns' -o -name '*proof.json' -o -name '*public.json' \) -print -delete || true

clobber:
	@if [ "$$ALLOW_CLOBBER" != "1" ]; then echo "Refusing to delete PTAU without ALLOW_CLOBBER=1"; exit 2; fi
	rm -f $(BUILD_DIR)/keys/*.ptau $(BUILD_DIR)/keys/*.tau