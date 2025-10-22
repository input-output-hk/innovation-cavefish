# ======================================================================
# WBPS — Circom + SnarkJS workflow (example-or-generate input)
# - Vendorizes circomlib (shallow clone) under ./vendor
# - Default uses examples/<circuit>_input.json if present, else generates
# - Prints a LOG PREFACE + PROPERTY OVERVIEW before witness
# - Records per-step timings and prints a benchmark report at the end
# - OUTPUT LAYOUT (new):
#     build/<circuit>/
#       relation/{relation.r1cs,relation_symbols.sym}
#       witness_generator/{witness_gen.wasm,generate_witness.js,witness_calculator.js}
#       setup/{powersoftau.ptau,pot_final.ptau,proving_key.zkey,verification_key.json}
#       runs/<UTC-ISO>/
#         input/generation_input.json
#         witness/{witness.wtns,public_signals.json}
#         logs/{generation_raw.log,generation_report.txt,generation_report.json}
#         proof/{proof.json,message_public_part.json}
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
# Paths (top-level)
# ----------------------------------------------------------------------
CIRCUIT_DIR := circuits
BUILD_DIR   := build
ARTIFACT    := $(BUILD_DIR)/$(CIRCUIT)
VENDOR_DIR  := vendor

# Static artifact folders
REL_DIR   := $(ARTIFACT)/relation
SETUP_DIR := $(ARTIFACT)/setup
WITGEN_DIR := $(ARTIFACT)/witness_generator

# Per-run folders
RUNS_DIR := $(ARTIFACT)/runs
RUN ?= $(shell date -u +%Y-%m-%dT%H-%M-%SZ)
RUN_DIR := $(RUNS_DIR)/$(RUN)

INPUT_DIR := $(RUN_DIR)/input
WIT_DIR   := $(RUN_DIR)/witness
LOG_DIR   := $(RUN_DIR)/logs
PROOF_DIR := $(RUN_DIR)/proof

INCLUDES  := -l $(VENDOR_DIR)/circomlib -l $(VENDOR_DIR)/hashing_circuits -l .

# ----------------------------------------------------------------------
# Files
# ----------------------------------------------------------------------
CIRCUIT_FILE := $(CIRCUIT_DIR)/$(CIRCUIT).circom

# relation/ (renamed artifacts)
R1CS   := $(REL_DIR)/relation.r1cs
SYM    := $(REL_DIR)/relation_symbols.sym

# witness_generator/ (runtime moved here)
WASM        := $(WITGEN_DIR)/witness_gen.wasm
GENWIT_JS   := $(WITGEN_DIR)/generate_witness.js
WITCALC_JS  := $(WITGEN_DIR)/witness_calculator.js

# setup/
ZKEY   := $(SETUP_DIR)/proving_key.zkey
VK     := $(SETUP_DIR)/verification_key.json
PTAU1  := $(SETUP_DIR)/powersoftau.ptau
PTAU2  := $(SETUP_DIR)/pot_final.ptau

# per-run main IO
INPUT  ?= $(INPUT_DIR)/generation_input.json
WIT    := $(WIT_DIR)/witness.wtns
PUB    := $(PROOF_DIR)/public_signals.json
PROOF  := $(PROOF_DIR)/proof.json
MSG_PUB := $(PROOF_DIR)/message_public_part.json

# Example input (preferred if present)
EXAMPLE_INPUT := examples/$(CIRCUIT)_input.json

# Logs
WITLOG  := $(LOG_DIR)/generation_raw.log
LOGTXT  := $(LOG_DIR)/generation_report.txt
LOGJSON := $(LOG_DIR)/generation_report.json

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
# Timings (append-only TSV) and helpers
# ----------------------------------------------------------------------

TIMINGS := $(LOG_DIR)/timings.tsv

define __now_ms
python3 -c 'import time,sys; sys.stdout.write(str(int(time.time()*1000)))'
endef

define TS
@{ mkdir -p $(ARTIFACT); \
   printf "%s\t%s\t%s\n" "$(1)" "$(2)" "$$($(__now_ms))"; } >> $(TIMINGS)
endef

# ----------------------------------------------------------------------
# Phonies
# ----------------------------------------------------------------------
.PHONY: all help deps deps-circomlib deps-hashing \
        compile witness setup prove verify info size-hint \
        clean distclean clobber \
        use-example-or-generate gen-input gen-input-debug \
        with-example with-generated-input tree show-paths bench-report \
        ensure-run-dirs

# ----------------------------------------------------------------------
# Entrypoints
# ----------------------------------------------------------------------
all: with-example

help:
	@echo "WBPS Circom/SnarkJS workflow"
	@echo "  make (with-example)      → compile + example-or-generate + witness + setup + prove + verify + bench-report"
	@echo "  make with-generated-input → compile + generator + witness + setup + prove + verify + bench-report"
	@echo "  make gen-input[-debug]    → only regenerate input"
	@echo "  make info | size-hint | clean | distclean | clobber"

tree:
	@printf "%s\n" \
	"circuits/" \
	"tooling/inputgen/" \
	"build/<circuit>/relation/" \
	"build/<circuit>/setup/" \
	"build/<circuit>/witness_generator/" \
	"build/<circuit>/runs/<utc-iso>/{input,witness,logs,proof}/" \
	"vendor/"

# Ensure all directories exist for this run
ensure-run-dirs:
	@mkdir -p $(REL_DIR) $(SETUP_DIR) $(WITGEN_DIR) $(RUN_DIR) $(INPUT_DIR) $(WIT_DIR) $(LOG_DIR) $(PROOF_DIR)

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
# Build / Compile → relation/
# ----------------------------------------------------------------------
compile: deps ensure-run-dirs $(R1CS) $(WASM) $(SYM) $(GENWIT_JS) $(WITCALC_JS)

# Emit to relation/ then rename to our canonical filenames
$(R1CS) $(WASM) $(SYM) $(GENWIT_JS) $(WITCALC_JS): $(CIRCUIT_FILE) | $(REL_DIR)
	$(call TS,compile,START)
	@echo "🔧 [Compile] $(CIRCUIT_FILE) → $(REL_DIR)/"
	# Compile in-place under relation/
	circom $(CIRCUIT_FILE) --r1cs --wasm --sym $(INCLUDES) -o $(REL_DIR)
	# Rename to canonical names
	@mv -f "$(REL_DIR)/$(CIRCUIT).r1cs" "$(R1CS)"
	@mv -f "$(REL_DIR)/$(CIRCUIT).sym"  "$(SYM)"
	# Move runtime into witness_generator/
	@mkdir -p "$(WITGEN_DIR)"
	@mv -f "$(REL_DIR)/$(CIRCUIT)_js/$(CIRCUIT).wasm" "$(WASM)"
	@mv -f "$(REL_DIR)/$(CIRCUIT)_js/generate_witness.js" "$(GENWIT_JS)"
	@mv -f "$(REL_DIR)/$(CIRCUIT)_js/witness_calculator.js" "$(WITCALC_JS)"
	@rm -rf "$(REL_DIR)/$(CIRCUIT)_js"
	$(call TS,compile,END)

# ----------------------------------------------------------------------
# Input: copy example if exists, otherwise generate → runs/<run>/input/
# ----------------------------------------------------------------------
with-example: compile use-example-or-generate witness setup prove verify bench-report
with-generated-input: compile gen-input witness setup prove verify bench-report

use-example-or-generate: ensure-run-dirs
	$(call TS,input,START)
	@echo "📥 Preparing input → $(INPUT)"
	@if [ -f "$(EXAMPLE_INPUT)" ]; then \
	  cp "$(EXAMPLE_INPUT)" "$(INPUT)"; \
	  echo "📥 Using example input → $(INPUT)"; \
	else \
	  echo "ℹ️ No example input at $(EXAMPLE_INPUT). Generating one..."; \
	  $(MAKE) gen-input GEN_DEBUG=$(GEN_DEBUG); \
	fi
	$(call TS,input,END)

# Generator deps only when needed
$(GEN_NODEM): $(GEN_PKJ)
	cd $(GEN_DIR); \
	if [ ! -f package-lock.json ]; then npm install --no-fund --no-audit; \
	else npm ci --no-fund --no-audit; fi
	@echo "✅ generator deps installed"

gen-input: $(GEN_BIN) $(GEN_NODEM) ensure-run-dirs
	@echo "📝 [Init] Generating $(abspath $(INPUT)) via $(GEN_BIN)"
	node "$(GEN_BIN)" $(GEN_ARGS)
	@echo "✅ Wrote $(abspath $(INPUT))"

gen-input-debug:
	@$(MAKE) gen-input GEN_DEBUG=1

# ----------------------------------------------------------------------
# LOG PREFACE + PROPERTY OVERVIEW (printed before witness)
# ----------------------------------------------------------------------
define WBPS_LOG_PREFACE

[1;36m──────────────────────── WBPS CIRCUIT LOGS ────────────────────────[0m
These numeric tags are emitted by the circuit during witness generation.
They are [1;33mCAPTURED silently[0m to file and then parsed into a structured
report ([1m[32m$(LOGTXT)[0m / [1m[32m$(LOGJSON)[0m). Nothing is printed
to the terminal while the circuit runs.

[1;36mNonce / commitment base (P1):[0m
  [1;33m900010[0m → [1m(g^ρ).x[0m     — x-coordinate of the BabyJub point g^ρ
  [1;33m900011[0m → [1m(g^ρ).y[0m     — y-coordinate of the BabyJub point g^ρ
  (ρ is range-checked via [1mNum2Bits(251)[0m; g^ρ is computed via [1mEscalarMulFix[0m.)

[1;36mCiphertext limbs (P2):[0m for each limb [1mi[0m = 0..(nb_commitment_limbs−1)
  [1;33m900200+i[0m  (tag header for limb i)
    [32mline 2:[0m μ̂[i]            — 254-bit packed limb of μ
    [32mline 3:[0m PRF[i]           — Poseidon-derived mask from (ek^ρ)
    [32mline 4:[0m μ̂[i] + PRF[i]   — masked limb
    [32mline 5:[0m Cmsg[i]          — provided ciphertext limb
    [32mline 6:[0m Δ[i] = Cmsg[i] − (μ̂[i] + PRF[i])  — should be [1;32m0[0m
  (The equality [1mCmsg[i] == μ̂[i] + PRF[i][0m is verified at the [1;35mTOP LEVEL[0m.)

[1;36mTranscript / challenge digest (P3):[0m
  [1;33m900300..900363[0m → c[j] for j=0..63, where
    [1mc = SHA-512(R || X || μ_bits)[0m,
    with R and X fed byte-wise bit-reversed as in Cardano’s EdDSA.

[1;36mLegend:[0m
  ρ       — commitment randomizer scalar
  ek      — solver encryption (ElGamal) public key
  μ       — message bits after overlay (public ⊕ private)
  μ̂[i]   — i-th 254-bit field limb of μ
  PRF[i]  — Poseidon-derived mask stream seeded by (ek^ρ)

[1;36mThe parsed report will summarize:[0m
  • [1;35mP1[0m: Commitment base [1m(g^ρ)[0m (x, y)
  • [1;35mP2[0m: Ciphertext binding per limb ([1mCmsg[i] ≟ μ̂[i] + PRF[i][0m)
  • [1;35mP3[0m: SHA-512 transcript digest c[0..63]

[1;36mExample outputs:[0m
  [1;32m✓[0m  Δ[i] = 0   → constraint satisfied
  [1;31m✗[0m  Δ[i] ≠ 0   → mismatch (likely constraint failure)

[1;36m────────────────────────────────────────────────────────────────────[0m
endef
export WBPS_LOG_PREFACE

define WBPS_PROPERTY_OVERVIEW
[1;34m──────────────────────── WBPS Proof Properties ────────────────────────[0m
[1mP0 – Message Reconstruction[0m
  Builds: the message [1mμ[0m by overlaying the private window onto the public bits
  Formula: [1mμ = Overlay(m_pub, m_priv)[0m

[1mP1 – Commitment Scalar Correctness[0m
  Range-check [1mρ ∈ [0,2^251)[0m, compute [1mek^ρ[0m and [1mg^ρ[0m, then assert:
    [1mC₀ == g^ρ[0m
  This binds the commitment to the nonce scalar [1mρ[0m.

[1mP2 – Ciphertext Binding Consistency[0m
  Pack [1mμ[0m into 254-bit limbs [1mμ̂[i][0m and generate a PRF stream from [1mek^ρ[0m.
  For every limb [1mi[0m:
    [1mCmsg[i] == μ̂[i] + PRF[i][0m   (mod p)
  Ensures the ciphertext encodes [1mμ[0m under the solver key and nonce.

[1mP3 – Transcript Integrity[0m
  Rebuild the Schnorr transcript:
    [1mc = SHA-512(R || X || μ_bits)[0m
  and assert the provided challenge equals [1mc[0m.
[1;34m───────────────────────────────────────────────────────────────────────[0m
endef
export WBPS_PROPERTY_OVERVIEW

# ----------------------------------------------------------------------
# Witness / Setup / Prove / Verify
# ----------------------------------------------------------------------
witness: $(WIT) $(PUB)

$(WIT): $(WASM) $(INPUT) | ensure-run-dirs
	@echo "$$WBPS_LOG_PREFACE"
	@echo "$$WBPS_PROPERTY_OVERVIEW"
	@echo "🧮 [Witness] $(INPUT) → $(WIT)"
	$(call TS,witness,START)
	# Capture circuit logs silently; redirect both stdout & stderr
	(snarkjs wtns calculate $(WASM) $(INPUT) $(WIT) >$(WITLOG) 2>&1) || true
	$(call TS,witness,END)
	$(call TS,log-parse,START)
	# Parse logs and emit colored summary
	@python3 tooling/parse_wbps_logs.py $(WITLOG) --txt $(LOGTXT) --json $(LOGJSON) --color || true
	$(call TS,log-parse,END)
	@echo "🧾 [Logs] Parsed → $(LOGTXT)"

$(PUB): $(WIT)
	$(call TS,export-public,START)
	@echo "📤 [Witness] Export public → $(PUB)"
	snarkjs wtns export json $(WIT) $(PUB)
	$(call TS,export-public,END)

# --- Setup (static under setup/) ---
setup: $(VK)

$(PTAU1):
	$(call TS,setup-phase1,START)
	@mkdir -p $(SETUP_DIR)
	@echo "🌍 [Setup/Phase1] curve=$(CURVE), size=2^$(PTAU_BITS)"
	snarkjs powersoftau new $(CURVE) $(PTAU_BITS) $(PTAU1)
	$(call TS,setup-phase1,END)

$(PTAU2): $(PTAU1)
	$(call TS,setup-phase2,START)
	@echo "🔑 [Setup/Phase2]"
	snarkjs powersoftau prepare phase2 $(PTAU1) $(PTAU2)
	$(call TS,setup-phase2,END)

$(ZKEY): $(R1CS) $(PTAU2)
	$(call TS,keygen,START)
	@echo "📦 [Setup/Groth16] Proving key"
	snarkjs groth16 setup $(R1CS) $(PTAU2) $(ZKEY)
	$(call TS,keygen,END)

$(VK): $(ZKEY)
	@echo "🔍 [Setup/Groth16] Verification key"
	snarkjs zkey export verificationkey $(ZKEY) $(VK)

# --- Proof (per-run under runs/<run>/proof/) ---
prove: $(PROOF)

# Extract only the portion needed by proof/verification
$(MSG_PUB): $(INPUT) | $(PROOF_DIR)
	@jq '.message_public_part // {}' "$(INPUT)" > "$(MSG_PUB)" 2>/dev/null || echo "{}" > "$(MSG_PUB)"
	@echo "🧾 [Proof] message_public_part extracted → $(MSG_PUB)"

$(PROOF): $(ZKEY) $(WIT) $(PUB) $(MSG_PUB) | $(PROOF_DIR)
	$(call TS,prove,START)
	@echo "🛡️  [Prove] Generating proof"
	snarkjs groth16 prove $(ZKEY) $(WIT) $(PROOF) $(PUB)
	$(call TS,prove,END)

verify: $(VK) $(PUB) $(PROOF)
	$(call TS,verify,START)
	@echo "✅ [Verify] Checking proof"
	snarkjs groth16 verify $(VK) $(PUB) $(PROOF)
	$(call TS,verify,END)

# ----------------------------------------------------------------------
# Benchmark report (printed automatically at end of entrypoint targets)
# ----------------------------------------------------------------------
bench-report:
	@set -euo pipefail; \
	NO_COLOR=$${NO_COLOR:-}; \
	COL_RESET="\033[0m"; COL_SEC="\033[1;36m"; \
	if [ -n "$$NO_COLOR" ]; then COL_RESET=""; COL_SEC=""; fi; \
	if [ ! -f "$(TIMINGS)" ]; then echo "No timings found."; exit 0; fi; \
	awk -F '\t' ' \
	  function msfmt(ms,  s,m,h,rem) { \
	    s=int(ms/1000); rem=ms%1000; m=int(s/60); s=s%60; h=int(m/60); m=m%60; \
	    if (h>0) return sprintf("%dh %02dm %02ds.%03d",h,m,s,rem); \
	    if (m>0) return sprintf("%dm %02ds.%03d",m,s,rem); \
	    return sprintf("%2ds.%03d",s,rem); \
	  } \
	  { m[$$1,$$2]=$$3 } \
	  END { \
	    order[1]="compile";        names["compile"]="Compile (circom)"; \
	    order[2]="input";          names["input"]="Prepare input"; \
	    order[3]="witness";        names["witness"]="Witness (WASM)"; \
	    order[4]="log-parse";      names["log-parse"]="Parse circuit logs"; \
	    order[5]="export-public";  names["export-public"]="Export public signals"; \
	    order[6]="setup-phase1";   names["setup-phase1"]="Setup Phase 1 (powersoftau new)"; \
	    order[7]="setup-phase2";   names["setup-phase2"]="Setup Phase 2 (prepare phase2)"; \
	    order[8]="keygen";         names["keygen"]="Groth16 Keygen (setup)"; \
	    order[9]="prove";          names["prove"]="Groth16 Prove"; \
	    order[10]="verify";        names["verify"]="Groth16 Verify"; \
	    printf("%s──────────────── WBPS Benchmark Report ────────────────%s\n", ENVIRON["COL_SEC"], ENVIRON["COL_RESET"]); \
	    total=0; printed=0; \
	    for (i=1;i<=10;i++) { \
	      s=order[i]; \
	      if ((s,"START") in m && (s,"END") in m) { \
	        d=m[s,"END"]-m[s,"START"]; total+=d; printed++; \
	        printf("  %-30s %s\n", names[s], msfmt(d)); \
	      } \
	    } \
	    if (printed==0) { print "No timings found."; exit 0; } \
	    if (total>0) printf("\n  TOTAL                         %s\n", msfmt(total)); \
	    printf("%s─────────────────────────────────────────────────────%s\n", ENVIRON["COL_SEC"], ENVIRON["COL_RESET"]); \
	  }' "$(TIMINGS)"

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
	@echo "Circuit:              $(CIRCUIT_FILE)"
	@echo "Build dir:            $(ARTIFACT)"
	@echo "Relation dir:         $(REL_DIR)"
	@echo "Setup dir:            $(SETUP_DIR)"
	@echo "Witness gen dir:      $(WITGEN_DIR)"
	@echo "Runs dir:             $(RUNS_DIR)"
	@echo "This run:             $(RUN_DIR)"
	@echo "R1CS:                 $(R1CS)"
	@echo "SYM:                  $(SYM)"
	@echo "WASM:                 $(WASM)"
	@echo "generate_witness.js:  $(GENWIT_JS)"
	@echo "witness_calculator.js:$(WITCALC_JS)"
	@echo "Input (example):      $(EXAMPLE_INPUT)"
	@echo "Input (this run):     $(INPUT)"
	@echo "Witness:              $(WIT)"
	@echo "Public signals:       $(PUB)"
	@echo "Raw logs:             $(WITLOG)"
	@echo "Parsed (txt):         $(LOGTXT)"
	@echo "Parsed (json):        $(LOGJSON)"
	@echo "Proof:                $(PROOF)"
	@echo "message_public_part:  $(MSG_PUB)"
	@echo "PTAU1:                $(PTAU1)"
	@echo "PTAU2:                $(PTAU2)"
	@echo "ZKey:                 $(ZKEY)"
	@echo "VK:                   $(VK)"
	@echo "Timings TSV:          $(TIMINGS)"

# ----------------------------------------------------------------------
# Cleanup
# ----------------------------------------------------------------------
clean:
	@echo ">> clean: removing build outputs for $(CIRCUIT)"
	rm -rf $(BUILD_DIR)/$(CIRCUIT)

distclean: clean
	@echo ">> distclean: removing zk artifacts (except *.ptau/*.tau)"
	find $(ARTIFACT) -type f \( -name '*.zkey' -o -name '*.wtns' -o -name '*proof.json' -o -name '*public.json' \) -print -delete || true

clobber:
	@if [ "$$ALLOW_CLOBBER" != "1" ]; then echo "Refusing to delete PTAU without ALLOW_CLOBBER=1"; exit 2; fi
	rm -f $(SETUP_DIR)/*.ptau $(SETUP_DIR)/*.tau
