# ================================================================
# Cavefish Paper — LaTeX Build System
# ================================================================
# This Makefile lives in ./paper/ and builds lc_main.tex
#
# Layout:
#   ./paper/       -> sources (lc_main.tex + others)
#   ./paper/tmp/   -> auxiliary/temp files (.aux, .log, etc.)
#   ../publication/-> final timestamped PDFs
#
# Example output:
#   ../publication/cavefish_2025-08-28_1015.pdf
#
# Targets:
#   make          -> compile paper, produce timestamped PDF
#   make clean    -> remove aux files from ./paper/tmp/
#   make distclean-> clean aux files + remove ../publication/
#
# Dependencies:
#   - latexmk (recommended)
#   - a full TeX distribution (TeX Live / MacTeX / MiKTeX)
#   - biber (if bibliography is used)
#   - Pygments + -shell-escape (if minted is used)
# ================================================================

# Entry LaTeX file (without extension)
MAIN=lc_main

# Where to store auxiliary/temp files
TMP_DIR=tmp

# Where final PDFs are stored (relative to ./paper/)
OUT_DIR=../publication

# Timestamp for unique output filenames (YYYY-MM-DD_HHMM)
DATE := $(shell date +"%Y-%m-%d_%H%M")

# Final PDF name (short + timestamped)
PDF=$(OUT_DIR)/cavefish_$(DATE).pdf

# ------------------------------------------------
# Default target: build the PDF
# ------------------------------------------------
all: $(PDF)

# Build rule: compile LaTeX and copy PDF to OUT_DIR
$(PDF):
	@mkdir -p $(OUT_DIR)
	@mkdir -p $(TMP_DIR)
	# Compile LaTeX with all aux files in ./paper/tmp/
	latexmk -pdf -interaction=nonstopmode -file-line-error \
	    -outdir=$(TMP_DIR) $(MAIN).tex
	# Copy the generated PDF to OUT_DIR with timestamp
	cp $(TMP_DIR)/$(MAIN).pdf $(PDF)

# ------------------------------------------------
# Clean auxiliary build files (but keep final PDFs)
# ------------------------------------------------
clean:
	latexmk -C -outdir=$(TMP_DIR) $(MAIN).tex
	rm -f $(TMP_DIR)/$(MAIN).pdf

# ------------------------------------------------
# Full cleanup: aux files + remove ../publication/
# ------------------------------------------------
distclean: clean
	rm -rf $(OUT_DIR)
