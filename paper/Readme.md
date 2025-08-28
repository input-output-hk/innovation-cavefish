<h1 align="center">Cavefish Paper</h1>
<h3 align="center">Communication-Optimal Light Client Protocol for UTxO Ledgers</h3>


This folder contains the LaTeX sources for the academic paper **Cavefish: Communication-Optimal Light Client Protocol for UTxO Ledgers**

The paper introduces a novel **intent-based light client protocol** for UTxO blockchains like Cardano and Bitcoin.  
It enables resource-constrained devices to build and submit transactions **without downloading or parsing the full ledger**,  
using a two-party protocol between a **Light Client (LC)** and a **Service Provider (SP)**,  
with built-in **fair compensation** and **security via weakly blind predicate signatures**.

---

## 📂 Layout

- `lc_main.tex` — main entry point for the paper.  
- `sections/` — (if present) individual `.tex` sections.  
- `figures/` — images, diagrams, plots used in the paper.  
- `tmp/` — auxiliary build folder (generated automatically).  
- `../publication/` — final PDFs with timestamped filenames.  

---

## 🛠️ Building the Paper

We use `latexmk` to simplify compilation.

### Quick Build
Run inside `./paper/`:
```bash
make
````

This will:

1. Compile `lc_main.tex` with LaTeX.
2. Store all auxiliary files under `./tmp/`.
3. Copy the final PDF to `../publication/` with a timestamp, e.g.:

   ```
   ../publication/cavefish_2025-08-28_1015.pdf
   ```

### Clean

Remove temporary files:

```bash
make clean
```

### Distclean

Remove temporary files **and** the `../publication/` folder:

```bash
make distclean
```

---

## 🔧 Requirements

* **LaTeX distribution** (TeX Live / MacTeX / MiKTeX).
* **latexmk** (build automation).
* **biber** if bibliography is used.
* **Pygments** if minted is used (code listings):

  ```bash
  pip install Pygments
  ```

---

## ℹ️ Notes

* This folder is **only for the paper** sources.
* The initiative-level documentation (problem statement, innovation, CIP/CPS references, etc.) is described in the **root README.md**.
* The root-level PDFs under `../publication/` are the canonical builds.

