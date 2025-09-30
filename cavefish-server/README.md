# Editor Setup: Tags Navigation
This project uses **tags** for fast "go-to-definition" navigation in editors like VS Code, Vim, and Emacs. The setup leverages `fast-tags` for Haskell-aware tag generation and a helper script, `gen-tags.sh`, for reproducible builds.

## 1. Prerequisites
Ensure you have:
- **Nix**: Installed to provide a reproducible environment (see [Nix installation guide](https://nixos.org/download.html)).
- **VS Code**: For the recommended editor experience.
- A Haskell project with a `cabal.project` file and dependencies.

## 2. Setting Up the Environment
All required tools, including `fast-tags`, `cabal`, and `git`, are provided in the project’s Nix environment.

To enter the environment:
```bash
nix-shell
```
This ensures all dependencies are available without global installation.

## 3. Installing the VS Code Extension
For VS Code, install the **ctagsx** extension:
- **Purpose**: Provides cross-platform ctags integration.
- **Features**:
  - **Go to Definition**: `F12` or `Ctrl+Click` on symbols.
  - **Search Tags**: `Ctrl+T` (or `Cmd+T` on macOS) to find symbols.
  - **Jump Back**: `Alt+T` to return to previous location.
  - **Manual Tag Search**: `Ctrl+Alt+T` to enter a tag manually.
- **Installation**: Search for "ctagsx" in the VS Code Extensions Marketplace and install.

## 4. Generating Tags
The `gen-tags.sh` script generates tags for Haskell source files using `fast-tags`. It scans:
- **Project Sources**: `./cavefish` directory (if present).
- **Built Dependencies**: `dist-newstyle/src` (populated by `cabal build`).
- **Source Repository Packages (SRPs)**: Defined in `cabal.project`.
- **Hackage Dependencies**: Resolved via `cabal.project.freeze` or `plan.json`.

To generate or update tags:
```bash
./gen-tags.sh
```

This produces:
- `tags`: For editors like Vim and VS Code.
- `TAGS`: For Emacs.

### Notes on Generation
- **Caching**: Dependencies are cached in `.deps-src/` to avoid redundant fetches.
- **Performance**: The script fetches dependencies in parallel where possible.
- **Requirements**: Must be run inside the `nix-shell` to access `fast-tags`.

## 5. Cleaning and Rebuilding Tags
To start from scratch (e.g., after dependency changes):
```bash
./gen-tags.sh clean
./gen-tags.sh
```
The `clean` command removes:
- `.deps-src/` (cached dependency sources).
- `tags` and `TAGS` files.

## 6. Using Tags in VS Code
With the `ctagsx` extension installed:
- **Go to Definition**: Press `F12` or `Ctrl+Click` on a symbol.
- **Search Symbols**: Use `Ctrl+T` (or `Cmd+T` on macOS) to search tags.
- **Navigate Back**: Press `Alt+T` to return to the previous location.
- **Manual Tag Entry**: Use `Ctrl+Alt+T` to manually enter a tag name.

## 7. Troubleshooting
- **No tags generated**:
  - Ensure you’re in the `nix-shell` (`fast-tags` must be available).
  - Verify `cabal.project` and `*.cabal` files exist.
  - Run `cabal build --only-dependencies` to populate `dist-newstyle/src`.
- **Missing definitions**:
  - Check if `tags` or `TAGS` files exist in the project root.
  - Re-run `./gen-tags.sh` to refresh tags.
- **Extension issues**:
  - Confirm `ctagsx` is installed and enabled in VS Code.
  - Check VS Code’s Output panel (ctagsx channel) for errors.
- **Git or Hackage failures**:
  - Ensure internet connectivity.
  - Inspect `.deps-src/` for incomplete clones or downloads.

## 8. Additional Notes
- The `gen-tags.sh` script is optimized for Haskell projects and handles complex dependency trees.
- For Vim/Emacs users, the `tags` file supports native tag navigation (e.g., `:tag` in Vim or `M-.` in Emacs).
- If you modify `cabal.project` or dependencies, re-run `./gen-tags.sh` to update tags.
- For large projects, tag generation may take a few minutes due to dependency fetching.