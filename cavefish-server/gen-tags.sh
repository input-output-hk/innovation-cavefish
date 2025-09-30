#!/usr/bin/env bash
set -euo pipefail

DEPS_ROOT=".deps-src"
HACKAGE_ROOT="${DEPS_ROOT}/hackage"
SRP_ROOT="${DEPS_ROOT}/srp"
TAGS_FILE="tags"
ETAGS_FILE="TAGS"
ROOT="$PWD"

mkdir -p "$HACKAGE_ROOT" "$SRP_ROOT" "$DEPS_ROOT"

clean_all() {
  echo "Cleaning generated sources and tags..."
  rm -rf "$DEPS_ROOT" "$TAGS_FILE" "$ETAGS_FILE"
  echo "Clean complete."
  exit 0
}

# --- helpers ---------------------------------------------------------------

proj_name=$(awk 'tolower($1)=="name:"{print $2; exit}' *.cabal 2>/dev/null || true)

collect_dist_src_roots() {
  find dist-newstyle -type f -path "*/src/*/*.cabal" -print0 2>/dev/null |
    xargs -0 -n1 dirname | sort -u
}

# parse plan.json with jq (fallback to python if jq missing)
parse_plan_json() {
  local plan=""
  for candidate in "dist-newstyle/cache/plan.json" "plan.json"; do
    if [ -f "$candidate" ]; then plan="$candidate"; break; fi
  done
  [ -n "$plan" ] || return 0

  if command -v jq >/dev/null 2>&1; then
    jq -r '.["install-plan"][] 
            | select(.["pkg-name"] and .["pkg-version"]) 
            | "\(.["pkg-name"]) \(.["pkg-version"])"' "$plan"
  else
    python3 - "$plan" <<'PY'
import json, sys
with open(sys.argv[1]) as f: data=json.load(f)
for u in data.get("install-plan", []):
    n, v = u.get("pkg-name"), u.get("pkg-version")
    if n and v:
        print(n, v)
PY
  fi
}

# --- cli -------------------------------------------------------------------
case "${1:-}" in
  clean) clean_all ;;
esac

DIRS=()

# [1] project
if [ -d "./cavefish" ]; then
  echo "[1/4] project ./cavefish"
  DIRS+=("./cavefish")
elif [ -n "$proj_name" ]; then
  echo "[1/4] project $PWD"
  DIRS+=("$PWD")
else
  echo "[1/4] project (none detected)"
fi

# [2] dist-newstyle/src
echo "[2/4] dist-newstyle/src"
while IFS= read -r d; do
  [ -n "${d:-}" ] && DIRS+=("$d") && echo "  - $d"
done < <(collect_dist_src_roots || true)

# [3] SRPs from cabal.project (POSIX-awk, no hangs)
echo "[3/4] SRPs (from cabal.project)"

srp_emit() {
  # prefer tag, then commit, then branch
  local url="$1" tag="$2" commit="$3" branch="$4"
  local ref=""
  [ -n "$tag" ] && ref="$tag"
  [ -z "$ref" ] && [ -n "$commit" ] && ref="$commit"
  [ -z "$ref" ] && [ -n "$branch" ] && ref="$branch"
  [ -z "$url" ] && return 0
  [ -z "$ref" ] && { echo "  - SRP at $url (no tag/commit/branch) — skipping"; return 0; }

  local repo dest
  repo=$(basename "${url%.git}")
  dest="${SRP_ROOT}/${repo}@${ref}"

  if [ ! -d "$dest/.git" ]; then
    echo "  - git clone $url -> $dest"
    if command -v timeout >/dev/null 2>&1; then
      timeout 45s git clone --filter=blob:none --depth 1 "$url" "$dest" >/dev/null 2>&1 \
      || timeout 45s git clone "$url" "$dest" >/dev/null 2>&1 || true
      timeout 15s git -C "$dest" fetch --tags >/dev/null 2>&1 || true
      timeout 15s git -C "$dest" checkout "$ref" >/dev/null 2>&1 || true
    else
      git clone --filter=blob:none --depth 1 "$url" "$dest" >/dev/null 2>&1 \
      || git clone "$url" "$dest" >/dev/null 2>&1 || true
      git -C "$dest" fetch --tags >/dev/null 2>&1 || true
      git -C "$dest" checkout "$ref" >/dev/null 2>&1 || true
    fi
  fi
  DIRS+=("$dest")
}

if [ -f cabal.project ]; then
  awk '
    BEGIN{blk=0; url=""; tag=""; commit=""; branch=""}
    /^[[:space:]]*source-repository-package([[:space:]]|$)/ {
      if (blk) { print url "\t" tag "\t" commit "\t" branch }
      blk=1; url=""; tag=""; commit=""; branch=""; next
    }
    blk && /^[[:space:]]*location:[[:space:]]*/ {
      sub(/^[[:space:]]*location:[[:space:]]*/,""); url=$0; next
    }
    blk && /^[[:space:]]*tag:[[:space:]]*/ {
      sub(/^[[:space:]]*tag:[[:space:]]*/,""); tag=$0; next
    }
    blk && /^[[:space:]]*commit:[[:space:]]*/ {
      sub(/^[[:space:]]*commit:[[:space:]]*/,""); commit=$0; next
    }
    blk && /^[[:space:]]*branch:[[:space:]]*/ {
      sub(/^[[:space:]]*branch:[[:space:]]*/,""); branch=$0; next
    }
    END{
      if (blk) { print url "\t" tag "\t" commit "\t" branch }
    }
  ' cabal.project | while IFS=$'\t' read -r url tag commit branch; do
       srp_emit "$url" "$tag" "$commit" "$branch"
     done
else
  echo "  - no cabal.project found"
fi

# [4] Hackage sources (from plan.json)
echo "[4/4] Hackage sources (from plan.json)"

# Refresh indices (best-effort)
cabal v2-update >/dev/null 2>&1 || true

# Unique list "pkg ver"
mapfile -t plan_lines < <(parse_plan_json | awk 'NF==2{print $1" "$2}' | sort -u)

EXCLUDE_RE='^(ghc($|-)|ghc-.*|ghc-prim|rts)$'
MAX_JOBS=$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

fetch_pkg() {
  local pkg="$1" ver="$2" dest="${HACKAGE_ROOT}/${pkg}-${ver}" lock="${HACKAGE_ROOT}/.locks/${pkg}-${ver}.lock"

  [[ "$pkg" =~ $EXCLUDE_RE ]] && return 0
  [ -n "$proj_name" ] && [ "$pkg" = "$proj_name" ] && return 0
  [ -z "$ver" ] && return 0

  # per-pkg lock (mkdir is atomic)
  mkdir "$lock" 2>/dev/null || return 0

  if [ -d "$dest" ]; then
    echo "  - cached ${pkg}-${ver}"
    printf '%s\0' "$dest" >> "$DEPS_ROOT/.fetched.z"
    rmdir "$lock" 2>/dev/null || true
    return 0
  fi

  if (cd "$ROOT" && cabal get "${pkg}-${ver}" --destdir "$HACKAGE_ROOT" >/dev/null 2>&1); then
    echo "  - fetched ${pkg}-${ver}"
    printf '%s\0' "$dest" >> "$DEPS_ROOT/.fetched.z"
  else
    # if a concurrent worker finished first, treat as cached
    if [ -d "$dest" ]; then
      echo "  - cached ${pkg}-${ver}"
      printf '%s\0' "$dest" >> "$DEPS_ROOT/.fetched.z"
    else
      echo "  - FAILED to fetch ${pkg}-${ver}"
    fi
  fi

  rmdir "$lock" 2>/dev/null || true
}
export -f fetch_pkg
export HACKAGE_ROOT DEPS_ROOT proj_name EXCLUDE_RE ROOT

rm -f "$DEPS_ROOT/.fetched.z"
mkdir -p "$HACKAGE_ROOT/.locks"

MAX_JOBS=$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

# robust parallel fan-out with back-pressure
running=0
while read -r pkg ver; do
  [ -z "$pkg" ] || [ -z "$ver" ] && continue
  [[ "$pkg" =~ $EXCLUDE_RE ]] && continue
  [ -n "$proj_name" ] && [ "$pkg" = "$proj_name" ] && continue

  fetch_pkg "$pkg" "$ver" &

  running=$((running+1))
  if [ "$running" -ge "$MAX_JOBS" ]; then
    wait -n 2>/dev/null || wait
    running=$((running-1))
  fi
done < <(parse_plan_json | sort -u)

wait || true

# Load fetched dirs (NUL-safe)
if [ -f "$DEPS_ROOT/.fetched.z" ]; then
  while IFS= read -r -d '' d; do
    [ -d "$d" ] && DIRS+=("$d")
  done < "$DEPS_ROOT/.fetched.z"
  pkcount=$(tr -cd '\0' < "$DEPS_ROOT/.fetched.z" | wc -c)
  echo "Loaded $pkcount Hackage packages."
fi

# Tagging
mapfile -t DIRS < <(printf '%s\n' "${DIRS[@]}" | awk 'NF' | sort -u)
echo "Tagging ${#DIRS[@]} directories..."
fast-tags -R -o "$TAGS_FILE" "${DIRS[@]}"
fast-tags -R -e -o "$ETAGS_FILE" "${DIRS[@]}"
echo "Done → $TAGS_FILE and $ETAGS_FILE"