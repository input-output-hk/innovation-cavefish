#!/usr/bin/env python3
import argparse, json, re, sys, os
from pathlib import Path

TAG_GX   = 900010
TAG_GY   = 900011
TAG_BASE = 900200
TAG_C0   = 900300  # 64 bytes: 900300..900363 inclusive

# ============ ANSI COLORS ============
class Ansi:
    R = "\x1b[0m"
    B = "\x1b[1m"
    DIM = "\x1b[2m"
    IT = "\x1b[3m"
    U = "\x1b[4m"
    RED = "\x1b[31m"
    GRN = "\x1b[32m"
    YLW = "\x1b[33m"
    BLU = "\x1b[34m"
    MAG = "\x1b[35m"
    CYN = "\x1b[36m"
    WHT = "\x1b[37m"
    # convenience
    OK = GRN + B
    BAD = RED + B
    TAG = YLW + B
    SEC = CYN + B
    LAB = WHT + B

def colorize(enable: bool, s: str) -> str:
    return s if enable else re.sub(r"\x1b\[[0-9;]*m", "", s)

# ============ PARSING ============
def parse_lines(lines):
    nums = []
    for ln in lines:
        ln = ln.strip()
        if re.match(r'^-?\d+$', ln):
            nums.append(int(ln))

    res = {
        "P1": {"g_rho": {"x": None, "y": None}},
        "P2": {"limbs": []},
        "P3": {"challenge_bytes": []},
        "_raw": {"unknown_tags": []}
    }

    i = 0
    n = len(nums)
    while i < n:
        v = nums[i]

        # P1: g^rho.x / g^rho.y
        if v == TAG_GX and i + 1 < n:
            res["P1"]["g_rho"]["x"] = str(nums[i + 1])
            i += 2
            continue
        if v == TAG_GY and i + 1 < n:
            res["P1"]["g_rho"]["y"] = str(nums[i + 1])
            i += 2
            continue

        # --- FIXED: Check P3 BEFORE P2 ---
        if TAG_C0 <= v <= TAG_C0 + 63:
            j = v - TAG_C0
            if i + 1 < n:
                b = nums[i + 1] % 256
                while len(res["P3"]["challenge_bytes"]) <= j:
                    res["P3"]["challenge_bytes"].append(None)
                res["P3"]["challenge_bytes"][j] = b
                i += 2
                continue
            else:
                res["_raw"]["unknown_tags"].append(v)
                i += 1
                continue

        # P2 blocks: 900200+i followed by 5 values (μ̂, PRF, masked, Cmsg, Δ)
        if v >= TAG_BASE and v < TAG_BASE + 100:
            limb_idx = v - TAG_BASE
            if i + 5 < n:
                limb = {
                    "index": limb_idx,
                    "mu_hat": str(nums[i + 1]),
                    "prf": str(nums[i + 2]),
                    "masked": str(nums[i + 3]),
                    "Cmsg": str(nums[i + 4]),
                    "delta": str(nums[i + 5]),
                }
                res["P2"]["limbs"].append(limb)
                i += 6
                continue
            else:
                res["_raw"]["unknown_tags"].append(v)
                i += 1
                continue

        res["_raw"]["unknown_tags"].append(v)
        i += 1

    res["P2"]["limbs"].sort(key=lambda x: x["index"])
    return res

def pretty_text(rep, use_color=True):
    A = Ansi
    c = lambda s: colorize(use_color, s)

    lines = []
    lines.append(c(f"{A.SEC}──────────────── WBPS Witness Report ────────────────{A.R}"))

    # -------- Property Summary --------
    gx = rep["P1"]["g_rho"]["x"]
    gy = rep["P1"]["g_rho"]["y"]
    p1_ok = (gx is not None and gy is not None)

    limbs = rep["P2"]["limbs"]
    p2_total = len(limbs)
    p2_ok_count = sum(1 for L in limbs if str(L["delta"]) == "0")
    p2_bad_count = p2_total - p2_ok_count
    p2_ok = (p2_bad_count == 0 and p2_total > 0)

    ch = rep["P3"]["challenge_bytes"]
    p3_complete = (len(ch) == 64 and all(x is not None for x in ch))

    badge = lambda ok: c(f"{A.OK}✓{A.R}") if ok else c(f"{A.BAD}✗{A.R}")
    lines.append(c(
        f"{A.LAB}Property Summary:{A.R}  "
        f"P1 {badge(p1_ok)}  "
        f"P2 {badge(p2_ok)} ({p2_ok_count}/{p2_total} limbs)  "
        f"P3 {badge(p3_complete)}"
    ))
    lines.append("")

    # ----------------- P0 -----------------
    lines.append(c(f"{A.SEC}P0 – Message Reconstruction{A.R}"))
    lines.append(c(f"  {A.LAB}Builds:{A.R} the message {A.B}μ{A.R} by overlaying the private window onto the public bits"))
    lines.append(c(f"  {A.LAB}Formula:{A.R} {A.B}μ = Overlay(m_pub, m_priv){A.R}"))
    lines.append("")

    # ----------------- P1 -----------------
    lines.append(c(f"{A.SEC}P1 – Commitment Scalar Correctness{A.R}"))
    lines.append(c(f"  {A.LAB}Checks:{A.R}  {A.B}ρ ∈ [0, 2^251){A.R} via Num2Bits range constraint"))
    lines.append(c(f"  {A.LAB}Computes:{A.R} {A.B}ek^ρ{A.R} and {A.B}g^ρ{A.R}"))
    lines.append(c(f"  {A.LAB}Asserts:{A.R}  {A.B}C₀ == g^ρ{A.R}"))
    lines.append(c(f"  {A.LAB}Meaning:{A.R} binds the commitment to the nonce scalar {A.B}ρ{A.R}"))
    lines.append(f"  (g^ρ).x = {c(A.TAG)}{gx if gx else '—'}{c(A.R)}")
    lines.append(f"  (g^ρ).y = {c(A.TAG)}{gy if gy else '—'}{c(A.R)}")
    lines.append("")

    # ----------------- P2 -----------------
    lines.append(c(f"{A.SEC}P2 – Ciphertext Binding Consistency{A.R}"))
    lines.append(c(f"  {A.LAB}Packs:{A.R}  the message {A.B}μ{A.R} into 254-bit limbs {A.B}μ̂[i]{A.R}"))
    lines.append(c(f"  {A.LAB}Generates:{A.R} a Poseidon PRF stream {A.B}PRF[i]{A.R} from {A.B}ek^ρ{A.R}"))
    lines.append(c(f"  {A.LAB}Asserts:{A.R}  for every limb i: {A.B}Cmsg[i] == μ̂[i] + PRF[i]{A.R} (mod p)"))
    lines.append(c(f"  {A.LAB}Meaning:{A.R} ensures the ciphertext encodes {A.B}μ{A.R} under the same key and nonce"))
    lines.append(f"  Summary: {badge(p2_ok)} {p2_ok_count} ok, {badge(False)} {p2_bad_count} mismatch")

    if p2_bad_count > 0:
        lines.append(c(f"  {A.BAD}Details (first {min(5,p2_bad_count)} mismatches):{A.R}"))
        shown = 0
        for L in limbs:
            if str(L["delta"]) != "0":
                i = L["index"]; d = L["delta"]
                hdr = c(f"{A.B}{A.BLU}  Limb {i}:{A.R} {badge(False)}")
                lines.append(hdr)
                lines.append(f"    μ̂[{i}]      = {L['mu_hat']}")
                lines.append(f"    PRF[{i}]     = {L['prf']}")
                lines.append(f"    μ̂[{i}]+PRF  = {L['masked']}")
                lines.append(f"    Cmsg[{i}]    = {L['Cmsg']}")
                lines.append(c(f"    Δ[{i}]       = {A.BAD}{d}{A.R}"))
                shown += 1
                if shown >= 5:
                    if p2_bad_count > 5:
                        lines.append(c(f"    {A.DIM}… and {p2_bad_count-5} more mismatches{A.R}"))
                    break
        lines.append("")

    # Optional detail printing (for small circuits)
    if p2_total and p2_total <= 12:
        for L in limbs:
            i = L["index"]; d = str(L["delta"])
            status = badge(d == "0")
            hdr = c(f"{A.B}{A.BLU}Limb {i}:{A.R} {status}")
            lines.append(f"  {hdr}")
            lines.append(f"    μ̂[{i}]      = {L['mu_hat']}")
            lines.append(f"    PRF[{i}]     = {L['prf']}")
            lines.append(f"    μ̂[{i}]+PRF  = {L['masked']}")
            lines.append(f"    Cmsg[{i}]    = {L['Cmsg']}")
            if d == "0":
                lines.append(c(f"    Δ[{i}]       = {A.OK}{d}{A.R}"))
            else:
                lines.append(c(f"    Δ[{i}]       = {A.BAD}{d}{A.R}"))
        lines.append("")

    # ----------------- P3 -----------------
    lines.append(c(f"{A.SEC}P3 – Transcript Integrity{A.R}"))
    lines.append(c(f"  {A.LAB}Rebuilds:{A.R}  the Schnorr transcript digest"))
    lines.append(c(f"  {A.LAB}Formula:{A.R}   {A.B}c = SHA-512(R || X || μ_bits){A.R}"))
    lines.append(c(f"  {A.LAB}Asserts:{A.R}   provided {A.B}challenge == c{A.R}"))
    lines.append(c(f"  {A.LAB}Meaning:{A.R}   binds R, X, and μ into a unique proof transcript"))

    if p3_complete:
        hexstr = ''.join(f'{b:02x}' for b in ch)
        lines.append(f"  bytes[0..63] = {', '.join(map(str, ch))}")
        lines.append(f"  hex          = {c(A.MAG + A.B)}{hexstr}{c(A.R)}")
    else:
        missing = [str(i) for i in range(64) if i >= len(ch) or ch[i] is None]
        lines.append(c(f"  {A.BAD}Challenge bytes incomplete in logs.{A.R}"))
        if missing:
            lines.append(c(f"  Missing indices: {A.DIM}{', '.join(missing)}{A.R}"))

    # ----------------- Unknown tags -----------------
    if rep["_raw"]["unknown_tags"]:
        lines.append("")
        lines.append(c(f"{A.DIM}Note: Unrecognized numeric lines (not tagged):{A.R}"))
        lines.append(c(f"{A.DIM}  {', '.join(map(str, rep['_raw']['unknown_tags']))}{A.R}"))
    lines.append(c(f"{A.SEC}─────────────────────────────────────────────────────{A.R}"))
    return '\n'.join(lines)

# ============ MAIN ============
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("logfile")
    ap.add_argument("--txt", help="write pretty text report")
    ap.add_argument("--json", help="write JSON report")
    ap.add_argument("--color", action="store_true", help="force color output")
    ap.add_argument("--no-color", action="store_true", help="disable color output")
    args = ap.parse_args()

    # auto color
    auto_color = sys.stdout.isatty() and not os.environ.get("NO_COLOR")
    use_color = args.color or (auto_color and not args.no_color)

    p = Path(args.logfile)
    if not p.exists():
        print(f"no log file: {p}", file=sys.stderr)
        sys.exit(0)

    lines = p.read_text(encoding="utf-8", errors="ignore").splitlines()
    rep = parse_lines(lines)

    if args.txt:
        Path(args.txt).write_text(pretty_text(rep, use_color=False), encoding="utf-8")
    if args.json:
        Path(args.json).write_text(json.dumps(rep, indent=2), encoding="utf-8")

    print(pretty_text(rep, use_color=use_color))

if __name__ == "__main__":
    main()