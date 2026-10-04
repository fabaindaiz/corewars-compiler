#!/usr/bin/env python3
"""Score warriors against published benchmarks: the benchmark as a regression signal.

    python3 tools/bench.py                 score the archetypes (RED and hand-written), compare with
                                           tools/bench_baseline.json
    python3 tools/bench.py --update        the same, then write the scores as the new baseline
    python3 tools/bench.py --hill          also place each warrior on the whole Koenigstuhl 94nop
                                           hill (about a thousand opponents; slow)
    python3 tools/bench.py FILE.red|.src   score these warriors instead of the archetypes

Opponent sets, downloaded into _build/bench/ on first use and never committed (no licence
statement covers redistribution, d-7d2612-472634's rule for third-party code):
    wilkies     the Wilkies benchmark, 12 warriors from koth.org, scored under the 94b settings
    koenigstuhl the top 20 of Koenigstuhl's 94nop archive hill (asdflkj.net), scored as that hill
                scores: pMARS's defaults (core 8000, 80000 cycles, length 100)

The score of a set is the mean over its warriors of (3 * wins + ties) * 100 / rounds, the formula
the hills use: 300 beats everything, 100 ties everything. Rounds use a fixed seed (-F), so a score
repeats exactly; the baseline holds scores only, never an opponent's code.

Needs network on first use, the host pMARS (tools/pmars-host.sh) and, for .src files, the opam
switch. Not part of `make check` (d-7d2612-1d4491): it depends on external sites.
"""
from __future__ import annotations

import json
import re
import subprocess
import sys
import tarfile
import urllib.request
import zipfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
BENCH = ROOT / "_build" / "bench"
PMARS = ROOT / "_build" / "pmars-host" / "pmars"
CONFIG_94B = ROOT / "pmars" / "config" / "94b.opt"
BASELINE = ROOT / "tools" / "bench_baseline.json"
ARCHETYPES = ROOT / "archetypes"

WILKIES_URLS = ["http://www.koth.org/wilkies/bench.zip", "http://www.koth.org/wilkies/wilkies.zip"]
KOENIGSTUHL_TAR = "https://asdflkj.net/COREWAR/94/94.tar.gz"
KOENIGSTUHL_RANKING = "https://asdflkj.net/COREWAR/94/hill32_rec.html"
TOP = 20
ROUNDS = {"wilkies": 500, "koenigstuhl": 200, "hill": 100}
SEED = 4000
# Two runs of one warrior differed by up to 4 points at 500 rounds with a free seed; with a fixed
# seed a score repeats, and a drop beyond this margin means the warrior changed.
MARGIN = 3.0


def fetch(url: str, dest: Path) -> None:
    dest.parent.mkdir(parents=True, exist_ok=True)
    with urllib.request.urlopen(url, timeout=60) as r:
        dest.write_bytes(r.read())


def wilkies() -> list[Path]:
    d = BENCH / "wilkies"
    if not list(d.glob("*.RED")):
        for url in WILKIES_URLS:
            try:
                fetch(url, d / "wilkies.zip")
                break
            except OSError:
                continue
        with zipfile.ZipFile(d / "wilkies.zip") as z:
            z.extractall(d)
    return sorted(d.glob("*.RED"))


def koenigstuhl_ranking() -> list[tuple[int, str, str, str, float]]:
    """(rank, file, name, author, score) for the whole hill, from its recursive scores page."""
    d = BENCH / "koenigstuhl"
    page = d / "hill32_rec.html"
    if not page.exists():
        fetch(KOENIGSTUHL_RANKING, page)
    if not (d / "HILL32").is_dir():
        fetch(KOENIGSTUHL_TAR, d / "94.tar.gz")
        with tarfile.open(d / "94.tar.gz") as t:
            t.extractall(d, filter="data")
    rows = []
    for line in page.read_text(encoding="latin-1").splitlines():
        m = re.match(r'\s*(\d+)\s+<a href="HILL32/([^"]+)">([^<]*)</a>\s+(.*?)\s{2,}([\d.]+)', line)
        if m:
            rows.append((int(m[1]), m[2], m[3].strip(), m[4].strip(), float(m[5])))
    return rows


def koenigstuhl(top: int | None = TOP) -> list[Path]:
    d = BENCH / "koenigstuhl" / "HILL32"
    rows = koenigstuhl_ranking()
    files = [d / f for _, f, _, _, _ in (rows[:top] if top else rows)]
    return [f for f in files if f.exists()]


def battle(warrior: Path, opponent: Path, rounds: int, config: Path | None) -> tuple[int, int]:
    args = [str(PMARS), "-b", "-k", "-r", str(rounds), "-F", str(SEED)]
    if config:
        args[1:1] = ["-@", str(config)]
    out = subprocess.run(args + [str(warrior), str(opponent)], capture_output=True, text=True, timeout=600)
    # -k prints one "wins ties" line per warrior, ours first; some warriors also print a listing
    # (an ;assert's output, a debug directive), so the result is the first line of two numbers.
    results = [l.split() for l in out.stdout.splitlines() if re.fullmatch(r"\s*\d+\s+\d+\s*", l)]
    if not results:
        raise RuntimeError(f"pmars gave no result for {warrior.name} against {opponent.name}: {out.stderr.strip()[:200]}")
    return int(results[0][0]), int(results[0][1])


def score(warrior: Path, opponents: list[Path], rounds: int, config: Path | None) -> float:
    """The mean over the opponents pMARS can run; one it cannot assemble here is left out, and said."""
    total, counted, skipped = 0.0, 0, []
    for o in opponents:
        try:
            w, t = battle(warrior, o, rounds, config)
        except RuntimeError:
            skipped.append(o.name)
            continue
        total += (3 * w + t) * 100 / rounds
        counted += 1
    if skipped:
        print(f"  ({warrior.name}: {len(skipped)} opponent(s) left out: {', '.join(skipped[:5])})", file=sys.stderr)
    return round(total / counted, 1)


def compile_red(src: Path) -> Path:
    out = BENCH / "red" / (src.stem + ".red")
    out.parent.mkdir(parents=True, exist_ok=True)
    run = subprocess.run(["opam", "exec", "--switch=.", "--", "dune", "exec", "--no-print-directory",
                          "execs/run_compile.exe", "--", "--warn=none", str(src)],
                         cwd=ROOT, capture_output=True, text=True)
    if run.returncode != 0:
        raise RuntimeError(f"{src}: {run.stderr.strip()}")
    out.write_text(run.stdout)
    return out


def warriors(argv: list[str]) -> list[tuple[str, Path]]:
    files = [Path(a) for a in argv if not a.startswith("--")]
    if not files:
        srcs = sorted(ARCHETYPES.glob("*.src"))
        files = srcs + [ARCHETYPES / (s.stem + ".red") for s in srcs if (ARCHETYPES / (s.stem + ".red")).exists()]
    named = []
    for f in files:
        label = f"{f.stem} ({'RED' if f.suffix == '.src' else 'hand'})"
        named.append((label, compile_red(f) if f.suffix == ".src" else f))
    return named


def main(argv: list[str]) -> int:
    if not PMARS.exists():
        subprocess.run([str(ROOT / "tools" / "pmars-host.sh")], check=True)
    sets = {"wilkies": (wilkies(), ROUNDS["wilkies"], CONFIG_94B),
            "koenigstuhl": (koenigstuhl(), ROUNDS["koenigstuhl"], None)}
    results: dict[str, dict[str, float]] = {}
    print(f"{'warrior':28} {'wilkies':>8} {'koenigstuhl top 20':>19}" + ("   hill place" if "--hill" in argv else ""))
    ranking = koenigstuhl_ranking() if "--hill" in argv else []
    whole = koenigstuhl(None) if "--hill" in argv else []
    for label, path in warriors(argv):
        results[label] = {name: score(path, opps, rounds, cfg) for name, (opps, rounds, cfg) in sets.items()}
        line = f"{label:28} {results[label]['wilkies']:8.1f} {results[label]['koenigstuhl']:19.1f}"
        if whole:
            # The hill's own score is the mean against every entry; where this one would sit.
            s = score(path, whole, ROUNDS["hill"], None)
            place = 1 + sum(1 for r in ranking if r[4] > s)
            results[label]["hill"] = s
            line += f"   {s:6.1f} -> #{place} of {len(ranking) + 1}"
        print(line, flush=True)
    if "--update" in argv:
        BASELINE.write_text(json.dumps(results, indent=2, sort_keys=True) + "\n")
        print(f"baseline written: {BASELINE.relative_to(ROOT)}")
        return 0
    if not BASELINE.exists():
        return 0
    base = json.loads(BASELINE.read_text())
    drops = [f"{w} {s}: {results[w][s]} < {base[w][s]} - {MARGIN}" for w in results if w in base
             for s in ("wilkies", "koenigstuhl") if s in base[w] and results[w][s] < base[w][s] - MARGIN]
    for d in drops:
        print(f"DROP {d}")
    return 1 if drops else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
