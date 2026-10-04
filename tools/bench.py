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
import math
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
        # a mirror may answer with an HTML page or a cut file: try the next one
        for url in WILKIES_URLS:
            try:
                fetch(url, d / "wilkies.zip")
                with zipfile.ZipFile(d / "wilkies.zip") as z:
                    z.extractall(d)
                break
            except (OSError, zipfile.BadZipFile):
                continue
        if not list(d.glob("*.RED")):
            raise RuntimeError("the Wilkies benchmark could not be downloaded")
    return sorted(d.glob("*.RED"))


def koenigstuhl_ranking() -> list[tuple[int, str, str, str, float]]:
    """(rank, file, name, author, score) for the whole hill, from its recursive scores page."""
    d = BENCH / "koenigstuhl"
    page = d / "hill32_rec.html"
    if not page.exists():
        fetch(KOENIGSTUHL_RANKING, page)
    rows = parse_ranking(page)
    # a missing entry means a missing or cut download: fetch and extract again
    if not all((d / "HILL32" / f).exists() for _, f, _, _, _ in rows[:TOP]):
        fetch(KOENIGSTUHL_TAR, d / "94.tar.gz")
        # extraction filters exist from Python 3.11.4 on; the floor is 3.11
        safe = {"filter": "data"} if hasattr(tarfile, "data_filter") else {}
        with tarfile.open(d / "94.tar.gz") as t:
            t.extractall(d, **safe)
    return rows


def parse_ranking(page: Path) -> list[tuple[int, str, str, str, float]]:
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
    missing = [f.name for f in files if not f.exists()]
    if missing:
        print(f"  ({len(missing)} ranked warrior(s) missing from the archive: {', '.join(missing[:5])})", file=sys.stderr)
    return [f for f in files if f.exists()]


def battle(warrior: Path, opponent: Path, rounds: int, config: Path | None) -> tuple[int, int]:
    args = [str(PMARS), "-b", "-k", "-r", str(rounds), "-F", str(SEED)]
    if config:
        args[1:1] = ["-@", str(config)]
    # stdin closed: a warrior with ;break would otherwise stop pMARS in cdb, waiting for input
    try:
        out = subprocess.run(args + [str(warrior), str(opponent)], stdin=subprocess.DEVNULL,
                             capture_output=True, text=True, timeout=600)
    except subprocess.TimeoutExpired:
        raise RuntimeError(f"pmars timed out on {warrior.name} against {opponent.name}")
    # -k prints one "wins ties" line per warrior, ours first; some warriors also print a listing
    # (an ;assert's output, a debug directive), so the result is the first line of two numbers.
    results = [l.split() for l in out.stdout.splitlines() if re.fullmatch(r"\s*\d+\s+\d+\s*", l)]
    if not results:
        raise RuntimeError(f"pmars gave no result for {warrior.name} against {opponent.name}: {out.stderr.strip()[:200]}")
    return int(results[0][0]), int(results[0][1])


def results(warrior: Path, opponents: list[Path], rounds: int, config: Path | None) -> dict[str, float]:
    """Each opponent's score for the warrior. The warrior itself is no opponent (a hill entry scored
    against its own hill); an opponent pMARS cannot run here is left out, and said."""
    per, skipped = {}, []
    for o in opponents:
        if o.resolve() == warrior.resolve():
            continue
        try:
            w, t = battle(warrior, o, rounds, config)
        except RuntimeError:
            skipped.append(o.name)
            continue
        per[o.name] = (3 * w + t) * 100 / rounds
    if skipped:
        print(f"  ({warrior.name}: {len(skipped)} opponent(s) left out: {', '.join(skipped[:5])})", file=sys.stderr)
    if not per:
        raise RuntimeError(f"{warrior.name}: no opponent could be run (an ;assert for another hill?)")
    return per


def score(warrior: Path, opponents: list[Path], rounds: int, config: Path | None) -> tuple[float, int]:
    """The mean score, and over how many opponents."""
    per = results(warrior, opponents, rounds, config)
    return round(sum(per.values()) / len(per), 1), len(per)


def recursive(per: dict[str, float], ranking: list[tuple[int, str, str, str, float]]) -> float:
    """Koenigstuhl's recursive score, estimated: the mean against everyone with weight 1, against the
    top half with weight 1/2, the top third with 1/3, ..., while the group has more than 50 (its page,
    koenigstuhl.html). Its own iterations re-rank the hill between steps; this uses the published
    order, which is where they ended."""
    names = [f for _, f, _, _, _ in ranking]
    n, num, den, k = len(names), 0.0, 0.0, 1
    while k == 1 or n / k > 50:
        top = [per[f] for f in names[:math.ceil(n / k)] if f in per]
        if top:
            num += (sum(top) / len(top)) / k
            den += 1 / k
        k += 1
    return round(num / den, 1)


def results_of(path: Path, whole: list[Path]) -> dict[str, float]:
    return results(path, whole, ROUNDS["hill"], None)


COMPILER = ROOT / "_build" / "default" / "execs" / "run_compile.exe"


def build_compiler() -> None:
    # Built once, then run directly: two dune processes at once fight over _build/.lock, and two
    # benchmarks may run side by side.
    subprocess.run(["opam", "exec", "--switch=.", "--", "dune", "build", "execs/run_compile.exe"],
                   cwd=ROOT, check=True, capture_output=True)


def compile_red(src: Path) -> Path:
    out = BENCH / "red" / (src.stem + ".red")
    out.parent.mkdir(parents=True, exist_ok=True)
    if not COMPILER.exists():
        build_compiler()
    run = subprocess.run([str(COMPILER), "--warn=none", str(src)], cwd=ROOT, capture_output=True, text=True)
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
    if any(a.endswith(".src") for a in argv) or not [a for a in argv if not a.startswith("--")]:
        build_compiler()
    sets = {"wilkies": (wilkies(), ROUNDS["wilkies"], CONFIG_94B),
            "koenigstuhl": (koenigstuhl(), ROUNDS["koenigstuhl"], None)}
    results: dict[str, dict[str, float]] = {}
    print(f"{'warrior':28} {'wilkies':>8} {'koenigstuhl top 20':>19}" + ("   whole hill" if "--hill" in argv else ""))
    ranking = koenigstuhl_ranking() if "--hill" in argv else []
    whole = koenigstuhl(None) if "--hill" in argv else []
    for label, path in warriors(argv):
        results[label] = {}
        for name, (opps, rounds, cfg) in sets.items():
            results[label][name], results[label][name + "_opponents"] = score(path, opps, rounds, cfg)
        line = f"{label:28} {results[label]['wilkies']:8.1f} {results[label]['koenigstuhl']:19.1f}"
        if whole:
            # Placed by Koenigstuhl's own kind of score; a plain mean would place a weak warrior far
            # too high (measured: #845's plain mean is 115.1 against its published 83.2).
            per = results_of(path, whole)
            r = recursive(per, ranking)
            hill = BENCH / "koenigstuhl" / "HILL32"
            place = 1 + sum(1 for row in ranking if row[4] > r and (hill / row[1]).resolve() != path.resolve())
            results[label]["hill_mean"] = round(sum(per.values()) / len(per), 1)
            results[label]["hill_recursive"] = r
            line += f"   mean {results[label]['hill_mean']:6.1f}, recursive {r:6.1f} -> #{place}"
        print(line, flush=True)
    if "--update" in argv:
        BASELINE.write_text(json.dumps(results, indent=2, sort_keys=True) + "\n")
        print(f"baseline written: {BASELINE.relative_to(ROOT)}")
        return 0
    if not BASELINE.exists():
        return 0
    base = json.loads(BASELINE.read_text())
    # A score counts as compared only over the same opponents; a warrior on one side only is said.
    problems = [f"DROP {w} {s}: {results[w][s]} < {base[w][s]} - {MARGIN}" for w in results if w in base
                for s in ("wilkies", "koenigstuhl") if s in base[w] and results[w][s] < base[w][s] - MARGIN]
    problems += [f"OPPONENTS {w} {s}: {results[w][s]} now, {base[w][s]} in the baseline" for w in results if w in base
                 for s in ("wilkies_opponents", "koenigstuhl_opponents") if base[w].get(s, results[w][s]) != results[w][s]]
    if not [a for a in argv if not a.startswith("--")]:
        problems += [f"MISSING {w}: in the baseline, not measured" for w in base if w not in results]
    for p in problems:
        print(p)
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
