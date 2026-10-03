---
name: verify
description: Run this repository's gate and report honestly what passed. Use before claiming a change works, after touching anything in src/ (code generation above all), before committing, and whenever asked to "check", "verify", "run the tests", "run the gate" or "is this ready".
allowed-tools: Bash, Read
---

# Verify

A compiler bug here rarely fails loudly: the warrior assembles, matches its golden and still does
the wrong thing in the core. The gate has a text half (goldens), an assembly half (pMARS `-A`) and
a behaviour half (cdb probes); a report says which halves ran.

## The gate

```sh
make check-tools      # tools/audit.py, tools/behave.py, bundle.py verify + ids — Python 3.11+ and cc only
make check-ocaml      # dune build + execs/run_test.exe (needs the opam switch; execute group Linux x86-64 only)
make check            # both
```

Without `dune` on PATH, `make check-ocaml` cannot run: say so in the report. CI runs both halves.

## If code generation changed (anything in src/compile.ml, src/util.ml, src/analyse.ml, src/lib.ml)

1. `make tests F=compare`. A golden that now differs is a **behaviour question first**: is the new
   output a fix, an equivalent rewrite, or a regression? Never update EXPECTED without stating which
   (d-7d2612-6a1527).
2. `python3 tools/behave.py`. A `FIXED?` line means a recorded bug now passes: remove its
   `known-failing:` line, move the roadmap item to Done, and say so — in the same change.
3. For the construct you touched, look at the emitted redcode (`make compile src=...`) and step it
   (`.claude/skills/run-warrior/SKILL.md`). Add a `.beh` spec if none covers it.

## Reporting

- Quote the summary lines (`audit: …`, `behave: …`, alcotest's totals), never paraphrase them.
- Name what did not run and why (no opam switch; not Linux x86-64 for `execute`).
- Known-failing items are listed as such, never presented as new failures or as passes.
- "Verified" means a command ran and its output is quoted.
