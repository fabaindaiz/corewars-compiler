# Agent changelog

One entry per change, newest first, written in the same change. Parallel sessions cannot see each
other; this file is how one warns the next. Write what went wrong and what was left undone, not only
what worked. Each entry's id comes from `python3 .agents/tools/bundle.py id s "<title>"`. The format
reference is at the end of this file: insert new entries directly below this paragraph.

## 2026-10-02 · s-7d2612-0a037e — Initialise the agent-guides bundle and bootstrap the repository

**What.** Took the agent-guides bundle 0.0.25 into `.agents/` (from the template's release export)
and minted carrier `r-7d2612`. Wrote the instruction system: `AGENTS.md` (canonical) and
`CLAUDE.md` (imports it); `docs/decisions.md`, `docs/roadmap.md`, `docs/references.md`,
`docs/architecture.md`, `docs/semantics.md`; skills `verify`, `run-warrior`, `troubleshoot-redcode`,
`state-review`; `.claude/settings.json`. Made the rules executable: `tools/audit.py` (11 checks),
`tools/behave.py` (behaviour specs in `behtests/`, cdb probes), `tools/pmars-host.sh` (pMARS for
the host from the vendored zip), `make check` / `check-tools` / `check-ocaml`, and
`.github/workflows/ci.yml` (replacing the template's `agents.yml`). Recorded seven compiler defects
without fixing them (d-7d2612-c5bb3c): four with characterization goldens in `bbctests/known-bugs/`
and known-failing specs, one with a known-failing audit check, two as roadmap items. Corrected the
stale statements in `README.md`, `REFERENCE.md`, `LANGUAGE.md` and `commands.md`.
**Areas.** `.agents/`, `.claude/`, `.github/`, `.githooks/`, `docs/`, `tools/`, `behtests/`,
`bbctests/known-bugs/`, `Makefile`, `.gitignore`, `.editorconfig`, root documents.
**Why.** The user asked to initialise `.agents` and to research Core War, pMARS, compilers in OCaml,
programming-language theory, the community, and ways to run and observe warriors, as part of it.
**Architecture.** ✅ Complies: no compiler source changed; the existing documents were extended, not
renamed.
**What went wrong on the way.** The first reading of the `execute` suite assumed it ran warriors;
pMARS's source shows `-A` assembles only, which turned the main guardrail around. A research claim
(comment text becoming a pMARS directive) did not survive measurement: RED's `com` prepends a space,
so it is recorded as closed by measurement (i-7d2612-476e03) instead of as a bug. The first draft of
the audit ran its checks at import time, so the enforcer check could not see the full registry;
restructured before use. `dune` is not installed here, so the compiler was checked with a scratch
`ocamlc` build of `src/` against a stand-in for `CCSexp` (outside the repository); it reproduced all
ten example goldens exactly, and it produced the four known-bug goldens.
Second pass, same session: added *Engineering standards* and the *what changed → what must move*
table to `AGENTS.md`; prefixed the `permissions.deny` path rules with `./`; enabled the commit hook
(`git config core.hooksPath .githooks`) and listed this repository in this machine's carrier manifest
(outside the repository; the home registers it at its next meta-session).
**Candidates for the bundle** (hold with none of this repository's nouns; the next harvest writes
them as proposals): (1) a structural audit that resolves documented paths must be run once on a
fresh clone, because generated directories named in the documents exist only on the machine that
wrote them; (2) editing files by script through a text API can normalise line endings and turn a
small change into a whole-file diff: compare `git diff --stat` with the intended size before
reporting; (3) recording a known bug as a check that must fail, and that fails the gate once it
passes, keeps "not fixed yet" visible without blocking the gate.
**What was left undone.** Every roadmap item. The OCaml half of the gate never ran on this machine.
`TUTORIAL.md` is still a title.
**Not verified.** The CI `ocaml` job (setup-ocaml inputs, installing BBCStepTester at 2cb3669 with
`dune install`, the vendored pmars on `ubuntu-latest` with `libx11-6`) and the `tools` job's pMARS
build with gcc: both wait for the first CI run. That alcotest's `test <name>` filter selects the
`parse` and `compare` groups as `make check-ocaml` assumes on non-Linux hosts.
**Measured.** `tools/behave.py`: 7 specs, 3 pass, 4 known-failing, each failing for the stated
reason (e.g. `do-while (GT x y)` with x = y still running after 10 instructions; prog8 alive after
273 instructions, dead after 274). Three mutated specs and one "fixed" known-failing spec each made
the runner exit 1. Every one of the 11 audit checks was seen red on a planted violation in a scratch
copy. On the documents as they were before this change, the audit reported 6 violations
(`LANGUAGE.md` missed `com`, `STP`, `LDP`, `AB`, `BA`, `B`) plus the known-failing
`single-error-type` (4 declarations); reading found 7 more stale statements in `REFERENCE.md`,
`README.md` and `commands.md` (wrong target names, a nonexistent `bin/tests.exe`, `Dev.` for
`Cored.`, 3 dead links). The first audit draft also failed on a fresh clone (`_build/` paths);
caught by running it on a copy without `_build/`. pMARS 0.9.4 builds on macOS arm64 with
`-Dround=pm_round`.

---

## Format

```markdown
## YYYY-MM-DD · s-7d2612-<content6> — <one-line title>
**What.** What changed, concretely.
**Areas.** Files or folders.
**Why.** The reason, including the request that prompted it.
**Architecture.** ✅ Complies · ⚠️ Deviation · REVIEW — and why.
**What went wrong on the way.** What the first attempt got wrong, and what caught it.
**What was left undone.** Debt created or walked past, named.
**Deviation from the plan.** Where the result departs from what was approved. Omit if none.
**Not verified.** What could not be checked here, and where the question waits. Omit if none.
**Measured.** The number, if a claim was made.
```
