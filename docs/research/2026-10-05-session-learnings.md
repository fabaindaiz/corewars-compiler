# Phases 7 and 8, the manual and the diagnostics: findings, pending items, learnings

Date: 2026-10-05 (closed in s-7d2612-61974e). One long working session, logged as s-7d2612-11efe5
(phase 7), s-7d2612-dec815 (phase 8), s-7d2612-5e5abb (the manual) and s-7d2612-14528c (the
diagnostics). This note gathers what they found, what they left, and what to do differently;
the details are in each changelog entry and in the notes linked below.

## What was built

| Work | Where | Decision |
|---|---|---|
| the phase-6 review's minors; the host pMARS trap found and patched | `src/parse.ml`, `tools/pmars-host.sh`, `tools/pmars-trap/` | d-7d2612-22c819, d-7d2612-b153c1 |
| a template's let shadows a parameter; the template naming gaps | `src/parse.ml` | d-7d2612-e1c41a |
| `(include "path")` and six snippets | `snippets/`, `bbctests/snippets/` | d-7d2612-c2df7c |
| Mice and a Silk-style paper, each equal to its hand-written twin | `archetypes/` | — |
| unary skip fusion closed by measurement | `docs/roadmap.md` | — |
| the user manual, self-verifying, and a page built from it | `docs/manual/`, `tools/manual_page.py` | d-7d2612-c06299 |
| the `write-warrior` and `present-red` skills | `.claude/skills/` | — |
| diagnostics a newcomer meets (self-hit warning, located expectations, named pointers, `ADD.F` steps) | `src/metrics.ml`, `src/warnings.ml`, `src/driver.ml` | d-7d2612-30fd8f, d-7d2612-fb3ce1 |

## Findings

- **Fresh reviews keep finding what the author missed.** Of five fresh reviews in this session, four
  found important issues (the review of the phase-6 minors found none): capture by a template's `for`, a
  pointer's value not counted by the peephole, include paths spelled two ways, a published
  warrior's constants reproduced from memory, a self-hit warning blind to the cookbook's own
  layout, and two measurements taken on the wrong warrior. A newcomer test of the manual found 11
  confusions and 8 mismatches no test could see.
- **The compiler costs nothing on the archetypes.** Twelve archetypes compile to their
  hand-written code (two scanners to a different layout of the same instructions) and score the
  same; the distance to the top of a hill is strategy (`docs/research/2026-10-04-benchmark.md`).
- **Papers win among the simple forms.** Measured on the compiled RED warriors, papers beat the
  stone and every scanner here; the stone beats the scanner (`docs/manual/03-cookbook.md`).
- **A self-hit can be predicted exactly.** The step-3 dwarf's first bomb on its own code is
  computed at iteration 2666 (7998 cycles); pMARS kills it at instruction 8000. Among the RED
  archetypes run alone, only the quickscan dies (15416; predicted 15396 cycles).
- **A host pMARS overflow** in `sim.c` trapped on battles against warriors carrying `;break`; the
  build patches it, and the battles it dropped now run with the same scores as predicted.

## Pending, all in `docs/roadmap.md`

- Strategy, not language: a quickscan handing over to a paper or a stone, a tuned Silk, a scanner
  fast enough to beat papers.
- A self-hit at the first position is reported one iteration early (i-7d2612-1b199a's note).
- The vendored Linux pMARS keeps the `sim.c` overflow (unreachable from the `execute` suite, which
  only assembles); pMARS 0.9.5 (i-7d2612-494e75).
- The published page is rebuilt and republished by hand after a manual change.
- Three frictions now recorded for the first time (below).

## Learnings, and the rule each became

1. **Measure the thing the text claims, never a stand-in.** The self-hit calibration and the
   what-beats-what table were first measured on `archetypes/*.red`, the hand-written twins, while
   the text spoke of the RED warriors; the scanners' layouts differ, and three numbers and one
   death were wrong (i-7d2612-861c83; `write-warrior`, `present-red`).
2. **A battle quoted without a seed does not repeat.** Fix `-F` in every quoted battle
   (`present-red`).
3. **Calibrate a static rule against runs, then let a reviewer try to break it.** The self-hit rule
   was right on its five calibration warriors and still wrong on two layouts a reviewer built in
   minutes (a pointer in the loop's `JMP`, a pointer rewritten before its loop).
4. **A warrior written from memory reproduces its data.** Choose constants afresh; the commit that
   held the copied ones was local and was replaced before it reached the history.
5. **Generated text must be paired the way the test pairs it.** A regex that spanned blocks put
   outputs under the wrong programs; the self-verifying test caught it on its first run.
6. **Hand counts are wrong before they run** (now six sessions, i-7d2612-340f22): a column was
   counted 31 for 30 again; let the run say it.
7. **A test that expects no output from a compiler that warns fails for the wrong reason** (two
   sessions, i-7d2612-8e76c5): assert the absence of `error:`, not an empty stream.
8. **The user's shell is fish**: loops written for bash fail silently (i-7d2612-e4d74d); run them
   under `bash -c`.
9. **Text-mode edits turn CRLF files into LF** (again in s-7d2612-11efe5; the audit caught it):
   edit CRLF files as bytes.
