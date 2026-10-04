---
name: troubleshoot-redcode
description: Diagnose a compiled warrior that misbehaves, a RED program the compiler rejects, or pMARS output that looks wrong - loops that never end, a variable that reads the wrong value, a jump that lands on DAT, "Undefined label", "redefinition of label", exit codes 1/2/3/126, a compile "error:", or a hang. Use when a symptom is reported, before changing the compiler.
allowed-tools: Bash, Read
---

# Troubleshoot redcode

First decide **where** the fault is: the RED program, the compiler, or pMARS's settings. Reproduce
on the nearest thing you can run (`.claude/skills/run-warrior/SKILL.md`) before reasoning about it.

## Symptom → known cause

| Symptom | Known cause | Record |
|---|---|---|
| `SLT`/`ADD` on two plain references acts on both fields | fallback modifier `.I` instead of the ICWS'94 default | i-7d2612-96f7b1 |
| a jump to a user label lands on a data cell; pMARS warns "redefinition of label" | user label named like a generated one (`LET1`, `IF3`, …); pMARS keeps the first | i-7d2612-425c66 |
| pMARS "Undefined label 'LETn'" | the `let` variable has no `(store x)` | i-7d2612-425c66 |
| `error: ...` from `run_compile.exe`, exit 1 | a compile error in the RED program (the message says which form); the compiler still has four `CTError` types, caught by `Cored.Driver` | i-7d2612-888db5, d-7d2612-8bba52 |
| pMARS hangs while assembling | a line of 256+ characters (a long label) | i-7d2612-174acf |
| `(LT -1 3)` is false | not a bug: values are unsigned mod CORESIZE (`-1` = 7999) | d-7d2612-2a4435 |
| `cannot execute binary file`, exit 126 | `pmars/pmars` is Linux x86-64; use `tools/pmars-host.sh` | d-7d2612-3d04ba |

## pMARS exit codes

`0` ran (warnings included) · `2` command-line error · `3` assembly error (bad opcode, undefined
label, longer than MAXLENGTH) · `4` cdb `quit` · `126` from the shell: wrong-platform binary.
Assembly messages go to stderr; bbctester ignores stderr, so only the exit code reaches a test.

## Procedure

1. Get the exact redcode (`make compile`, or the golden's EXPECTED) and assemble it:
   `_build/pmars-host/pmars -@ pmars/config/94b.opt -A <file>` — read every warning.
2. Step it to the first instruction that differs from what the RED program means
   (`docs/semantics.md`, *Dynamics* and *The compilation scheme*).
3. Classify: user program wrong (say so), compiler wrong (record it: a characterization golden in
   `bbctests/known-bugs/`, a known-failing spec, a roadmap item — d-7d2612-c5bb3c), or settings
   (94b vs another hill).
4. A symptom not in the table above is new knowledge: add its row here in the same change.
