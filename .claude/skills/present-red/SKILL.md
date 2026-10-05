---
name: present-red
description: Explain, show, teach or document the RED language and its compiler - a tour with examples, a manual chapter, a cookbook recipe, a page to share. Use when asked to "show how the language looks", "explain RED", "give examples", "write documentation", "a user manual", "make a page", "publish the manual", or to present results to someone, so the examples are compiled rather than written by hand and the output is one the reader can trust.
allowed-tools: Bash, Read, Write, Edit
---

# Present RED

What was learned presenting RED (s-7d2612-5e5abb and the tour before it), as rules. The evidence
is in `docs/research/2026-10-05-presenting-red.md`.

## Pick the medium by who reads it and how long it must stay true

| The ask | Medium | Tool |
|---|---|---|
| a quick look in the conversation ("show me how it looks") | the chat, in the user's language | compile each example now with `_build/default/execs/run_compile.exe` (or `dune exec ...`) and paste source and output |
| documentation that lives with the code | a chapter in `docs/manual/` (English, d-7d2612-cbb430) | `red` blocks, then `redcode` blocks filled by the compiler; `test_manual_examples` keeps them true |
| something to share or read outside the repository | the single-page manual | `python3 tools/manual_page.py`, then publish `_build/manual-page/index.html` with the Artifact tool, same path to keep the URL |
| syntax a user looks up | `LANGUAGE.md` | the reference; the manual links to it and never duplicates it |

Ask before writing a new manual or page: where (repo, page, both), language, scope; recommend one.
Offer the page in one line when a chat answer would read better as a page.

## Rules

1. **Never write redcode output by hand.** Compile every example. In the manual, a `redcode` block
   is checked against the nearest `red` block before it; fill outputs by parsing blocks, never with
   a regex that can span them (it paired outputs with the wrong programs once).
2. **Run every claim before writing it, reproducibly**: a battle's result, a cell after N steps, a
   cost. A number in the text carries the run that produced it, and a battle carries a fixed seed
   (`-F 4000`): without one pMARS places warriors at random, and the manual's first "152 ties"
   became 150 on the next run.
3. **Show source and output side by side**, explain the generated labels once (`_LET1` is a
   variable's cell, `_REP4` a loop's head), and point at the one line that matters in each output.
   Decode a tool's output the first time it appears (`-k`'s lines, `Results: W1 W2 T`, a score).
4. **Explain before using**: cells and fields, modes, modifiers, processes, cycles and hills are in
   `docs/manual/00-core-war.md`; link it for a newcomer instead of assuming it. Say each default
   where it first matters (an include path is relative to the including file; `JN` on a cell tests
   its B-field; opcodes are capitals; cdb exits 4).
5. **Never let a warning or a test mislead**: if the obvious fix to a warning is wrong (step 4 to
   step 3 kills the dwarf), say so beside it; if an expectation cannot hold (`alive 80000` on a
   80000-cycle round), say why before a reader takes it for a defect.
6. **An example compiles cleanly or says why not**: a data cell without a label prints a dead-code
   warning; label it.
7. **Watch RED's traps in examples**: a bare number is a value (`(SPL (Dir 1))`, not `(SPL 1)`);
   `step` is reserved; cdb prints values above 4000 as negative.
8. **Test a manual on a newcomer** before calling it done: a fresh-context agent that follows it in
   a scratch directory and reports where it got stuck, with the built binary
   (`_build/default/execs/run_compile.exe`) instead of `dune`, so it does not take the build lock.
   The compiled-examples test keeps examples true; only a newcomer finds what the text leaves out
   (the first test found 11 confusions and 8 mismatches in five minutes).
9. **A page leaves the repository**: no repository URLs or identifiers in it (privacy), repository
   files shown as paths; check every internal link against the page's ids before publishing; the
   page is private until the user shares it, say so.
10. **The conversation follows the user's language; the repository stays in English.**

## Done means

`make check` passes (the manual test included); the page, if any, was rebuilt and republished from
the committed Markdown; the changelog says what was presented and what went wrong.
