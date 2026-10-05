# Presenting RED: what worked, what misled, and how to choose the medium

Date: 2026-10-05 (s-7d2612-5e5abb). The user asked, in turn, to see how the language looks with
examples, for user manuals made from that, and then to iterate on the manual with use cases, the
best workflow and what was learned about presenting, so that a future session chooses these tools
by itself. The rules distilled from this note are the `present-red` and `write-warrior` skills
(`.claude/skills/`); this note holds the evidence.

## The three media, and when each served

| Medium | Asked as | What it was good for | What it cost |
|---|---|---|---|
| a tour in the conversation | "show me how the language looks, with examples" | a quick look, in the user's language, five compiled examples with their output | nothing kept: it lives in the chat only |
| `docs/manual/` in the repository | "documentation like user manuals" | something that stays true: every example compiled by `test_manual_examples` | a test, six chapters, a fill script |
| a published page | the same request ("both") | reading and sharing outside the repository, examples side by side | a generator (`tools/manual_page.py`) and a republish after each change |

The user chose, when asked: Markdown in the repository and a page made from it, in English, a
guide plus tutorial plus cookbook plus tools, with `LANGUAGE.md` kept as the reference.

## What went wrong, and the rule each produced

1. **Hand-written output would have drifted; a fill script went wrong instead.** The expected
   redcode was filled by a script whose non-greedy regex spanned blocks and paired outputs with the
   wrong programs. The self-verifying test caught it on its first run. Rule: never write output by
   hand, and fill it by parsing blocks with the test's own pairing (a `redcode` block belongs to the
   nearest `red` block before it).
2. **A claim measured once was not reproducible.** The manual said an imp against a dwarf "ties
   152 of 200 rounds"; the usability test got 150. pMARS places the second warrior at random unless
   given `-F`: with `-F 4000` the same battle gives 149 ties and 51 dwarf wins on every run. Rule: a
   battle quoted in a document carries a fixed seed.
3. **Output nobody decoded.** The manual said `-k` "prints wins and ties"; the newcomer could not
   tell which line was which warrior, nor read `Results: 0 51 149` (the first warrior's wins, the
   second's, the ties) or `Unknown by Anonymous scores 302` (3 a win, 1 a tie; no name given). Rule:
   show a tool's output once, decoded.
4. **Concepts used before they were explained.** Cells and fields, modes, modifiers, processes,
   cycles, hills and the benchmarks were used from the first page. A chapter 0 now covers them, and
   readers who know redcode skip it.
5. **A warning that steers into a mistake.** "Step 4 visits only 2000 of 8000 cells" led the
   newcomer to step 3, which the compiler reports as full coverage and which kills the warrior at its
   8000th instruction (it bombs its own `MOV`; measured with `alive 7999` and `dead 8000`). The
   tutorial now explains why 4 (every bomb lands 3 more than a multiple of 4; the code is in cells 0
   to 2), and the compiler's gap is a roadmap item (i-7d2612-1b199a).
6. **A test that cannot hold read as a defect.** The newcomer concluded that the core-clear kills
   itself because `(alive 80000)` failed. It is alive at instruction 79998; the round ends at 80000
   cycles. The manual now says that `alive N` with N of 80000 or more never holds.
7. **Unstated defaults.** An include path is relative to the including file (the snippets failed
   from the newcomer's own directory); `JN` on a cell tests its B-field (a `JMP $5, #0` reads as
   empty); opcodes are capitals; cdb exits with code 4; a bare number is a value (`(MOV 0 1)` is not
   an imp). Each is now said where it first matters.
8. **A link checked late.** The first page sent four links to anchors that did not exist (a file
   outside the manual taken for a chapter); checking every internal link against the page's ids
   caught it before publishing.

## What a newcomer test found that the author could not

A fresh-context agent followed the manual in a scratch directory with the built binary (no `dune`,
so it did not take the build lock) and reported, with commands and output, every place it got stuck.
In about five minutes of its time it found eleven confusions and eight mismatches, none of which the
self-verifying test can see, because they are about what the text leaves unsaid, not about whether
an example compiles. Both checks are needed: the test keeps the examples true; the newcomer keeps
the explanation sufficient.

Its reading of the use cases, kept in chapter 5: learning Core War (was poorly served: no primer),
learning RED as a redcode player (well served), writing a competitive warrior (partly: no guidance
on constants or on what beats what), testing an idea (well served: `--report`, `expect`,
`--emit-beh`; a paper-and-imp warrior from snippets in about five minutes), teaching (served by the
compiled examples; missing exercises).

## Measured for the manual

- Cost of each tool on one warrior (the stone): compile and report under 10 ms, a behaviour spec
  0.07 s, `tools/bench.py` 6 s, `tools/bench.py --hill` 89 s.
- What beats what, the RED warriors of the cookbook compiled (94b, 200 rounds, `-F 4000`): papers
  beat the stone (152 to 3) and every scanner here (scanner 124 to 0, SEQ scanner 147 to 0,
  quickscan 182 to 4, core-clear 132 to 0); the stone beats the scanner (113 to 0); Mice beats the
  stone (139 to 1). A first version measured the hand-written twins and quoted three rows that the
  RED warriors do not reproduce (the scanners' layouts differ); the branch review caught it. The classic
  circle's "scanners beat papers" does not hold for these forms; it needs a faster scanner with a
  clear (ASSUMPTION from the literature, not measured here).
