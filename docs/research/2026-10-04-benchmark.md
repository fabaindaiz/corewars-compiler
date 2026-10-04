# RED against the best published warriors

Date: 2026-10-04 (s-7d2612-9b0d20). The user asked for a measurement that uses the compiler and
compares it with the best programs available. Tool: `make bench` (`tools/bench.py`,
d-7d2612-1d4491); opponents downloaded into `_build/bench/`, never committed (no licence covers
redistribution; their code is not read for ideas here either).

## Method

- **Opponents.** The Wilkies benchmark (12 warriors from the early 1990s, koth.org), scored under
  the 94b settings; the top 20 of Koenigstuhl's 94nop archive hill (asdflkj.net, 1106 published
  warriors, ranked by a recursive score), scored with pMARS's defaults (core 8000, 80000 cycles,
  length 100), as that hill scores; and, for a placement, the whole hill.
- **Score.** The mean over the opponents of (3·wins + ties)·100/rounds: 300 beats everything, 100
  ties everything. 500 rounds against Wilkies, 200 against the top 20, 100 against the whole hill,
  all with a fixed seed (`-F 4000`), so a score repeats exactly.
- **Placement.** Koenigstuhl ranks by a recursive score (its page): the mean against everyone with
  weight 1, against the top half with weight 1/2, the top third with 1/3, ..., while the group has
  more than 50, re-ranking between steps. `tools/bench.py --hill` estimates it from each opponent's
  result in the published order and places the warrior among the published scores. **Calibrated** on
  hill entries scored the same way: #750 (Archer II) 101.5 against its published 101.3, #845 (Return
  of the Boss) 84.6 against 83.2, the top five within 6 points (161.0 against 156.5 for The
  Collective, 155.0 against 161.4 for Arvon). A plain mean is no placement: it puts a weak warrior
  about 30 points too high (#845's plain mean is 115.0); the first version of this study did that
  and placed the best RED warrior at #797 (corrected after the branch review).
- **Warriors.** The nine archetypes in RED (`archetypes/*.src`) and by hand (`archetypes/*.red`), and
  two compositions written with the compiler for this study (`examples/stone_imp.src`,
  `examples/paper_imp.src`).

## The compiler's own cost: none measurable

With the seed fixed, RED and hand-written versions score **exactly alike** for imp, dwarf, stone,
paper, imp ring and imp spiral, on both sets: the compiled code plays the same game. The three that
differ do so by layout, not by cost: the scanner (RED 62.5 / 44.5, hand 58.9 / 42.0: its pointer in
the loop's `JMP`, measured before as the reason), the SEQ scanner (38.9 / 24.1 against 36.2 / 21.2:
the same) and the core-clear (42.1 / 15.9 against 46.6 / 18.8: RED's pointer cell comes after the
code, the hand-written one before it with `END top`; RED has no `ORG`). The north star
(d-7d2612-e006c2), near-zero overhead against the hand-written form, holds for every archetype.

## The distance to the best: strategy, not compilation

| Warrior | Wilkies | Koenigstuhl top 20 | recursive (estimated) | place of 1107 |
|---|---|---|---|---|
| The Collective (Koenigstuhl #5) | 171.5 | 141.3 | 161.0 | (#2 by the estimate) |
| King Cobra (#4) | 177.2 | 140.6 | 158.6 | (#4) |
| pst v4 (#3) | 159.5 | 131.8 | 160.5 | (#2) |
| Arvon (#1) | 158.8 | 132.0 | 155.0 | (#8) |
| Azathoth (#2) | 155.6 | 129.9 | 159.8 | (#3) |
| stone (RED) | 80.5 | 51.8 | 75.2 | #899 |
| stone and imp ring (RED, composed) | 86.9 | 51.3 | 73.3 | #905 |
| paper and imp ring (RED, composed) | 84.0 | 70.0 | 67.5 | #928 |
| paper (RED) | 81.0 | 71.2 | 67.2 | #929 |
| scanner (RED) | 62.5 | 44.5 | 65.3 | #933 |
| imp ring (RED) | 79.4 | 36.5 | 55.1 | #968 |
| imp (RED) | 50.9 | 43.6 | 53.8 | #971 |
| imp spiral (RED) | 70.5 | 40.7 | 52.4 | #978 |
| dwarf (RED) | 48.6 | 28.4 | 50.0 | #990 |
| SEQ scanner (RED) | 39.0 | 24.1 | 40.4 | #1031 |
| core-clear (RED) | 42.1 | 15.9 | 36.8 | #1045 |

The top warriors' "top 20" scores leave out the battle against themselves (the first version of this
table included it).

The hill's scale: #1 161.4, #10 154.3, #20 151.4, #100 145.1, #500 127.1, #750 101.3, #845 83.2,
#1000 47.6, last 3.8. The best RED warrior, the stone, places #899 of 1107 (in the bottom fifth);
the best published ones score about twice as much against Wilkies and the top 20, and their
recursive score is about twice the stone's.

The gap is the strategy: the archetypes are the textbook forms of the early 1990s, and the hill's
top is two decades of refinement (quickscans in front of papers, multi-phase bombers and clears,
constants tuned by optimizers, several by an evolver, Forge-AI). The compiled forms lose nothing to
their hand-written twins, so closing the gap is writing better warriors, and what RED cannot yet
write is what keeps the top strategies out of reach:

- **compile-time repetition**: a quickscan is a score of unrolled comparisons (waits for the macro
  layer, by the user's decision, i-7d2612-ec4d2d);
- **an A-field postincrement on a number** (`}`): the fast Silk-style paper copies through one;
  RED writes `}` only through a variable stored in an A-field (i-7d2612-e98368);
- **an entry point other than the first cell** (`ORG`/`END`): warriors that keep data before code
  (i-7d2612-e725ef);
- **a phase change**: switching from a scan to a clear when the scan ends is written today with
  labels and jumps, outside the structured fragment.

## Using the compiler to write warriors

The two compositions took a header each and reused the archetypes' code: `(hill 94nop)` emitted
the right header and `;assert`, `(name ...)` and `(strategy ...)` the metadata, constants the
`EQU`s, label arithmetic the ring's launch, and the step warning pointed at the stone's mod-4 step
until `(expect (step 3044))` stated it. Composing helped against Wilkies (stone 80.5 → 86.9, paper
81.0 → 84.0), not against the top 20 (51.8 → 51.3, 71.2 → 70.0) nor on the whole hill (75.2 → 73.3,
67.2 → 67.5).

A sweep of the paper's two constants (distance 2000 or 3200, step 1471, 2365, 3039 or 3359), each a
one-line change to an `EQU`, moved it by at most 5 points against Wilkies and 9 against the top
20; the best was the archetype's own (2000, 2365) or (2000, 1471): 71.2. The paper's limit is its
design (one process copying seven cells), not its constants.

## Reproducing

`make bench` (the archetypes, against both sets, compared with `tools/bench_baseline.json`);
`python3 tools/bench.py --hill FILES` for the placement; `python3 tools/bench.py FILE.red` for any
warrior, the top ones included once downloaded (`_build/bench/koenigstuhl/HILL32/`).
