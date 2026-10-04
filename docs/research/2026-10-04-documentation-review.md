# The Core War documentation, checked against its sources

Date: 2026-10-04 (s-7d2612-9b0d20). A read-only review, in a fresh context, of every factual claim
about ICWS'94, pMARS, the hills and the community in `docs/references.md`, `docs/semantics.md`,
`LANGUAGE.md`, the decision rows that state pMARS facts, the `run-warrior` and `troubleshoot-redcode`
skills, and `pmars/config/94b.opt`. Sources: the ICWS'94 draft (`icws94.txt`), pMARS 0.9.4's sources
and manual (the vendored zip) with each behaviour claim run on the host pMARS, koth.org, SAL (via the
Wayback Machine: the site was down), Koenigstuhl, corewar.co.uk, the package archives.

## Corrected

| Claim | What the source says | Now |
|---|---|---|
| the default modifiers are ICWS'94 "A.2.1.1" (four documents) | A.2.1.1 is the ICWS'86 conversion; the table followed is A.2.1.2, the default ICWS'88 conversion | A.2.1.2, as pMARS implements it; pMARS gives `NOP` `.F` where the draft gives `.B`; the `LDP`/`STP` defaults are pMARS's own |
| the draft is "the only normative definition" | an annotated draft (3.3, 1995), never ratified, to-do list open | the de-facto definition; pMARS the practical reference |
| cdb's `list` pages "after about 40 lines" | `TEXTLINES 23` (`cdb.c`), measured | every 23 lines |
| `pqueue` "lists processes" | it switches `list` to the process queue (`pmars.6`), measured | `pqueue` then `list 0,N`; `registers` also shows it |
| `calc CYCLE`: 80000 minus instructions, "with one process" | the count is shared by warriors, not processes (`cdb.c`), measured with 7 processes | "with one warrior, any number of processes" |
| `-F 4000`: "fixed position" | warrior 2 at 4000 in round 1, seeded positions after (`pmars.6`) | said so |
| a lone warrior "always survives" | the score formula (W·W−1)/S is 0 for one warrior | it scores 0 dead or alive |
| `validate.red` "an ICWS compliance test" | its header: ICWS'88 compliance and KotH compatibility | said so |
| pMARS packages | Debian trixie and Ubuntu 24.04/25.10 0.9.4; testing/sid and 26.04 0.9.5; bookworm and 20.04/22.04 0.9.2 | updated |
| 0.9.5 "builds on macOS unpatched" | a stock `make` fails on X11 headers; it builds without source patches once the X11 flags are dropped | said so |
| koth.org's 94nop "most active"; p-space "absent" | not stated; most recent battle; p-space disallowed, pMARS's `94nop.opt` uses `-S 1` | "most recently active"; disallowed, `-S 1` |
| SAL's 94b settings | right, plus 250 rounds; the site down on 2026-10-04 | both noted |
| corewar.io "redirects elsewhere" | parked, redirects to advertising | said so |
| Digital Red Queen's MARS | a fork under CC BY-NC-SA 3.0 inside an Apache-2.0 repository | noted before any use as a second simulator |
| `DAT`/`NOP`/`JMP`/`SPL` arguments "only store data" | both operands are evaluated: `<`/`>` still move a pointer | said so in `LANGUAGE.md` |
| empty core is "`INSTR`" (d-7d2612-f7ae87) | pMARS's name is `INITIALINST` | renamed |
| exit 1 for a missing file (`pmars.6`) | 0.9.4 exits 3 ("Unable to open file") | in the troubleshooting skill |

## Verified as stated

ICWS'94: the `ORG 0` default; fields 0..M−1 and unsigned comparisons; division by zero removes the
task; `SLT.I` as `SLT.F`, strict `<`; `SPL` queues PC+1 then the target; `DAT` kills; the KotH
settings and the empty-core instruction; the condition layouts of `docs/semantics.md` §5. pMARS:
case-sensitive labels and predefined symbols, reserved pseudo-opcodes, the redefinition warning, the
trailing-label discard, the 256-character hang (255 assembles), warrior 1 at address 0, `-A` new in
0.9.4, `-k`'s "wins ties", the blank listing of empty core, the exit codes, `skip K` = K+1
instructions, EQU's unparenthesized substitution (the reason for d-7d2612-d9339f's parentheses),
p-space 500 by default. `94b.opt` matches SAL's 94b but for the rounds. The web entries (koth.org,
corewar.co.uk, Koenigstuhl, arXiv 2601.03335, rmars, Wilkies, n1LS, samurai/redcomp) as stated.

Not re-verified: "565 of n1LS assemble under 94b", "12×200 rounds in about 1 s", the lp and mp
settings beyond 2016 snapshots.
