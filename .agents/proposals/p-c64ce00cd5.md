---
# bundle-proposal: a change this repository offers to the agent-guides bundle. Only the home repository integrates it; nothing here is guidance.
proposal: p-c64ce00cd5
bundle: agent-guides
carrier: r-7d2612
base: 0.0.25
digest: 469d45b59078
kind: method
action: extends
target: examples-tested-by-assembly
lacks: a carrier of another owner (this is the second carrier, same owner)
seen: "2026-10-05"
---

A guide's worked examples are checked by the test suite against the real tool: every output block in the guide must equal what the tool produces from the nearest source block before it, so an example that drifts fails the gate. The check also catches the scripts that fill such a guide. Its boundary is the prose: a test over the examples cannot see a claim in the surrounding text that is false, nor a step a newcomer cannot follow; that part needs a run of the guide by a reader with no context.

## Evidence

A second carrier, same owner as the first occurrence. A user manual with several chapters had each output block required, by a test, to be the tool's output for the source block before it. On its first run the test caught a fill script whose non-greedy pattern spanned blocks and put several outputs under the wrong programs; the first pairing rule, which expected each output block directly after its source block, also skipped an example with a command between the two blocks, and was changed to pair each output with the closest preceding source block. Then a fresh agent followed the manual as a newcomer, from a built binary in a scratch directory, and reported about a dozen confusions and about eight mismatches between the prose and the tool's behaviour, none of which the example test could see; each was fixed and re-measured.

Literature: the candidate's own; no new source consulted.
