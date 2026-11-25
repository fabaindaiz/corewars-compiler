---
# bundle-proposal: a change this repository offers to the agent-guides bundle. Only the home repository integrates it; nothing here is guidance.
proposal: p-61ff345fef
bundle: agent-guides
carrier: r-7d2612
base: 0.0.25
digest: 469d45b59078
kind: method
action: extends
target: clean-context-review-closes-a-batch
lacks: a carrier of another owner; restates a line of the owner's global instruction file, so one source
seen: "2026-10-05"
---

The measured hit rate of a clean-context review at the close of a batch is high but not total, and it falls with the size and novelty of what is reviewed: a review of a small commit of minor fixes found nothing important, while reviews of whole feature branches found important defects nearly every time. A close's summary of its reviews is counted from the review records, not remembered, because the tempting summary is "every review found something".

## Evidence

Two carriers of one owner. In the first, over one long working session, four of five fresh-context reviews found important defects (a capture in a macro expansion, a value not counted by an optimiser, one path spelled two ways breaking read-once, published constants reproduced from memory, a warning blind to a documented layout, and two measurements taken on the wrong artefact); the fifth, of a small commit of review minors, found none. The close's first draft of its learnings said every review had found an important issue, and was corrected before committing. In the second carrier, three fresh reviews in one day found no blocker and at least one should-fix each, mostly statements left stale elsewhere by the change.

What admission still needs: a carrier of another owner. The practice restates a line of the owner's global instruction file (a close never skips a review), so these carriers are one source for the rule; the new part is the boundary on the hit rate.

Literature: the candidate's own; no new source consulted.
