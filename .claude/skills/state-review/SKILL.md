---
name: state-review
description: Periodic review of this repository's instruction system and plans - whether the documents in AGENTS.md's map are still true, every rule still has a working enforcer, the roadmap reflects what was built or measured, and the agent-guides bundle is current. Use when asked for a "state review", "health check of the docs", "is the roadmap up to date", or every few weeks of active work.
allowed-tools: Bash, Read, Grep, Glob
---

# State review

Bootstrapping is not the end; the documents rot unless someone checks them. Read-only until the
report is approved.

## Run first

```sh
python3 tools/audit.py
python3 tools/behave.py
python3 .agents/tools/bundle.py verify
python3 .agents/tools/bundle.py changelog --since "$(sed -n 's/^version: "\(.*\)"/\1/p' .agents/README.md)"
git log --oneline -20
```

## Answer, each with evidence

1. Does every document in `AGENTS.md`'s map exist, and is every claim in it still true? (The audit
   checks paths; read the claims.)
2. Does every rule in `AGENTS.md` and `docs/decisions.md` still have an enforcer that does the job?
   Which rules are still `—` and could cheaply move to a check in `tools/audit.py` or a spec?
3. What changed since the last review that should have become a decision row and did not?
4. What in `docs/roadmap.md` is now closed — built, or retired by a measurement? Is *Where we are*
   true today? Is any known-failing spec still marked although its bug was fixed?
5. Has `AGENTS.md` drifted towards its 200-line budget, and which section grew?
6. What friction appears twice in `.claude/logs/agent-changelog.md` and is not in the roadmap's
   *Process and tooling* area? Price it: seconds × occurrences × sessions.
7. Is anything in `origin/dev` (i-7d2612-ec4d2d) newer than the roadmap says?
8. What has this repository learned that the method does not know? Apply the generality test in
   `.agents/method/prompt-context.md` §*Improving the method*; candidates go to
   `.agents/method/prompt-harvest.md`, never into `.agents/` by hand.
9. Is the bundle behind the latest release? If so, run `.agents/method/prompt-update.md` before the
   next significant piece of work.

## Report

One table: question, finding, evidence, proposed change. Propose; do not perform.
