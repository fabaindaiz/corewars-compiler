# Known bugs

A characterization golden for each recorded compiler defect: a `.bbc` whose EXPECTED is what the
compiler emits today, wrong output included, with its roadmap id in DESCRIPTION. Its behaviour spec
in `behtests/` carries `known-failing: <id>`; when a fix makes the spec pass, `tools/behave.py` fails
until the golden moves to `bbctests/examples/` with the corrected output and the mark is removed
(d-7d2612-c5bb3c). Empty means no compiler defect is recorded with a failing spec.
