#!/bin/sh
# Build pMARS for the machine you are on, from the vendored source in pmars/pmars-0.9.4.zip.
#
# Why: pmars/pmars is a Linux x86-64 ELF (it runs the `execute` suite on Linux CI). It cannot run
# on macOS, and it has no use as an observation tool there. This build keeps the cdb debugger
# (no -DSERVER), so `pmars -e` can step a warrior and print core cells (see the run-warrior skill).
#
# Output: _build/pmars-host/pmars (under _build/, which git ignores). Idempotent: rebuilds only
# when the binary or its patch stamp is missing. Writes nothing outside _build/pmars-host/.
#
# -Dround=pm_round: pMARS declares a global `round` that clashes with libm's round() on current
# compilers ("redefinition of 'round' as different kind of symbol"); renaming it at compile time
# leaves the source untouched. No X11 display code is compiled in (no -DGRAPHX).
#
# The extracted copy is patched, never the zip: sim.c formats "Warrior %d: %s terminated - End of
# round %d" into a 60-byte buffer when an opponent's ;break armed the debugger, so a warrior whose
# name has about 19 characters or more overflows it (macOS's fortified sprintf traps, exit 133;
# elsewhere the stack is silently overwritten). 256 bytes hold any name pMARS accepts on a line.
# Battle results do not change. The stamp makes an older, unpatched build rebuild.
set -eu
root="$(cd "$(dirname "$0")/.." && pwd)"
out="$root/_build/pmars-host"
bin="$out/pmars"
stamp="$out/.patched-outs256"
if [ -x "$bin" ] && [ -f "$stamp" ]; then
  echo "$bin"
  exit 0
fi
mkdir -p "$out"
work="$out/src"
rm -rf "$work"
mkdir -p "$work"
unzip -q -o "$root/pmars/pmars-0.9.4.zip" 'pmars-0.9.4/src/*' -d "$work"
(
  cd "$work/pmars-0.9.4/src"
  sed -i.orig 's/char    outs\[60\];/char    outs[256];/' sim.c
  grep -q 'char    outs\[256\];' sim.c
  make CC="${CC:-cc}" \
    CFLAGS="-O -w -DEXT94 -DPERMUTATE -DRWLIMIT -Dround=pm_round" \
    LIB="" LFLAGS="" >/dev/null
)
# a new file, not one copied over: macOS kills a binary rewritten in place after it ran (Killed: 9)
rm -f "$bin"
cp "$work/pmars-0.9.4/src/pmars" "$bin"
touch "$stamp"
rm -rf "$work"
echo "$bin"
