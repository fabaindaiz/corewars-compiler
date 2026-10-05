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
# The extracted copy is patched, never the zip: sim.c formats "Warrior %d: %s terminated ..." into
# a 60-byte buffer when an opponent's ;break armed the debugger, so a warrior whose name has about
# 19 characters or more overflows it (a fortified sprintf traps, exit 133). A name is bounded only
# by pMARS's line buffer, so both such sprintf calls become snprintf, and the buffer grows to 256 so
# that names up to about 210 characters still print whole. Battle results do not change. The stamp
# makes an older build rebuild; the tools call this script every time for that reason.
set -eu
root="$(cd "$(dirname "$0")/.." && pwd)"
out="$root/_build/pmars-host"
bin="$out/pmars"
stamp="$out/.patched-snprintf"
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
  sed -i.orig -e 's/char    outs\[60\];/char    outs[256];/' -e 's/sprintf(outs, /snprintf(outs, sizeof(outs), /' sim.c
  grep -q 'char    outs\[256\];' sim.c
  [ "$(grep -c 'snprintf(outs, sizeof(outs), ' sim.c)" -eq 3 ]
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
