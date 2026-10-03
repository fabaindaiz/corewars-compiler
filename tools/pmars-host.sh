#!/bin/sh
# Build pMARS for the machine you are on, from the vendored source in pmars/pmars-0.9.4.zip.
#
# Why: pmars/pmars is a Linux x86-64 ELF (it runs the `execute` suite on Linux CI). It cannot run
# on macOS, and it has no use as an observation tool there. This build keeps the cdb debugger
# (no -DSERVER), so `pmars -e` can step a warrior and print core cells (see the run-warrior skill).
#
# Output: _build/pmars-host/pmars (under _build/, which git ignores). Idempotent: rebuilds only
# when the binary is missing. Writes nothing outside _build/pmars-host/.
#
# -Dround=pm_round: pMARS declares a global `round` that clashes with libm's round() on current
# compilers ("redefinition of 'round' as different kind of symbol"); renaming it at compile time
# leaves the source untouched. No X11 display code is compiled in (no -DGRAPHX).
set -eu
root="$(cd "$(dirname "$0")/.." && pwd)"
out="$root/_build/pmars-host"
bin="$out/pmars"
if [ -x "$bin" ]; then
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
  make CC="${CC:-cc}" \
    CFLAGS="-O -w -DEXT94 -DPERMUTATE -DRWLIMIT -Dround=pm_round" \
    LIB="" LFLAGS="" >/dev/null
)
cp "$work/pmars-0.9.4/src/pmars" "$bin"
rm -rf "$work"
echo "$bin"
