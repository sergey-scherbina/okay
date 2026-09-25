#!/bin/sh
# probe-contmacro-depth.sh <depth> — writes src/test/scala/ProbeContMacroDepth.scala:
# one `shift` whose body is an `if … else if …` chain <depth> levels deep,
# every branch a tail `k(i)`, the shape ContMacro.rewrite follows one
# frame per level. Compile it with `scripts/gate.sh "okayJVM/Test/compile"`
# and read which phase overflows: a ContMacro frame in the trace would
# break the bound written beside `rewrite` (specs/stack-safety.md,
# Decision 2). DELETE the file afterwards; it is a probe, not a test.
# Measured 2026-09-25, Scala 3.9.0, sbt's compile thread (-Xss8m):
# 2075 compiles and rewrites (Cont$.tailShift in the bytecode); 2150 to
# 4200 die in PostTyper, 4800 in Typer, 10000 in the parser — before
# the Inlining phase where the macro runs.
set -eu
n="${1:?usage: probe-contmacro-depth.sh <depth>}"
out=src/test/scala/ProbeContMacroDepth.scala
{
  printf 'package okay\n\nobject ProbeContMacroDepth:\n'
  printf '  def f(x: Int): Int /> Int = shift[Int, Int, Int](k =>\n'
  printf '    if x == 0 then k(0)\n'
  i=1
  while [ "$i" -lt "$n" ]; do printf '    else if x == %d then k(%d)\n' "$i" "$i"; i=$((i + 1)); done
  printf '    else k(-1))\n'
} > "$out"
echo "wrote $out ($n levels)"
