## core-modularise-workflow - the workflow was a leaf, and behaved like one

Stage 2 of specs/core-modules.md, the day after stage 1 carried the
streams out. `Wf` (980 lines), `Proc` (551) and `ProcMacro` (698)
became `okay-workflow`, with their thirteen suites.

THE SURVEY RAN WITH THE INSTRUMENT STAGE 1 HAD TO REBUILD FOUR TIMES,
and it is worth saying that it cost nothing this time. Comments
stripped rather than grepped around; every platform source directory
walked, not just `src/main/scala`; the grep keyed on the TOP-LEVEL
SYMBOLS the moving files define rather than on their file names; and
the test scan reading string literals, which is where stage 1's last
failure hid. The answer came back clean: the core names `Wf`, `Proc`
or `ProcMacro` **zero times in code**.

It held up. Both halves compiled on the first attempt, and the entire
73-module family needed exactly ONE `dependsOn` - okay-persist.
okay-ui uses `Proc` in a suite and reaches it transitively.

`Replayable` stayed, which is the whole of the core's side of this
seam: `Delim` is typed on that 78-line marker, not on the 980-line
`Wf`. That was the third of the three independent confirmations that
produced stage 1's rule, so seeing it hold when the files actually
moved is the useful part.

A crossProject rather than a JVM module, deliberately: these three
files sat in the core's shared source directory and therefore compile
on JS and Native today. A JVM-only module would have been a silent
loss of that, invisible until someone asked for it.

The core is now 52 files and 13 555 lines, from 74 and 21 914 before
stage 1 - 38% gone across two stages, with not one consumer's import
edited.

Spec: specs/core-modules.md. Commit baa1ecec.
