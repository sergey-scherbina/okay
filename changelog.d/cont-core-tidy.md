## cont-core-tidy - the continuation core's comments short

The operator's ask (2026-10-01): remove what is extra from Cont, Cont0
and Delimited, and make the comments much shorter. Nothing in the
code was unused; what was extra was prose. Every comment in Cont.scala
and Delimited.scala is now one or two lines (Cont.scala 966 -> 513
lines, Delimited.scala 91); the code is byte-identical (compared with
comments stripped). The history and the measured reasons live in the
specs (cont-core.md, delimited.md, freer-kont.md). `Frames.runOf`,
`cat` and `installed` are private, `noFrames`/`noStack` package-private.
docs/direct-style.md and docs/theory/06-tagless-staging.md quote the
new comments. Also: delim-internal-shift0's history rows re-cite its
commit after land.sh rebased it.
