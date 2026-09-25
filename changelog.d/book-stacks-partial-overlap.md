## book-stacks-partial-overlap - chapter 16b leads with stacks of different composition

The "stacks do not compose" subsection now starts where it hurts most:
two teams share ONE effect (errors, in the same place) and not the rest.

- Team A: `EitherT[List, String, *]` (choices that may fail); team B:
  `EitherT[Writer[Vector[String], *], String, *]` (a log that may fail).
  Neither helper types in the other's stack — no ordering to agree on —
  and the compiler's refusal is quoted and pinned (`compileErrors`).
- The basket needs the union stack, which neither team wrote, and one
  hand conversion per team (`fromA`, `fromB`); tested to give the
  basket's answer.
- The effects version: team A's helper on `Throws % String`, team B's on
  `Writer % String`, both widened into the program's row with `.at`, no
  conversion; tested equal.
- The order example stays, as teams X/Y/Z, after the composition one.
- TestBookTwoMonadsCats 8 -> 11.
