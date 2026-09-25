## stack-safety-core - the core's stack recursions become loops (specs/stack-safety.md stage 1a)

Three recursions in okay's core overflowed a 128 KB stack at a few
thousand levels. Each is a loop now, and the test for each ran RED on
master first:

- `Aggregate.topK`: inserting past k kept elements was k frames deep.
  It is a `@tailrec` loop over the kept prefix, reversed.
- `Delim.split`: a shift to a prompt under 20 000 other delimiters
  rebuilt the prefix on the way back up. It walks down as a loop now,
  pushing each segment it passes as a polymorphic frame onto a
  type-aligned `Wrap`. `unwind` applies the frames, with no cast.
- `Static.foldMap`: arguments and both sides of a `Select` were folded
  by ordinary calls. `Args` is a complete type-aligned continuation
  now (`AppTo`, `SelectE`, `SelectF` beside `More` and `Mapped`), and
  `fold` is one `@tailrec` loop. Nesting on any of the three axes
  folds at any depth.

`SmallStack` (src/test/scala) runs a body on a thread with a small
stack, so a stack-safety test needs thousands of levels, not hundreds
of thousands. `scripts/recscan.py` now treats a `LazyList` cons as
deferred.
