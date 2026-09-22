## direct-loops-v2 - the whole for-comprehension, the HOFs a mark lands in, yield into any collection

The operator named the shapes ("многогенераторные, HOF и yield",
2026-09-22), and the loops phase gained one thing per loop, every loop
keeping v1's shape — an immutable LazyList, a recursive def, the body
compiled per element against the loop's own tail:

- GUARDS: `for x <- xs if p(x) do/yield …` — `xs.withFilter(x => p)`
  peeled off the receiver, chained for several, evaluated per element
  in source order; a marked guard binds before the body.
- SEVERAL GENERATORS: `for x <- xs; y <- ys yield f` (`flatMap` over
  `map`) — results in the comprehension's order, a guard between
  generators honoured, a short-circuit inside the inner generator
  ending the whole comprehension (the Option law).
- HOFs with a marked lambda: `exists`/`forall`/`find` stop at the
  element that decides (the log proves where), `filter` keeps the
  matches, `foldLeft(z)(f)` threads the accumulator with a marked `z`
  binding first.
- YIELD SHAPES: a for-yield answers the node's own collection —
  List/Seq/Iterable as is, Vector/IndexedSeq, Set, Map of pairs;
  anything else (LazyList, Array) is refused naming the workaround.

Two v1 tests asserted the refusal of `filter` and `exists`; they now
assert it on `sortBy` and `count`, which stay refused. `TestDirectLoops2`
(12) covers each shape against the monad's own program, with the
Writer log as the witness of order and of where a walk stopped.

What the lane learned about quotes, three pickler crashes deep: a
quote nested inside a splice may name the outer quote's SYMBOLS
(`loop`, `tl`, `acc`) but not carry a TYPE TREE of the pattern-bound
element type — `(b: u) => …`, `x :: acc`, `Some(h)` all fail with
"unresolved symbols: given instance u$given", whether the given is a
`tpe2` lambda's parameter or an `asType` pattern's. Every per-element
step is therefore built by reflection (`bind`, `consTo`, `Apply(loopFn,
…)`), and the only nested quote is a harmless `loop(tl, acc)` the parts
are read from. The rule is at the top of DirectLoops' v2 section.
