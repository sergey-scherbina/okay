Okay! Extensible effects for Scala 3.

Inspired by Oleg Kiselyov.

https://okmij.org/ftp/Haskell/extensible/more.pdf
"Freer Monads, More Extensible Effects" Oleg Kiselyov

https://bentnib.org/paramnotions-jfp.html
"Parameterised notions of computation" Robert Atkey

## Architecture

- `Cont[A, S, R]` (Cont.scala) — the parameterised continuation monad
  (answer-type modification, shift/reset), defunctionalized like Free,
  so running a flatMap chain is stack-safe.
- `Control[M[_, _, _]]` (Cont.scala) — final tagless interface of
  delimited control; instances: `Cont` (stack-safe data) and `Func`
  (the function encoding, the reference).
- `Effects[M[_[+_], _]]` (Effects.scala) — final tagless interface of
  extensible effects, founded on the continuation paramonad: a handler
  is `F !> S = F ==> ([X] =>> X /> S)`, an interpretation of the
  operations in Cont, and the meaning of a computation is its `foldCont`;
  `runWith` and `handle` derive from it. Instances: `Free` (initial,
  defunctionalized) and `Eff` (final, Church). Choosing: the tree is
  for tools (stepping, staged relay, stack safety on any bind shape),
  the function is for speed (fused build-and-run pipelines), and the
  interface is for not choosing too early — `fromFree` and `reify`
  move programs between the encodings.
- `!.relay` (Effects.scala) — tail-resumptive handling: the answer-polymorphic
  handler must resume exactly once, which keeps the loop tail-recursive.
  `Effects.handle` — general handlers (abort, forwarding), via foldCont.

Benchmarks: `sbt 'Jmh/run .*FibBenchmark.*'`, history in src/jmh/history.tsv.
