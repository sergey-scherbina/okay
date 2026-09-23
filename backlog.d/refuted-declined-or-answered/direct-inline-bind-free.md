- direct-inline-bind-free — DONE 2026-09-23, REFUTED: a Free `direct`
  block's binds resolved to the given's `override inline flatMap`
  (the precise-typed `mm$direct` val + `Select`, as the staged road)
  read 14.35/14.46 µs against 14.25/14.28 before and 160 128 B against
  164 928 — time 1.00, bytes 0.97, both under the 3% line this entry
  set (history `dib-*`, quiet box, load 2–4). What it took to even
  compile said why it is not worth keeping: the inliner proxies the
  bind's receiver (`val a$proxy = fa`) and re-homes what it holds, and
  a Free block's receiver holds REFLECTED lambdas — `Free.delay(() =>
  …)` thunks of deferred calls — whose `$anonfun` LambdaLift then
  cannot find (TestBookInTheSystem, the third time); fixed by binding
  the receiver to a val first and quoting the one-parameter program
  lambda, 367/367 green, and then the number said no. The macro road
  stays the quote road for every carrier but a staged block, byte for
  byte. The gap that remains to the hand-written Free program
  (13.3 µs / 154 528 B, `freeHandNestedRun`, kept in StagedBenchmark)
  is the DEFERRED SELF-CALL — a thunk and a `Delay` node per iteration,
  the stack-safety feature, ~5 KB and 1 µs per hundred — not a bind.
