# Staged tagless: partial evaluation of Control programs

## Overview

"Finally Tagless, Partially Evaluated" (Carette–Kiselyov–Shan), adapted:
the final tagless `Control` interface doubles as a staging API. A program
written as an `inline def` against an abstract carrier `M`, taking its
instance by `staged[M]`, is partially evaluated by the Scala 3 inliner at
each instantiation: the instance arrives at its precise (anonymous
refinement) type, so its `inline` operations resolve statically and the
tagless dispatch disappears at compile time. At the `Func` carrier the
program unfolds into plain nested closures — direct-style CPS code with
no interpretive layer; at the `Cont` carrier the same program builds the
tree and keeps the tools.

## Interface

- `transparent inline def staged[M[_, _, _]]: Control[M]` (Cont.scala) —
  summons the instance at its precise type; the single new public name.
- The staged programming style: `inline def prog[M[_, _, _]] =
  { val C = staged[M]; C.flatMap(C.pure(1))(x => C.shift(...)) }` —
  operations called directly on `C` (extension-as-method), which is what
  makes them statically resolvable.
- No changes to `Control`, its instances, or any other API. The
  instances' `override inline def` operations are the load-bearing
  contract: staging works exactly because they are inline.

## Behavior

- [x] One inline program instantiates at both carriers and agrees:
      `reset(prog[Cont]) == prog[Func](identity)` (TestCont "staged").
- [ ] The payoff is measured: a statically-unrolled flatMap chain at
      `Func` vs `Cont` (StagedBenchmark), medians recorded in
      src/jmh/history.tsv. Prediction, written before measuring: Func
      ≥ 1.5x faster (no nodes, no interpretive match); below that the
      staging claim is weak and the spec must be revisited.
- [ ] All existing tests stay green.

## Design

Why not quotes/Expr (true multi-stage): `Control`'s operations traffic
in raw host values and functions — `shift(f: (A => S) => R)`,
`flatMap(f: A => M[B, S2, S])` — and arbitrary host functions cannot be
lifted into `Expr` (no `ToExpr` for functions; CKS stage interfaces are
representation-abstracted for exactly this reason). Expr-staging would
require a separate Symantics-style DSL layer, out of scope.

Why `transparent inline` + `summonInline`: a plain `using C: Control[M]`
parameter is typed at the widened trait, so every call is virtual and
nothing unfolds. `summonInline` under a `transparent inline def` returns
the given at its precise anonymous-class type; member calls on that type
resolve to the `inline` overrides and expand.

Limits, stated honestly:
- Only static program structure unfolds; a runtime-`n` loop does not
  (unrolling needs `inline n` and compile-time recursion, bounded by
  `-Xmax-inlines`, default 32).
- Extension syntax (`m.flatMap(f)`) inside a staged program needs a
  `given Control[M]` in scope and stays a virtual call (JIT-monomorphic,
  but not compile-time-unfolded); the staged style calls through `C`.
- `Func` remains non-stack-safe; staging inherits the carrier choice
  rule: the tree for tools, the function for speed.

## Decisions

- **Inline partial evaluation, not quotes** — see Design; the CKS
  "partially evaluated" half fits the existing signatures, the
  multi-stage half does not. Rejected: `scala.quoted` instance
  (impossible for raw-function signatures), runtime staging library
  (same, plus a compiler dependency).
- **`staged` returns `Control[M]` transparently** rather than adding
  inline members to the trait — abstract inline members would force
  every instance to be inline and still not devirtualize through the
  widened trait type.
- **Effects staging deferred** — `Effects.foldCont` is fixed to the
  `Cont` carrier, so effect programs interpret into the tree regardless;
  staging them needs a Control-generic foldCont first (a separate
  design with its own costs).

## Out of scope

- A Symantics-style quoted DSL over Control.
- Effects/Eff staging (see Decisions).
- Any change to existing semantics or APIs.

## Results

To be filled after the benchmark: per-carrier medians and the ratio,
also appended to src/jmh/history.tsv.
