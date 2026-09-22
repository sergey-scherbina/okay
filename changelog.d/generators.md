## generators - Python-style yield: Gen[W], generator blocks, three endings

A generator is a program that tells — `Bind(Inject(Say(w)), k)`
suspends until a reader calls `k` — and this lane gives that program
its name and its words (specs/generators.md). `Gen[W]` (core,
Gen.scala) is a value class over `Unit ! (Writer % W + Stop)` — a
class only for name resolution: an extension on the alias cannot infer
`W` from a type-lambda row, and an extension on an opaque type lost to
the package's generic `map` over `Id` in lexical scope; a member beats
both and `AnyVal` costs nothing. `Stop` is one more member of the row,
the early end.
Element-wise `map`/`flatMap`/`withFilter`/`take`/`takeWhile`/`drop`/
`++`/`zipWithIndex` make a plain for-comprehension over a `Gen` a
generator with no macro; the readers — `toList`, `first`, `find`,
`exists`, `forall`, `foreach`, `foldUntil` — are `FoldUntil` (the
sibling lane's stopping fold), so each stops the body where it has
read enough; `iterator` is the Python semantics made literal: `next()`
runs the body to its next tell and holds the continuation for the call
after. In okay-direct, `generator[W] { … }` is a direct block over the
row in which `for x <- xs yield e` emits each `e` (statement or final
position — the one place `yield` means emit), `Gen.emit`/`Gen.stop`
are marks or bare statements, and `for x <- gen do` in an ordinary
block reads a generator through its iterator as far as the loop drives.

Laws (`TestGen` 10, `TestGenerator` 9): laziness by a step counter —
k `next()` calls, k steps; the same through `take`/`first`/`find`/
`exists` and through a generator block; the three endings — the body
ends, `Gen.stop` mid-loop (nothing after it runs, through `map` and
`take` too), the reader stops; an infinite generator through
`map`/`filter`/`take`; a nested for-comprehension with a guard, lazy;
non-memoising by default, `toLazyList` memoising; 100 000 elements flat;
a recursive generator block; a yielded value with a mark in it (the
mark runs first, then the emit); `for x <- gen do` reading three and no more.

What was decided against: a `Gen` class (would re-export every Writer
combinator and split programs from generators); `yield`-as-emit
outside a generator block (Scala's `yield` collects, and the block is
what says otherwise); `Throws` for the early end (a second member the
user sees in every type). The first `iterator` applied the held
continuation eagerly and was one yield ahead of Python — the counter
caught it. `yield twice(x).!?` emitted nothing: ANF hoisted the mark
out of the `Say` op's argument and the op became a bound value, not a
statement — a marked yield is bound to a val first, the op built from it. A new top-level name in package `okay` shadows every
nested `Gen` in the package: `Uid.Gen`'s constructor-proxy calls became
E177 and now say `new` (okay-stream's `Pipeline.Gen` is an enum case,
okay-staging's is abstract — no proxy, no clash). Docs: direct-style.md "Generators" with the three ways to
write one and the three endings; tutorial ch. 2; references
(Kiselyov–Peyton Jones–Sabry 2012, James–Sabry 2011, PEP 255).
