# 6. Final tagless and staging

## Two answers to one accusation

The accusation against everything in chapters 1–5 is *interpretive
overhead*: programs-as-data pay a node per step and a dispatch per
node. This chapter is the two classical answers, both present in okay,
each in the place its theory says it belongs.

## Final tagless: abstract over the interpreter, then let it vanish

Carette, Kiselyov and Shan's "Finally tagless" [Carette, Kiselyov &
Shan 2009] represents a program not as a data tree but as a
*polymorphic function over its interpreter*: write against a trait of
operations, instantiate with a concrete carrier, and there is no tree
— the "interpretation" is ordinary method calls, inlinable by the
compiler. Their title's second half, *partially evaluated*, is the
point okay leans on: when the carrier is statically known, the
abstraction can be made to cost nothing.

okay's `Control` trait (chapter 2) is exactly this shape, and
`Cont.scala:19–27` names the maneuver:

```scala
/** Staging via final tagless (Carette–Kiselyov–Shan, the partial
 * evaluation half): in an `inline def` program, `val C = Control[M]`
 * summons the instance at its precise type, so the instance's inline
 * operations resolve statically and the tagless dispatch evaporates
 * at compile time — at the Func carrier the program partially
 * evaluates to plain nested closures. */
transparent inline def Control[M[_, _, _]]: Control[M] = summonInline[Control[M]]
```

Scala 3's `inline`/`summonInline` is the partial evaluator here: the
same program text runs defunctionalized (the stack-safe `Cont` carrier)
or as raw closures (the `Func` carrier), chosen by a type argument, and
`specs/staged-tagless.md` records the measurements that keep both.

The same mechanism, aimed at data rather than at dispatch, is the
`inline`+`summonFrom` specialization that chapter 7's fold story and
`ChunkBuf.scala` depend on: `summonFrom { case ct: ClassTag[A] => … }`
asks *at the call site, at compile time* whether the element type is
concrete, and emits the unboxed branch when it is. The measured stakes
are in `docs/benchmarks.md` §12 — 38.2µs → 7.0 for a fold whose step
inlines — together with the trap the same page records: an `inline`
constructor that stores its step in a `Function2` field un-specializes
itself, because the field's `apply` erases generic. Staging by inlining
works only while the value never crosses a generic boundary.

## Staging proper: programs that build programs

Multi-stage programming [Taha & Sheard 1997, 2000] makes generation
explicit: code that constructs code, with the type system guaranteeing
the generated program is well-typed. MetaML's brackets and escapes are
the reference design; Scala 3 carries the idea natively as quotes and
splices, and `specs/staged-pipelines.md` chose deliberately between the
two staging styles — "the proven CKS half from staged-tagless first,
Expr/quotes second" — using quotation only where inlining runs out.

Where inlining runs out is instructive, and `Pipeline.scala` is the
worked example. A stream pipeline reified as a typed operator tree
(`Pipeline.scala:12` — "the Catalyst idea in our native ground") can be
*rewritten* before it runs: map fusion, filter fusion, take pushdown,
each rule property-tested to preserve semantics. But at a `Mapped` node
the intermediate element type is existential — gone at the type level —
so compile-time specialization has nothing to grab. The answer
(`Pipeline.scala:16–27`): **the evidence travels with the data**. A
`ClassTag[B]` is captured where the node was built and `B` was still
concrete, and the compiler-time specializer of `Chunks.map` is
reconstructed *from data* at interpretation time. Staging by inlining
where types are static; staging by carried evidence where they are
not; quotation reserved for the whole-stage loops that neither reaches
— three rungs, each used exactly where the rung below stops.

## The unifying view

Chapter 4's `Free` and this chapter's tagless are the two classical
encodings of the same algebra — initial (data, easy to inspect) and
final (functions, easy to run fast) — and okay refuses to pick one
globally. Effects are initial, because handlers must *split rows and
inspect operations*. The control substrate is final-taglessed, because
nothing inspects it and speed matters. The pipeline layer is initial
again — reified precisely to be rewritten — and then compiled onto the
staged-by-inlining chunk workers. Each layer's encoding follows from
what consumes it, which is the CKS lesson applied structurally rather
than dogmatically.

## References

- Jacques Carette, Oleg Kiselyov, Chung-chieh Shan. *Finally tagless,
  partially evaluated: tagless staged interpreters for simpler typed
  languages.* JFP 19(5):509–543, 2009.
- Walid Taha, Tim Sheard. *Multi-stage programming with explicit
  annotations.* PEPM 1997.
- Walid Taha, Tim Sheard. *MetaML and multi-stage programming with
  explicit annotations.* Theoretical Computer Science
  248(1–2):211–242, 2000.
