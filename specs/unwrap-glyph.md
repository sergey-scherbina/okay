# `.?` — one glyph, one meaning

## Overview

Three different `?` reach a value in this library today, and on
`A ! F` all three are candidates:

| where | signature | means |
|---|---|---|
| `Throws.scala` | `extension [A, E <: Unsafe](a: A throws E) inline def ? : A` | the value, or the error **thrown** |
| `Effects.scala` | `extension … def ? : Handler[F] ?=> ?` (inside `object !`) | **peek** the nearest answer, running operations through a `Handler` |
| `Direct.scala` | retired | **bind** the program into the enclosing block |

`Direct` retired its `.?` on purpose and says so in its own comment —
*".! shadowed object !, .? was ambiguous with the Throws row-?, and
.!? — once retired as redundant beside .? — is the one symbol that
collides with nothing"*. So the block mark is spelled `.!?` not
because binding is a different idea from unwrapping, but because the
glyph was already taken twice.

**The three are one idea.** Each takes a value with something around
it and hands back what is inside, leaving the surrounding context to
deal with the rest: `Throws` throws, the peek runs a handler, the mark
binds. That is Rust's `?` — unwrap here, propagate outward — and it is
worth one glyph rather than three spellings and a retirement.

**The defect that makes this urgent, and it is not cosmetic.**
`throws` is `into opaque infix type throws[+A, +E <: Unsafe]`
(Throws.scala:133) with a `Conversion[A, A throws E]` in its
companion. `into` grants that conversion without a per-call-site
import, so **every** value is a `A throws Nothing`, and therefore
`x.?` type-checks on **anything** and is a silent no-op when `x` is
not an error-carrying value.

That is not theory. Writing stage 3's tests
(specs/applicative-static.md) I wrote `leaf(latch, 200, 1).?` inside a
`direct` block, believing `.?` was the mark because
**specs/direct-macro.md's Interface section still documents it as
one** — while that same file's Decisions section says it was retired.
The block compiled. It ran. It gave the right answers. It bound
through auto-coloring, not through any mark, and `.?` had done
nothing at all. The tests only failed later, on a fork COUNT, and the
`.?` was found by dumping the macro's input.

A no-op that compiles is worse than an ambiguity, because an
ambiguity is a compile error and this was silence.

## Interface

The end state this spec argues for. Nothing here is implemented yet;
every stage below is gated on the measurement before it.

```scala
// Throws.scala — unchanged in meaning, refused where it means nothing
extension [A, E <: Unsafe](a: A throws E)
  inline def ?(using NotGiven[E =:= Nothing]): A = unwrap

// Effects.scala — the peek gets a word, and gives up the glyph
extension [A, F[+_]](self: A ! F)
  def peek: Handler[F] ?=> ?          // was `?`

// Direct.scala — the mark takes the glyph back
extension [F[_], A](m: F[A])
  def ? : A                            // was `.!?`
  def reflect: A                       // the word, unchanged
  def unary_! : A                      // the prefix, unchanged
```

After it, `.?` reads the same way everywhere — *give me the value; the
context deals with what was around it* — and `.!?` retires as the
crutch it was.

## Behavior

Stage 1 — the measurement that decides everything after it:
- [ ] `NotGiven[E =:= Nothing]` on the no-arg `?` compiles the whole
      family, or the lane stops here and says what broke. Predicted
      before measuring: **no call site moves at all.** The tree has
      seventeen lines carrying a no-arg `.?`; three are Throws'
      (TestThrows.scala:23, :32, :43) and each was read: `y: String
      throws Safe`, `runThrows[Int, Nothing, Unsafe](...)` whose
      result is `Int throws Unsafe`, and a for-comprehension over
      `half: Int throws Unsafe`. Every one has a real `E`. The other
      fourteen are the Effects peek and are not this extension at all.
- [ ] A value that is not error-carrying REFUSES the glyph:
      `compileErrors("42.?")` is non-empty. This is the defect this
      spec exists for, and it is the one test that must fail first.
- [ ] A genuine `A throws E` is unaffected: `div(84, 2).?` still
      answers 42 and still throws on the error road.

Stage 2 — the peek gives up the glyph:
- [ ] `peek` is the name; every call site moves — fourteen lines
      today: Effects.scala internally ×2, TestGenerate ×3,
      TestEffects ×2, TestCont ×1 (a line carrying three, `.?.?.?`),
      FibBenchmark ×4, HandlerBenchmark ×2. None is in a module; the
      peek has never been used outside the core's own tests and
      benchmarks, which is itself part of the argument below.
- [ ] Nothing else changes: the same programs answer the same values,
      and `src/jmh/history.tsv` gains no row, because a rename cannot
      move a number. If one moves, the rename was not a rename.

Stage 3 — the mark takes the glyph:
- [ ] `direct { val x = m.? }` binds, at every carrier the existing
      direct tests use, with `.!?` and `.reflect` still working.
- [ ] `.?` OUTSIDE a direct block on a program fails — at compile time
      if the types allow it, at run time with the mark's existing
      "outside a direct block" message otherwise. Pinned either way,
      because this is the position the peek used to occupy and
      somebody will write it.
- [ ] The three spellings agree: `m.?`, `m.!?` and `m.reflect` emit
      the same tree (compare the answers AND a recording handler's
      log, as TestStatic compares toFree against the hand-written
      program).
- [ ] No ambiguity between `Throws.?` and the mark on any type in the
      family — which is the claim Direct.scala's comment made in the
      other direction, so it must be re-tested rather than assumed.

Stage 4 — the record:
- [ ] specs/direct-macro.md's Interface stops contradicting its own
      Decisions. **This is the root cause of the incident above and is
      worth doing even if every other stage is refused.**
- [ ] docs/direct-style.md, docs/typepedia.md and docs/tutorial.md say
      one glyph, and the three-strikes history moves into the spec's
      Decisions where a reader looks for it.

## Out of scope

- The prefix `!p` mark. It shadows nothing, reads as "perform" for
  rows, and is not in the way.
- `?(f)` — the mending form, `a.?(recover)`. It takes an argument, so
  it never competed for the postfix position, and it stays.
- `??` (normalize to Either) and `.wrap`. Different job, different
  glyph, 38 explicit uses.
- Removing `into` from `throws`. The modifier is doing its designed
  job for the four ABSORBING conversions (a value, an error, an
  Either, a Try, all into the union) and Throws.scala argues for it
  carefully. The defect is not that the conversion exists; it is that
  an ELIMINATOR is offered on the converted result. Stage 1 closes the
  eliminator, not the conversion.
- Auto-coloring. It is the reason the bad `.?` still produced correct
  answers, which is why the incident was quiet — but it is a separate
  feature with its own opt-in, working as designed.

## Design

**Why refuse `E = Nothing` rather than detect the conversion.** A
macro could ask whether the receiver needed a conversion; a
`NotGiven[E =:= Nothing]` asks something simpler and more honest.
`A throws Nothing` is *a value that cannot throw*. `unwrap` on it can
only be the identity. Refusing it costs nothing real and closes the
whole class: any receiver that reached the extension by conversion
has `E = Nothing`, because the conversion is `Conversion[A, A throws
E]` with nothing to fix `E`.

**Why the peek is the one that must be renamed, not the mark.**
Three tests, all pointing the same way. The peek is used fourteen
times and not once outside the core's own tests and benchmarks, while
the mark is written by every user of direct style. The peek's name says nothing
about what it does (it runs operations through a `Handler` — a
`.peek` that *performs effects* deserves the word that warns you);
the mark's `.?` is the shape users arrive with, from Rust and from
this repository's own stale spec. And the peek's job is inspection,
which is exactly the kind of thing that should be spelled out.

**Why not teach the macro to recognise the peek as a mark.** It was
considered and refused. Inside a `direct` block over `Async` a
`Handler[Async]` is in scope, so `m.?` would type-check as the peek
and the macro could rewrite that application into a bind — one glyph,
no renames. It fails for any row with no `Handler` in scope, which is
most of them, and the failure would be a confusing type error about a
missing `Handler` in a block that never wanted one. A feature that
works only when an unrelated given happens to be in scope is worse
than a rename.

**Ordering, and why stage 1 is first.** Stage 1 is the only stage that
can fail for a reason nobody predicted: it changes the applicability
of a public extension across ninety modules. Stages 2 and 3 are
renames whose blast radius is already counted (12 and 0 call sites).
If stage 1 refuses, stages 2 and 4 still stand on their own — the
peek's name is bad regardless, and the stale spec is a defect
regardless — and only stage 3 is lost, leaving `.!?` in place but no
longer beside a silent no-op.

## Decisions

- **The glyph goes to the mark, the word goes to the peek** — chosen
  because usage points that way (12 peek call sites, all in tests and
  benchmarks, against every direct-style user), and because a name
  that performs effects should say so. Rejected: renaming the mark
  instead (it is the one users write), and leaving both (the collision
  is what retired `.?` in the first place).
- **Refuse `E = Nothing`, keep `into`** — chosen because the defect is
  the eliminator on a converted value, not the conversion. Rejected:
  dropping `into` (the four absorbing conversions are the type's
  reason to exist, and Throws.scala's comment argues the case
  carefully), and a macro-side receiver check (more machinery, same
  answer).
- **Staged, with stage 1 as the gate** — chosen because only stage 1
  can surprise. Rejected: one lane for all four (a rename held hostage
  by a subtyping question).
- **The stale spec is fixed first if anything at all is done** — it is
  three lines, it is the cause of the incident that opened this file,
  and it is the one change with no risk.

## Results

Stage 0 (this spec): written 2026-09-17, out of an incident in the
applicative-static stage 3 lane. Counts in the Behavior section were
taken from the tree at that date and are the bars the stages are
measured against.
