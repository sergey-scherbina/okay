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
// Throws.scala — unchanged in meaning, reachable only from the type
// that means it. NOT a side condition: see the REFUTATION below.
object throws:
  extension [A, E <: Unsafe](a: A throws E)
    inline def ? : A = a.unwrap                       // moved here
    inline def ?(f: E | Unsafe => A): A = a.handle(f) // and this, with it

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
- [x] The fix compiles the whole family and **no call site moves at
      all** — the prediction held, though the mechanism is not the one
      predicted. `NotGiven[E =:= Nothing]` was tried first and is
      REFUTED (see Results); the glyphs moved into `object throws`
      instead.
- [x] A value that is not error-carrying REFUSES the glyph:
      `compileErrors("42.?")` is non-empty (TestUnwrapGlyph — it
      failed first, which is what made it a test).
- [x] A genuine `A throws E` is unaffected: TestThrows is untouched
      and green, `div(84, 2).?` still answers 42.

Stage 2 — the peek gives up the glyph:
- [x] `peek` is the name; every call site moved — fourteen lines
      today: Effects.scala internally ×2, TestGenerate ×3,
      TestEffects ×2, TestCont ×1 (a line carrying three, `.?.?.?`),
      FibBenchmark ×4, HandlerBenchmark ×2. None is in a module; the
      peek has never been used outside the core's own tests and
      benchmarks, which is itself part of the argument below.
- [x] Nothing else changes: the same programs answer the same values,
      and `src/jmh/history.tsv` gains no row, because a rename cannot
      move a number. If one moves, the rename was not a rename.

Stage 3 — the mark takes the glyph:
- [x] `direct { val x = m.? }` binds, at every carrier the existing
      direct tests use, with `.!?` and `.reflect` still working.
- [x] `.?` OUTSIDE a direct block on a program fails — at compile time
      if the types allow it, at run time with the mark's existing
      "outside a direct block" message otherwise. Pinned either way,
      because this is the position the peek used to occupy and
      somebody will write it.
- [x] The three spellings agree: `m.?`, `m.!?` and `m.reflect` emit
      the same tree (compare the answers AND a recording handler's
      log, as TestStatic compares toFree against the hand-written
      program).
- [x] No ambiguity between `Throws.?` and the mark on any type in the
      family — which is the claim Direct.scala's comment made in the
      other direction, so it must be re-tested rather than assumed.

Stage 4 — the record:
- [x] specs/direct-macro.md's Interface stops contradicting its own
      Decisions. **This is the root cause of the incident above and is
      worth doing even if every other stage is refused.**
- [x] docs/direct-style.md and docs/typepedia.md say
      one glyph, and the three-strikes history moves into the spec's
      Decisions where a reader looks for it.

Stage 5 — `.!?` retires after all (mark-glyph-only, operator
2026-09-25: «уберем .!? и … будем использовать именно только .?»):
- [ ] `Direct`'s `def !?` is gone, on `F[A]` and on `Gen[W]`; `Gen[W]`
      gets its own `def ? : Unit`, for the same reason it had its own
      `!?` (the generic mark would answer a `W` that never was).
- [ ] `Cont.Monadic`'s symbolic μ is `.?[B]`, not `.!?[B]`.
- [ ] The macros' mark sets and every "use the explicit marks"
      message name `.reflect / .? / !prog` and nothing else.
- [ ] `m.!?` REFUSES to compile with `Direct.*` imported
      (`compileErrors` non-empty in TestUnwrapMark), watched failing
      first while the method still exists.
- [ ] Every call site in main, test and jmh sources, and every live doc
      under docs/, is written `.?`; `git grep -F '.!?'` finds only the
      archives (CHANGELOG.md, BACKLOG-ARCHIVE.md, src/jmh/history.tsv,
      changelog.d) and the history in specs.

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

**Why the glyphs MOVE rather than carry a side condition.** The side
condition was the first design and it is refuted; the reasoning is in
Results. What replaces it is the move this file already made once, for
`map` and `flatMap`, and for the same stated reason — *at the package
level they would capture foreign receivers through the Conversion
givens*. The capture was a contest `map` could lose; for `?` it was
silent, which is why `?` should have moved with them.

The move costs nothing because `object throws` is the COMPANION of the
opaque type: its extensions are in the implicit scope of `A throws E`,
so a genuine receiver finds them with nothing imported. A receiver
whose actual type is `Int` does not, because implicit scope follows
the receiver and not the conversion target. Both `?` forms move
together — with only the no-arg one gone, `x.?` stopped being a no-op
and started ETA-EXPANDING to the mending form, which is one silent
shape traded for another.

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

### Stage 1 — landed 2026-09-17, by a different road than the spec proposed

**THE PROPOSED DESIGN IS REFUTED, and the compiler said so in one
message.** `NotGiven[E =:= Nothing]` rests on the claim that a
converted receiver has `E = Nothing`. It does not. `throws` is
COVARIANT in `E`, so the typer solves `E` to its upper bound — and the
proof is the refusal printed for a GENUINE receiver when the guard was
tried as `NotGiven[E =:= Unsafe]`:

    value ? is not a member of String throws okay.Safe.
      okay.?[A, okay.Unsafe](y)(/* missing */ summon[NotGiven[Unsafe =:= Unsafe]])

`y` was declared `String throws Safe` and the extension was
instantiated at `Unsafe`. A converted `42` lands at `Unsafe` too, so
**no condition on `E` can separate the two cases.** The guard was
tried, compiled, and changed nothing at all — the first version of
this lane shipped a test that still failed.

**What works instead**: both `?` forms move into `object throws`,
which is the companion of the opaque type. Extensions there are in the
IMPLICIT SCOPE of `A throws E`, so a genuine receiver still finds them
with nothing imported, while a receiver whose actual type is `Int`
does not — implicit scope follows the receiver, not the conversion
target. `42.?`, `"s".?`, `List(1).?` and `pure(1).?` are now compile
errors; `TestThrows` is untouched and green; **zero call sites moved**,
which is what the spec predicted for the wrong reason.

**BOTH FORMS HAD TO MOVE, and that was measured too.** With only the
no-arg `?` gone, `x.?` did not become an error: it eta-expanded to the
mending `?(f)` and came back as `(Throwable => Int) => Int`, which
munit reported as "can't compare these two types". One silent shape
traded for another. `handle` is the same operation with a name and
stays at package level.

The whole core suite is green (1122 tests, 0 failures) and no module
changed.

### Stages 2, 3 and 4 — landed 2026-09-17, in the same lane

- **The peek is `peek`.** Fourteen lines moved, all in the core's own
  tests and benchmarks, none in a module — which is what the argument
  rested on. One of the fourteen was NOT the peek and the rename
  caught it: `TestCont`'s `(example1, example2).?.?.?` is a LOCAL
  extension the test declares on a tuple of thunks, three lines above
  its use. A regex would have silently broken it; the file was read
  and reverted. (Verify scripted edits: the house rule, earning its
  keep again.)
- **The glyph is the mark.** `Direct.?` is added beside `.reflect`,
  `.!?` and prefix `!`, and `markSyms` in the macro gains it. All
  three postfix spellings emit the same program: asserted on the
  answers AND on a recording handler's log (TestUnwrapMark).
- **`.!?` stays.** It is written across the repository and its docs,
  and a mark with two symbols costs nothing; the spec's Interface had
  it retiring, which was tidier than it was useful.
- **The record is fixed**, which was stage 4 and the one thing worth
  doing regardless: specs/direct-macro.md's Interface block no longer
  contradicts its own Decisions entry, and both now carry the history
  — including the sentence that the block "used to show `.?` alone,
  and was wrong for a year". docs/typepedia.md and docs/direct-style.md
  follow.

**One thing the tests had to be split for.** `.?` on a program is the
MARK once `Direct.*` is imported, so a file that imports it cannot
also assert that a program refuses the glyph. TestUnwrapGlyph keeps
the refusals and imports nothing; TestUnwrapMark imports Direct and
asserts the mark. The split is the feature, stated: which `?` a
program gets depends on whether the block's marks are in scope, and
that is now the only thing it depends on.
