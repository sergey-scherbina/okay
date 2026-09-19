## cont-category - a typestate program is a semigroupoid, and one cause refuses two things

The operator asked what optics and arrows do WITH continuations. The
answer turned out to be sharper than either half.

`PState.Zooming[X, R] = [A, B] =>> Cont[X, B => R, A => R]` — a
program computing an `X` that takes its state from `A` to `B` — is a
`Strong` profunctor, which is why `PState.zoom` is an ordinary optic
run at a continuation and not a special function. That landed earlier
today. What this lane adds is where it STOPS, and why the stopping is
one fact rather than two:

  - `compose` is writable, and means what it should: two typestate
    programs in order, threading `A -> B -> C`.
  - `id` is NOT. The identity must compute an `X` while leaving the
    state alone, and `X` is universally quantified — only the inner
    program can make one. `idGiven(x)` exists; `id` needs a `???`.
  - `Choice.right` fails for the SAME reason one step along: on the
    case it must not run, it still owes an `X`.

A category without an identity is a SEMIGROUPOID, and that is exactly
what this carrier is. ONE CAUSE, TWO REFUSALS - and `Strong` survives
untouched precisely because `first` and `lens` never answer without
running the inner program. The moment an operation must answer having
run nothing, it becomes impossible. That is parametricity: an answer
belongs to whoever computed it, and no cast supplies one.

FOUND BY TRYING, not by thinking. `compose` was written first to see
whether it would typecheck (it does), and `id` was written next to see
what it would need (an `X` from nowhere). A probe is cheaper than an
argument and it cannot be talked out of its result.

`TestContSemigroupoid` pins the three claims with the `Strong` summon
beside them as the control — a refusal proves nothing unless something
in the same scope is accepted. Written up in docs/arrows.md (with the
two spellings side by side), theory ch. 10, and specs/optics.md
stage 12.
