# Delimited control as an effect (multi-prompt)

## Overview
`Cont` has always been the floor of this library — handlers ARE
continuations (`F !> S`). What a user program could not do was
capture its own continuation: to use shift/reset you dropped a layer.
This makes delimited control an ordinary effect, in the shape of
Dybvig / Peyton Jones / Sabry's multi-prompt framework (2007): a
prompt is a first-class tag carrying the delimiter's answer type,
`push` installs one, `shift` captures up to a NAMED prompt.

## Two corrections that shaped the design

**`push` (reset) is an OPERATION, not a handler application.** The
first sketch had reset as a function that applies a handler — and
that is wrong for multi-prompt, for a reason deeper than symmetry
with shift: capturing across an INTERVENING delimiter is the whole
point, and nested handlers cannot do it. An inner handler forwarding
a shift it does not own forwards it opaquely, leaving its own frames
out of the captured continuation. One machine has to own the entire
prompt stack, so push and shift must both reach it as operations.
(Reset-as-operation is a scoped operation in the sense of Wu,
Schrijvers and Hinze; the scoping hazard — other handlers forwarding
it blindly — is why `Delim.run` is meant to be applied at the
delimiter's own level.)

**Tags are what let several answer types share ONE row.** A signature
parameterised by its answer (`Control % R`) gives two prompts of
different answer types the same runtime class, and union splitting
here is by class — they would be indistinguishable. With the answer
type riding inside the prompt there is a single `Delim` signature,
and identity keeps the delimiters apart.

## Design
```scala
final class Prompt[R]                       // identity is the tag
enum Delim[+A]:
  case Push[R](prompt: Prompt[R], body: Any) extends Delim[R]
  case Shift[R, A](prompt: Prompt[R], f: Any) extends Delim[A]
```
The payloads are programs in the same row, which a single-parameter
signature cannot express; they are erased in the constructors and
re-typed in the machine. The smart constructors are the only way to
build these operations, so the casts are sealed module invariants —
the same discipline as Writer's phantom equation.

The machine keeps a list of segments (bind frames and prompt
markers). Our Bind nodes already reify continuations as plain
functions, so the freer tree IS the control stack; a captured segment
is turned back INTO A PROGRAM (binds become flatMaps, markers become
pushes), which is why the continuation is an ordinary value and
multi-shot comes for free. Foreign operations suspend the machine and
resume it with the same stack — the shape `State.handle` already
uses.

## The family
Two independent bits, so one operation with two flags rather than
four cases: `underPrompt` (does f's body run with the delimiter still
installed) and `delimitK` (does invoking the continuation re-install
it).

```
reset(E[shift    f]) = reset (f (x => reset E[x]))
reset(E[control  f]) = reset (f (x =>       E[x]))
reset(E[shift0   f]) =        f (x => reset E[x])
reset(E[control0 f]) =        f (x =>       E[x])
```

A correction worth recording: the first implementation was labelled
`shift` and behaved as `shift0` — it popped the delimiter for the
body. Nothing caught it, because with a single prompt and no nested
capture the two agree; the discriminating tests below now pin each
bit separately.

## Behavior
- [x] shift/reset returns the continuation as a value (`k(5) * 2`)
- [x] the captured continuation includes what follows the shift
- [x] dropping the continuation is an early exit, and effects inside
      the abandoned part do not run
- [x] multi-shot: the continuation invoked twice, and again from
      inside itself (it re-installs its own prompt)
- [x] MULTI-PROMPT: a shift escapes past an intervening delimiter;
      that delimiter's tail does not run, while what follows the
      TARGET prompt — outside it, therefore not captured — does
- [x] two prompts with different answer types in one row
- [x] other effects pass through the machine untouched, in order
- [x] a shift to an uninstalled prompt fails loudly (NoPrompt)
- [x] shift vs shift0: a second capture to the same prompt from
      inside the body finds the delimiter under `shift` and escapes
      past it under `shift0`
- [x] shift0 vs control0: a capture inside the invoked continuation
      finds a delimiter under `shift0` (k re-installs) and none under
      `control0` (a bare segment)
- [x] a NEW EFFECT defined in user code: a generator (`emit`) built
      from a prompt and `shift` alone — no signature, no handler, no
      library change — which is the payoff of having delimited
      control as an effect at all

## Adoption doctrine (operator, 2026-09-01)

Stated once here for BOTH control facilities (PState links here from
specs/typestate.md rather than restating):

- **Additive by default.** PState and Delim enter a module as an
  OPT-IN capability: a wrapper, an extra combinator, a typed
  internal — never a rewrite of the working form. Everything must
  keep working without them; a consumer that ignores the new door
  loses nothing. (Shipped precedents: Dialog's Scope — "Delim in
  Dialog, as an OPTION"; Stage.phased beside transduce, which
  stays.)
- **Primary where necessary.** Where the capability has NO
  equivalent and its absence makes the design worse, it is the
  mechanism, not an option. The known case: aborts and cancellable
  scopes ACROSS boundaries — nested handlers provably cannot
  express multi-prompt capture (this spec's first design note), so
  cross-boundary cut/cancel IS Delim. For PState the bar is the
  same and no current case clears it: typestate stays additive
  (internals, extra combinators) until a protocol exists that
  value-level guards cannot hold.

## See also
Implicit prompts — the context-function door over this machinery
(nearest-scope by nesting, E8-verified) — live in
specs/context-functions.md, ctx-prompts.

## Measured (2026-08-31, history.tsv)
1000 emits, a generator defined in USER code over Delim (a prompt and
a shift) against the native `Writer` it competes with:

| plain List (floor) | Writer (native) | Delim push only | Delim generator |
|---|---|---|---|
| 4.9 | 22.6 | 25.4 | 86.0 |

Read it as the price of universality: a DELIMITER costs about what an
operation costs (25.4 against 22.6 — installing and popping a prompt
is cheap), and what you pay for is CAPTURING and re-invoking the
continuation — 3.8x the effect written for the job. That is the
expected shape, and it is the argument for keeping specialised
effects: `Delim` is how a user defines a new one, not how the library
should implement the ones it ships.

The lane also found a real bug. The machine was split into `loop` and
`onOp`, and mutual recursion is NOT tail-optimised, so every
operation cost stack frames and a thousand nested captures threw
StackOverflowError — invisible to the tests, which nest a handful of
times. Merged into ONE `@tailrec` loop with a `step` function
answering `Either[done, (next, kont)]`; only a foreign operation now
suspends under a closure, the shape `State.handle` uses. Fixing it
also took 20% off the delimiter lane (33.8 to 25.4).

## Decisions
- **A separate signature from `Control[M]`,** the tagless interface —
  hence the name `Delim`. The floor stays the floor; this is the
  user-facing door to it.
- **`shift`, not `control`/`shift0`**: the captured continuation
  re-installs its prompt, which is the semantics most people mean and
  the one the tests pin.
- Stack: the machine's loop is tail-recursive, and a shift body that
  eagerly re-enters its continuation costs frames — the library's
  standing contract, unchanged.

## Out of scope
- static prompt scoping (a shift naming an uninstalled prompt is a
  runtime error, as in every multi-prompt implementation without a
  region system);
- `control`/`shift0`/`prompt0` variants — the machine can express
  them (do not re-install the marker in the captured segment) if a
  use appears.
