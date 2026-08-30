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
