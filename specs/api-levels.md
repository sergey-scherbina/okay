# The three levels of the API

Operator, 2026-10-02: "a single, coherent API and implementation for continuations, effects and handlers, so
that users can write code with continuations and effects in the monadic and in the direct style, and all of
it is simple and clear". Who sees what is decided here. How the continuation half was built is
specs/shift-effect.md.

## The levels

1. **The user** uses ready effects and continuations and writes no effect of their own. The user sees one
   type and six words, by default (top level in `okay`) and through the typeclass `Effects[M]`, in both
   styles.
2. **The effect author** declares a signature and writes its handler. This level sees `Cont` (a handler's
   clause is a `Cont[X, B ! G, B ! G]`, and `Cont.shift`/`Cont.reset` are its capture and delimiter, with
   answer-type modification), `Effects.handle(m)(ret)(clause)`, `!.relay`, `split`, `Delim`'s named prompts,
   `Handler[E, O]` / `Handler.Full` (a handler as a level-1 value), `Answers[F]` (an answer per operation) and
   `Shift.cont`/`Shift.embed`.
3. **The library** is `Freer`, `Cont0`, `Delimited`, `Frames`/`Stack`, `StackSwitch`, `ContMacro` and the
   `direct` macro.

## Level 1, the whole of it

| | default (top level) | through `Effects[M]` |
|---|---|---|
| a program | `A ! F`, rows `F + G` | `M[F, A]` |
| a value | `pure(a)` | `E.pure(a)` |
| an operation | ready ops; `perform(op)` / `op.perform` | `E.perform(op)` |
| a capture | `shift[A]`, `shift0[A]` inside `reset { … }`; `shift[R, A, F]` elsewhere (`Shift % R` in the row) | `E.shift`, `E.shift0` |
| a delimiter | `reset` = `handle(Reset[R])` | `E.reset` |
| take an effect off | `p.handle(h)`, `p.handle(h1, h2)`: `State(s)`, `Reader(r)`, `Writer.log`, `Throws.either`, `Choose.all`, `Maybe.option`, `Once.memo`, `Resource.region`, `Fresh.counter`, `Supply.from`, `Prob.exact`, `Chronicle.verdict`, `Async.blocking`, `Reset[R]` | `E.handle(p, h)` |
| named patterns | `Shift.exit(v)`, `Shift.collect { … Shift.emit(w) … }` | — |
| the value | `p.run` | `E.run(p)` |
| direct style | `direct { … }`, `.?` or auto-colouring | `direct` over `Effects.monad[M, F]` |

The user page is docs/effects-and-continuations.md. Every example line on it is pinned by
TestDocExamplesLevel1 and TestDocExamplesLevel1Direct.

## Decisions

- **A continuation is an effect** (specs/shift-effect.md): `Shift % R` in the row, `reset` its handler. Level
  1 has no second type for continuations, and adding an effect to code that captures changes nothing but
  the row.
- **Level 1 takes the short names.** `shift`/`reset` were Cont's and are now `Cont.shift`/`Cont.reset`
  (cont-shift-rename). Level 2's general `Effects.handle(m)(ret)(clause)` keeps its name as an overload of
  level 1's `handle(m, h)`.
- **One `handle`, its handlers values** (handle-handler-values). The effects' own runners (`State.run`,
  `runEither`, …) stay beside it.
- **`perform(op)` is `op.perform`**: an extension method is a function too, so the top level needs no second
  definition (a second one is a double definition, E120). TestDocExamplesLevel1 calls it both ways.

- **The name `Handler` is the literature's** (handler-rename): the value that takes an effect off the row,
  `Handler[E, O]` (`Handler.Full[E, I, O, N]` in full; it was `Handling`). An answer per operation with no
  continuation, `F ~> Id`, which held the name before, is `Answers[F]`.

## Open

- None left of level 1's own. Inside `reset { … }` a `shift` names its value type only (shift-in-scope).

## In okay2 (Scala 2.13), 2026-10-02

The same three levels (okay2-level1-api, specs/okay2.md stage 49): `Shift[R]`
with `shift`/`reset`, handler values with `p.handle(h)`, the four forms and
`Effects` with level 1 in the trait. What Scala 2 changes: `p.handle` is a
whitebox macro (an intersection's rest cannot be inferred), `{ case … }` is
checked at `F#Op[Any]` and refuses a caller-chosen answer by name, whose
handler is a `.poly` clause, an anonymous class (no polymorphic function
literal; okay2-handler-case-form), a `shift` names its
answer, value and row (no context functions), and `reify`/`reflect` are
`Effects.` members. docs/okay2.md §4, §6, §23.
