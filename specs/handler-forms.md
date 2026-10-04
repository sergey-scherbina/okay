# Handler forms: one door for the effect author

Operator, 2026-10-02 (level 2, specs/api-levels.md). Today an author picks among seven ways to write a
handler: `Answers[F]`, `!.relay`, `Effects.handle` with a `Cont` clause, a hand loop over `split`/`resume`,
`!.translate`/`!.interpret`, `Delim`, and `Handler.Full` to make the result a level-1 value. Each has a
different shape (a value, a `Cont`, a polymorphic function, a program). An effect with state has no form
at all: State itself is a hand loop.

## Overview

Four constructors on `object Handler`, ordered by power. Each gives a level-1 `Handler`, so the user applies
any of them as `p.handle(h)`, and each sits on the machinery that is already fastest for its case:

| constructor | the author writes | the result | over |
|---|---|---|---|
| `Handler.answer[F](f)` | `[X] => F[X] => X`: an answer, and the program goes on | `Handler[F, [A] =>> A]` | `!.relay` |
| `Handler.state[F, S](s0)(f)` | `[X] => (S, F[X]) => (S, X)` | `Handler[F, [A] =>> (S, A)]` | State's loop |
| `Handler.into[F, G](f)` | `[X] => F[X] => X ! G`: each operation a program in G | `F` replaced by G, G in the rest of the row | `!.translate` |
| `Handler[F].control[O](ret)(f)` | `ret: [A] => A => O[A]`, and `[X, A, G[+_]] => (F[X], X => O[A] ! G) => O[A] ! G`: call `resume` once, twice, or not at all | `Handler[F, O]` | `Effects.handle` |

An `Answers[F]` becomes form 1 with `Handler.from(answers)`.

## Behavior

- [x] `answer`: docs' `Users` over a Map, and Reader re-expressed, the same answers as `Reader(r)`.
- [x] `state`: a counter of `Users` operations, and State's get/set re-expressed, `(S, A)` as `State(s)`.
- [x] `into`: `Users` into `State % Map[Long, String]`, then `State(…)`.
- [x] `control`: Maybe re-expressed (`resume` dropped), Choose re-expressed (`resume` twice), Throws
      re-expressed, the same answers as the built-ins.
- [x] forwarding: every form leaves the rest of the row to its own handler, in order.
- [x] stack: 100 000 operations through each form.
- [x] JMH, one lane each, against the built-in code: Reader (`answer` vs `Reader(r)`), State (`state` vs
      `State(s)`), Maybe (`control` vs `Maybe.option`).

## Decisions

- **Builders, two roads under one name** (the operator: "работало и с макросом и без макроса"):
  `Handler.answer[F]`, `Handler.state[F, S](s0)` and `Handler.into[F, G]` return a builder. Its `apply`
  takes `{ case … }`, checked by a macro, and its `.poly` takes `[X] => …`, checked by the compiler. One
  overloaded method could not serve both, because an overload costs the lambda its expected type. The
  first cut, `answer(f)` beside `answer(answers: Answers[F])`, was refused that way. `Handler.from(answers)`
  is its own name for that reason.
- **The cases see an operation at an abstract `Answer`**, not at `Any` (the operator: "можно ли избежать
  стирания до F[Any] => Any?"). At `F[Any]` an operation whose caller chooses its answer (`Asks[R, A]`)
  types `g(1)` and `"oops"` alike as `Any`, and nothing could tell them apart. At `F[Answer]`, with
  `Answer` opaque in an object of its own (inside `Handler` the macro would see through it to `Any`), the
  typer gives `g: Int => Answer`. Only the operation's own data produces an `Answer`, so `g(1)` passes and
  `"oops"` is refused: parametricity, as the polymorphic form has it. A FIXED answer is read off the
  constructor's declaration, at the effect's parameters (`Get[R, A] extends Env[R, R]` at `Env % Int` is
  `Int`). The pattern's own type could not be used: the typer types it with a pattern-bound variable.
- **Where the case form stops**: an operation whose answer is ANOTHER parameter of the effect that one
  of its fields also has (`State`'s `Set(s: S) extends State[S, S]`). Matched at `State[Int, Answer]`, the
  typer would need both `S = Int` and `S <: Answer`. It cannot have both, so it binds `S` to a fresh
  type, and `n` in `Set(n)` is no longer an `Int`. Such an effect is written with `.poly`, and the macro says so by name ("Set
  answers the type its field `s` has … write this handler with `.poly`"). To reach the macro first, the
  state form's expected type is `(S, F[Answer]) => Any`, and the macro checks the pair itself. A body
  that uses the field (`s + 1`) still fails in the typer before the macro sees it. Operations with no such
  field (`Reader`'s `Ask()`, `Env`'s `Get()`) are fine. Why no single type serves: at `F[Any]` the
  caller-chosen answers are lost; at an abstract `F[Answer]` the tied fields are; and at an `Answer`
  bounded below by the fixed answers, `case Asks(g) => 5` would pass for a caller who chose `String`.
  Only a polymorphic function carries its own `X` per case.
- **Exhaustive as an error**: a case with a guard covers nothing, and a missing operation is a compile error
  naming it, whatever the user's `-Werror`. A `case _` may only `throw`. `.poly` keeps the compiler's own
  warning.
- **`control` answers a tail `resume` with no capture**: `resume(x)` called once and returned as the
  clause's answer is `Cont.Pure(x)`; any other use fills the captured `k` before it runs.
- **`state` is inline**: the clause expands into its call site's loop, so the JIT drops the pair it answers.

- **The effect read off the cases** (handler-infer, the operator: "p.handle(Handler.answer { case Find(id)
  => … })"). `Handler.answer { case … }` with no `[F]` is a transparent macro. Each pattern names a
  constructor, the sealed parent they share is the effect, and the result is typed `Handler[F, …]`. The
  cases are typed against `Any` (no `F` to type them against yet), so an effect with parameters besides its
  answer (`Reader % Int`: `Ask()` does not say `Int`) is refused by name and NAMED instead. That spelling is
  now `Handler[F].answer { … }` / `.state(s0)` / `.into[G]`, with `.poly` beside each. One name could not
  hold both `answer[F]` and `answer(cases)`: Scala calls that overload ambiguous. `state` keeps its effect
  named. Read off its cases, the pair's second would be an `Any`, and the compiler's exhaustiveness check
  over `(S, Any)` warns, which `@unchecked` on the component does not reach.

- **One implementation, its helpers for inference** (handler-apply, the operator: "Пусть реализация
  будет одна — старая, а новым будет только вывод типов через хелперы"). The forms are `Handler.answer[F]`,
  `Handler.state[F, S](s0)`, `Handler.into[F, G]` and `Handler.control[F, O](ret)`. `Handler[F]` names the
  effect once and delegates, filling in the types. `Handler[Accounts] { case … }` is `answer`, the default
  form, and `.state(s0)` reads `S` off `s0`. Reading the effect off the cases (handler-infer's
  `Handler.answer { … }`) is gone again: it cannot share the name with `Handler.answer[F]`, and the
  operator prefers the effect named ("явно указываем эффект который хотим обработать").

- **Seen per effect** (handler-shape): see `Handler.Seen`. `F[Any]` for an effect whose only hard operations
  answer their own field's type, `F[Answer]` otherwise. The macro skips the tied refusal at `Any`.
- **The built-ins are the forms** (builtins-through-forms): `Reader(r)` is `answerOf` and `State(s)` is
  `stateOf`, with one implementation each. Kept at 1.04x by the operator's call.
- **Several handlers at once** (handle-many): explicit evidence per step, because a `transparent inline` body is
  typed at its definition, where the row is abstract.
- **control's resume** (control-resume-node): the first call's node defers to the Resume object, -24 B.

- **The whole list, one table** (handler-api-surface, 2026-10-04). The author's page has one table of every
  way to give an effect a meaning, keyed by what the handler must do (docs/your-own-effect.md, "Which one").
  It holds the four forms, `Answers[F]` for a whole row, `!.interpret`/`!.tracing` for a row the program does
  not hold yet, and `Lexical` for two instances by name. `Effects.handle`, `!.relay` and `!.translate` are
  what the forms stand on, and `HandleFrames.stateRun`/`stateRunUntil`/`stateRunOr` are LEVEL 3: a built-in's
  fold as one step (handler-one-step). Nothing was removed. Each entry answers a question none of the others
  does, and every one has callers. The two hand loops left outside the core that were plain state folds
  (okay-agent's `Memory.handle`, okay-py's `PyStream.holding`) are one step on `stateRun` now. What is still
  hand-written re-tells an effect as another (`State.zoomWith`, `Maybe.prune`, `Writer.map`, okay-stream's
  `Source`), runs finalizers (`Resource`) or searches (`Logic.msplit`).

## Results

JMH (history.d `handler-forms`, one lane at a time, quiet box):

| form | built-in | the form |
|---|---|---|
| `answer` (Reader, 1000 asks) | 42.98 µs | 43.67 µs (1.02x), same bytes |
| `state` (State, 1000 get+set) | 11.78 µs | 18.86 µs as a function value (1.60x) → **12.30 µs inline (1.04x)**, same bytes |
| `control` (Maybe, 1000 Somes) | 31.03 µs | 67.39 µs capturing every operation (2.17x) → **38.96 µs with the tail resume (1.26x)** |
| `{ case … }` vs `.poly` (Tick, 1000) | — | answer 46.1 vs 46.6 µs, state 33.5 vs 33.2 µs, same bytes |

`control`'s remaining 1.26x is a `Resume` and a `Delay` node per operation.

## In okay2 (Scala 2.13), 2026-10-02

okay2-handler-case-form ported the case forms: `Handler[F] { case … }`,
`.answer { … }`, `.state(s0) { case (s, op) => … }`, `.into[G] { … }`, each
checked by a blackbox macro (`HandlerCases`) against the constructor's
declared answer, with the effect's parameters substituted, and `.poly(clause)`
beside each for the trait. One difference, MEASURED: scalac 2 refuses a
constructor pattern against an opaque answer ("constructor cannot be
instantiated to expected type", ProbeCaseForm, deleted after it answered),
so the core's `Answer` road is closed and the cases are typed at
`F#Op[Any]`, the core's `Seen = Any` road for every effect. There a
caller-chosen answer (`Update[S, B]`) or one tied to the operation's own
field (`Emit[A](a: A)`) has no type to check, and the macro refuses such a
case by name, pointing at `.poly`. Exhaustiveness is scalac's own warning,
an error under `-Werror` (checked by hand: "It would fail on the following
input: Save(_, _)"). specs/okay2.md stage 51.
