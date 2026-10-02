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
| `Handler.control[F, O](ret)(f)` | `ret: [A] => A => O[A]`, and `[X, A, G[+_]] => (F[X], X => O[A] ! G) => O[A] ! G`: call `resume` once, twice, or not at all | `Handler[F, O]` | `Effects.handle` |

`Answers[F]` is form 1's argument already: `Handler.answer(answers)`.

## Behavior

- [ ] `answer`: docs' `Users` over a Map, and Reader re-expressed, the same answers as `Reader(r)`.
- [ ] `state`: a counter of `Users` operations, and State's get/set re-expressed, `(S, A)` as `State(s)`.
- [ ] `into`: `Users` into `State % Map[Long, String]`, then `State(…)`.
- [ ] `control`: Maybe re-expressed (`resume` dropped), Choose re-expressed (`resume` twice), Throws
      re-expressed, the same answers as the built-ins.
- [ ] forwarding: every form leaves the rest of the row to its own handler, in order.
- [ ] stack: 100 000 operations through each form.
- [ ] JMH, one lane each, against the built-in code: Reader (`answer` vs `Reader(r)`), State (`state` vs
      `State(s)`), Maybe (`control` vs `Maybe.option`).

## Decisions

## Results
