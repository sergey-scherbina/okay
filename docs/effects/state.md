# State

A value the program reads and replaces as it goes: a counter, a parser's
position, an accumulator. `A ! State % S` reads "computes A, with an S
it may change".

## Operations and handlers

| | |
|---|---|
| `State.get[S]` | the current state |
| `State.set(s)` | replace it |
| `State.modify(f)` | replace it by a function of itself |
| `State.update(f)` | answer something and replace the state in one step |
| `State.run(s)(p)` | handle it: answers `(final state, answer)` for a program with no other effects |
| `State.handle(s)(p)` | the same, forwarding the program's other effects |
| `State.zoomWith(look, put)(p)` | run a program on one part of a bigger state |

The handler is a bespoke tail-recursive loop, so a long run of `get` and
`set` does not grow the stack.

## Example

```scala
val next: Int ! State % Int =
  for
    n <- State.get[Int]
    _ <- State.set(n + 1)
  yield n

val twice = State.run(10)(next.flatMap(a => next.map(b => (a, b))))   // (12, (10, 11))

val doubled = State.run(5)(State.modify[Int](_ * 2))   // (10, 10)
```

## Notes

- A row holds ONE `State % S`: its operations carry no runtime trace of
  `S`, so two states in one row cannot be told apart. For two, name them
  ([several instances](../many-instances.md)).
- `PState` is the type-changing version, where `set` may change the type
  of the state (typestate). It lives in the same file.
- Under a search ([Choice](choice.md)), handler order decides whether
  each branch has its own state or all share one.

See also: `src/main/scala/State.scala`, [Reader](reader.md),
[Supply](supply.md) for a state that may only be drawn from.
