# Gen

Generators, Python's `yield`: a body hands values out one at a time, runs
only as far as the reader asks, and may end itself early. A generator is
a [Writer](writer.md) program with `Stop` beside it, so each value told
suspends the body until the reader wants the next.

## Operations and readers

| | |
|---|---|
| `Gen.emit(w)` / `Gen(ws*)` / `Gen.from(iterable)` | build one |
| `Gen.stop` | end the generator here; nothing after it runs |
| `map`, `filter`, `flatMap`, `take`, `drop`, `zip`, `++` | combine, lazily |
| `toList`, `first`, `find`, `exists`, `foreach` | read; readers that stop, stop the body |
| `iterator` / `toLazyList` | step it one value at a time |

A plain for-comprehension over a `Gen` builds a generator with no macro.
A `Gen` is a value: reading it twice runs the body twice.

## Example

```scala
val evens: Gen[Int] = Gen.from(1 to 10).filter(_ % 2 == 0)

val firstThree = evens.take(3).toList   // List(2, 4, 6)

val upToFour = Gen.from(1 to 10).flatMap(i => if i == 4 then Gen.stop else Gen.emit(i)).toList   // List(1, 2, 3)
```

In a `direct` block a generator reads like Python's
([direct style](../direct-style.md)). `okay-stream`'s `Source` is the
same shape with `Async`.

See also: `src/main/scala/Gen.scala`, `specs/generators.md`.
