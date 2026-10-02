# Writer

Output the program produces along the way: a log, a trace, a stream of
results. `A ! Writer % W` reads "computes A, telling W".

## Operations and handlers

| | |
|---|---|
| `Writer.tell(w)` | emit `w` |
| `Writer.collect(p)` | handle it: answers `(everything told, answer)` |
| `p.handle(Writer.log)` | handle it: what was told, in order, and the answer |
| `Writer.run` / `fold` / `foldWith` | fold what is told into any result, as it is told |
| `Writer.listen(p)` / `censor(p)(f)` | see what a sub-program told / rewrite it |
| `Writer.map(p)(f)` / `expand(p)(f)` | re-tell at another type / as several values |
| `Writer.uncons(p)` | the first thing told and the rest of the program |

Telling IS streaming: a writer program is already a stream, so reading
it is a fold with no reinterpretation pass, and `uncons` pulls one
element at a time.

## Example

```scala
val steps: Int ! Writer % String =
  for
    _ <- Writer.tell("parse")
    _ <- Writer.tell("check")
  yield 42

val (log, answer) = steps.handle(Writer.log).run   // (List(parse, check), 42)
```

## Notes

- The element type is separate from the answer: a writer can compute an
  `Int` while telling `String`s.
- A generator is a writer program that can also stop early; see
  [Gen](gen.md). `okay-stream`'s `Source` is the same shape with
  `Async` added.
- Why one GADT constructor, and the five encodings tried before it:
  [existentials](../existentials.md).

See also: `src/main/scala/Writer.scala`.
