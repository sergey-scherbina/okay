## okay2-split-at-rest - every okay2 handler loop splits by `Split.at`, and `Producer.each` is stack-safe

The loops okay2-handler-allocs did not reach now match the split as a
name-based pattern. `Split.split` answered through two closures, a
`Tuple2` and an `Either` per handled operation. The loops moved:
- the Delim machine;
- `Producer.fold`/`foldUntil`/`each`/`streamIn`;
- `Writer.unconsIn`/`expand`/`mapAt`;
- `Logic.msplit`, `Stream.iterator`, `Effects.translate`, `State.zoomAt`;
- `SharedOnce`, `Source.runForeach`, `Channel.feedFlushing`,
  `Flush.map`;
- the `Take` pipes;
- fs2/zio interop.

`Producer.each` threw StackOverflowError at 200 000 productions on
master: it recursed inside the split's closure. It is a `@tailrec`
loop now (TestGenerate). The same text in okay was safe because okay's
`split` is inline (producer-each-stack).

Not measured: the box never went quiet in an hour. The four new bench
lanes and the measurement are backlog `okay2-split-at-rest-measure`
(specs/okay2.md stage 45).
