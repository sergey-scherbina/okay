## okay-arrow-types - okay-arrow's model holds every type okay's data takes, on every platform

okay-arrow stage 4 (specs/okay-arrow.md), per the operator's "Arrow wherever
it makes sense, on JVM, JS and Native":

- `Column` grows from five kinds to sixteen: ints of every width signed
  and unsigned, float32 (float16 read), binary and fixed-size binary,
  decimal128, date32/64, timestamp with unit and zone, duration, list and
  struct (nested at any depth), beside int64, float64, utf8, bool and
  null. Large forms and dictionary-encoded columns are read (decoded).
  `Column.take` and `Column.concat` work on every kind.
- `OkayArrow` writes and reads all of it on the JVM, Scala.js and Native;
  `ApacheArrow` maps all of it through Arrow Java's own vectors.
- pyarrow validates all 25 Arrow types we write and its own writer's
  stream reads back the same; each implementation reads the other's.
- okay-py's `ArrowFrames` maps the lossless new kinds to `PyValue` and
  refuses by name the ones a PyFrame cannot say.
