## stack-safety-catch-up-okay2 - no UNAUDITED recursion left in okay2; jdbc, Typed.fits and SparkSchema safe in both cores

- `JdbcSql.valueOf` walks a driver's nested arrays and `jdbcOf` walks a
  user's `SqlValue`. Nothing bounds a value, so both run on explicit
  stacks. At 200 000 levels they overflowed before.
- `Typed.fits` compares two type trees. It is now a worklist of pairs.
- `SparkSchema` refuses a type nested past `MaxNesting` (64, Arrow's own
  limit) at every door, before any walk starts. Its walks are BOUNDED
  rows now.
- The fs2/ZIO `again`s are TRAMPOLINED by the library's own lazy
  `++`/`flatMap`. New tests drive 200 000 non-Writer operations through
  each.
- Both cores. 14 inventory rows were paid, 8 marked BOUNDED and 2
  TRAMPOLINED.
- Found on the way: `java.util.ArrayDeque` on Scala.js returns null
  from a non-empty deque (192 of 66 667 pops). The cross `Typed.fits`
  uses Scala's own `Stack`; filed as `scalajs-arraydeque-null`.
