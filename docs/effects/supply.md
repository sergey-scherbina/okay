# Supply and Fresh

A source of fresh values: every draw answers one not answered before.
Names, ids, labels. `State % Long` with `modify(_ + 1)` would do the
same, but it also hands the code `set`, and code that should only draw
could rewind it. A supply has one operation.

## Operations and handlers

| | |
|---|---|
| `Fresh.next` | the next `Long`, from 0 |
| `p.handle(Fresh.counter)` | handle it |
| `Fresh.run(p)` | handle it |
| `Supply.next[S]` | the next value of any type |
| `p.handle(Supply.from(first)(step))` | handle it from `first`, each next by `step` |
| `Supply.run(first)(step)(p)` | handle it from `first`, each next by `step`; answers the value after the last one drawn and the answer |

## Example

```scala
val three = for a <- Fresh.next; b <- Fresh.next; c <- Fresh.next yield List(a, b, c)

val ids = three.handle(Fresh.counter).run   // List(0, 1, 2)

val (after, letters) = Supply.next[Char].flatMap(x => Supply.next[Char].map(y => s"$x$y")).handle(Supply.from('a')(c => (c + 1).toChar)).run   // ('c', "ab")
```

A row holds one Supply; two need names
([several instances](../many-instances.md)). This is Launchbury's value
supply, and the `Fresh` effect of fused-effects and polysemy.

See also: `src/main/scala/Supply.scala`, [State](state.md).
