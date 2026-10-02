# Resource

Things that must be released: files, connections, locks. A resource is
acquired inside a region, and everything acquired is released at the end
of the region, in reverse order, including when the program fails or is
aborted.

## Operations and handlers

| | |
|---|---|
| `Resource.acquire(make)(release)` | acquire, and register its release |
| `Resource.scoped(p)` | run a program, then release everything it acquired |
| `p.handle(Resource.region)` | handle it, forwarding the program's other effects |
| `Resource.run(p)` | the same, forwarding the program's other effects |
| `Resource.open(p)` | run it and hand back the answer and a `close` |
| `bracket(acquire)(release)(use)` | the classic three-part form, over any row |

## Example

```scala
var log = Vector.empty[String]
val sum: Int ! Resource =
  for
    a <- Resource.acquire({ log :+= "open a"; 1 })(_ => log :+= "close a")
    b <- Resource.acquire({ log :+= "open b"; 2 })(_ => log :+= "close b")
  yield a + b

val three = Resource.scoped(sum)   // 3, and log is Vector(open a, open b, close b, close a)
```

## Notes

- Release survives a handled abort and an exception in the middle of a
  step.
- The region is lexical: a handle stored somewhere and used after the
  region ends is not prevented by the types yet (backlog
  `resource-regions`).

See also: `src/main/scala/Resource.scala`.
