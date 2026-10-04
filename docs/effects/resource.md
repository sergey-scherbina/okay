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

## Through a capture: `abort`, and a `k` that is dropped

A scope can sit inside a delimited continuation: a `shift` or an `abort`
to a prompt OUTSIDE the scope captures the scope along with everything
else up to the prompt. When that continuation is resumed, the scope goes
on and releases at its end as usual. When it is DROPPED, the scope must
still release, and the only one who can know it was dropped is whoever
dropped it.

`abort` knows. It throws `Shift.Discontinued` into the piece it drops,
where the piece was captured, and only then answers its value. Each scope
in the piece releases what it holds, inner first, and passes the throw
on. A `try` (`Throws`, `CanTry`) does not answer it: this is not a failure
of the code there, and recovering from it would run on in a continuation
that nobody resumes. If a release fails, the `abort` fails with that
failure.

```scala
def scope[A](log: scala.collection.mutable.ArrayBuffer[String], name: String)(body: String => A ! Resource + D): A ! D =
  Resource.run[A, D](Resource.acquire(name)(x => log += s"release $x").at[Resource + D].flatMap(body))

val r = !.run(Shift.run[String, Pure](Shift.push[String, Pure](p)(
  scope(log, "outer")(_ => scope(log, "inner")(_ =>
    Shift.abort[String, String, Pure](p)("aborted").at[Resource + D]).at[Resource + D]))))
assertEquals(r, "aborted")
assertEquals(log.toList, List("release inner", "release outer"))
```

A `shift` body that does not call `k` is another matter. Nothing can tell a
`k` that was dropped from one that was STORED to be resumed later (a
generator, a dialogue). Releasing at the capture would break the second,
so the body says which it is: `Shift.discontinue(k)` drops `k` the way
`abort` does.

```scala
scope(log, "a")(_ => Shift.shift[String, Int, Pure](p)(k =>
  Shift.discontinue(k).map(_ => "dropped")).at[Resource + D].map(_.toString)))))
assertEquals(r, "dropped")
assertEquals(log.toList, List("release a"))
```

A `k` that is neither resumed nor discontinued keeps its scopes open. This
is the contract OCaml 5 states for its continuations (`continue` or
`discontinue`, exactly once). The design follows Leijen, "Algebraic Effect
Handlers with Resources and Deep Finalization" (MSR-TR-2018-10): a
resumption that will not be resumed is finalized by running it with a
throw that only the finalizers inside it act on. Racket's `dynamic-wind`
is the contrast: it runs its post-thunk whenever control leaves, and its
pre-thunk again on re-entry. A resource cannot be acquired again by
re-entering, so here leaving by a capture releases nothing until the
continuation is known to be dropped.

## Shared by branches: a resource before a `choose`

A scope that acquires and THEN chooses among alternatives hands the same
resource to every branch the search starts. It was acquired once, so it
is released once: when the last branch that can use it is done. Every
other effect of a search's prefix behaves the same way: it runs once,
when the search crosses it.

```scala
def shared(log: Log)(xs: Seq[Int]): Int ! R =
  Resource.run[Int, R](Resource.acquire("a")(x => log += s"release $x").at[Resource + R].flatMap(_ =>
    effect[Choose, Int](Choose(xs)).at[Resource + R].map { x => log += s"use $x"; x }))

assertEquals(!.run(runChoice[Int, Pure](shared(log)(Seq(1, 2, 3)))), Seq(1, 2, 3))
assertEquals(log.toList, List("use 1", "use 2", "use 3", "release a"))
```

A search that stops early still releases it once:

```scala
assertEquals(!.run(runChoice[Int, Pure](cut[Int, Pure](shared(log)(Seq(1, 2, 3))))), Seq(1))
assertEquals(log.toList, List("use 1", "release a"))
```

How it works: each acquisition counts its holders. The path that took it
is one holder. When the choice passes out of the scope, that holder moves
to the BRANCH POINT, and every branch the handler starts becomes one more
holder. A branch that ends (its value, a throw, a final operation, a
discontinue) lets go of its hold. The branch point lets go when every
alternative has started, or when the rest of the search is ABANDONED.
`Logic.cut` and `observe` abandon what they drop, and so does a throw
that leaves `runChoice` or `msplit`. A handler of one's own that stops
early says `Choose.abandon(c)`, or `Logic.abandon(rest)` for a rest that
`msplit` handed out. One that stops without saying so keeps the resource
open, the same contract as a dropped continuation.

A scope INSIDE a branch is the branch's own: it is acquired and released
once per branch, as before.

## Notes

- Release survives a handled abort and an exception in the middle of a
  step.
- The region is lexical: a handle stored somewhere and used after the
  region ends is not prevented by the types yet (backlog
  `resource-regions`).

See also: `src/main/scala/Resource.scala`.
