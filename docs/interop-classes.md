# The class ladder across cats, ZIO and kyo

okay's `Functor`, `Applicative`, `Selective` and `Monad`
(`src/main/scala/Monad.scala`) are one ladder. Each rung up can do more
and promises less about what will run before it runs. cats has the same
ladder without `Selective`. ZIO and kyo have no type classes at all:
their combinators are methods on `ZIO` and functions over `A < S`.

So "the same classes on both sides" means two directions:

- **inward**: okay's classes answer for THEIR types, so `okay.traverse`,
  `whenS` and a `direct` block run over a cats `IO`, a `ZStream`, a kyo
  `A < S`;
- **outward**: cats' classes answer for OUR types, so cats' `traverse`,
  `mapN` and `parTraverse` run over `okay.Validated`, `Static` and `Par`
  and keep what makes each of them worth having.

Outward stops at cats, because only cats has classes.

## cats

`import okay.cats.given` brings the default instances.

okay's accumulating `Validated` under cats' `traverse` reports every
error, not the first. The instance delegates to okay's own `app` and
never goes through a `flatMap`:

```scala
val out = List(1, 2, 3, 4, 5).traverse(check)
assertEquals(out, Validated.invalid(Vector("odd 1", "odd 3", "odd 5")))
```

A free selective built by cats' `traverse` is still a `Static`. Its
operations can be listed before anything runs:

```scala
val s = List("a", "b", "c").traverse(k => Static.op(Op.Get(k)))
assertEquals(s.leaves.toList, List(Op.Get("a"), Op.Get("b"), Op.Get("c")))
```

`A ! Async` and `Par` are cats' `Parallel` pair, the same relation as
`IO` and `IO.Par`. `parTraverse` forks through `Async.par`, and plain
`traverse` stays sequential:

```scala
val p: List[Boolean] ! Async = List(1, 2, 3, 4).parTraverse(_ => leaf(latch, 10000))
```

Both `Validated`s combine their errors with whichever semigroup you
hold, okay's or cats-kernel's (`Combine`, okay's first). The general
bridges for `Semigroup`, `Monoid` and `Group` are their own imports,
`FromCatsKernel` and `ToCatsKernel`: import one only where a type has
just one side's instance. Next to `okay.given` they tie with okay's own
numeric instances.

In the other direction, cats' `Validated` gets the rung cats does not
have: `select` runs its handler only for a valid `Left`. `IO` and
`Eval` get okay's `Monad`:

```scala
assertEquals(okay.traverse(Seq(1, 2, 3))(i => IO(i * 2)).unsafeRunSync(), Seq(2, 4, 6))
```

`A ! Choose` gets cats' `MonoidK` (`<+>` is choice). The full
`Alternative` is the explicit `CatsClasses.chooseAlternative`. A given
`Alternative` would tie with the program monad for every
`cats.Applicative[A ! Choose]`.

### Any instance, either way

Two generic bridges, one import each:

- `import okay.cats.FromCats.given` gives okay's class from cats': any
  cats `Monad`, `Alternative`, `Applicative` or `Functor`, with
  `MonadPlus` when cats has both a `Monad` and an `Alternative`;
- `import okay.cats.ToCats.given` gives cats' class from okay's:
  `Monad`, `Alternative`, `Applicative`, `Functor`.

```scala
val out = okay.traverse(Seq(1, 2))(i => NonEmptyList.of(i, i * 10))
```

Never import both at once: each derives the other's instance from its
own, so the search goes in a circle. Neither is in the default import
either. A given in lexical scope is found before the type's own
instance, so a default `FromCats` would reroute okay's own
`Monad[A ! F]` through cats.

cats' `Monad` needs a stack-safe `tailRecM`. Every okay `Monad` has
one, `TailRecM` (specs/monad-tailrecm.md), and it is safe even on an
eager carrier, because each iteration's continuation is resumed by
`Cont`'s data machine and not by the host stack. So `ToCats` gives
cats' `Monad` too, and cats' own laws hold for it, their `tailRecM`
stack-safety law included.

### An okay program as cats-effect's `F`

Code written `F[_]: Async`, `Concurrent` or `Temporal` (http4s, doobie,
fs2's effectful streams) runs at `CatsEffect.Program`. Its binds are
okay's tree. cats-effect's primitives (`uncancelable`/`poll`,
`canceled`, `onCancel`, `start`, `sleep`, `cont`, `Ref`, `Deferred`) are
one effect, `CatsFx`, in the row. `CatsEffect.toIO` runs the whole
program as one IO fiber, so masking and cancellation behave as they do
in IO. cats-effect's own laws hold for it, every `AsyncTests` property.

A function that knows only `Async`, called with `F = Program`:

```scala
def releasedOnCancel[F[_]](released: AtomicInteger)(using F: Async[F]): F[Outcome[F, Throwable, Unit]] =
val oc = run(releasedOnCancel[Program](released))
```

`Program` is opaque, as `Par` is, so it has exactly one instance and
needs no import. A plain okay `A ! Async` crosses in with
`CatsEffect.lift`:

```scala
assertEquals(run(CatsEffect.lift(p)), 100000)
```

## ZIO

`import okay.zio.given`: `ZIO` and `ZStream` are okay `Monad`s. Over
streams, `traverse` is the cartesian product, the list monad's reading.
The parallel applicative is chosen at the call site, the way cats
chooses `Parallel`:

```scala
out <- okay.traverse(Seq((a, b), (b, a)))(leaf.tupled)(using ZioClasses.parApplicative)
```

It is not a given because it would tie with the monad.

## kyo

`import okay.kyo.given`: `A < S` is an okay `Monad` for every effect set
`S`. kyo puts the value first (`<[+A, -S]`), and Scala infers an `F[_]`
by filling the last parameter. So name the hole with `Pending[S]`:

```scala
val k = okay.traverse[Pending[Env[Int]], Int, Int](Seq(1, 2, 3))(i => Env.use[Int](_ + i))
```

`KyoClasses.parApplicative[E]` is the parallel reading, by
`Async.parallel`.

One caveat: `A < S` is `A | Kyo[A, S]`. A value that is itself a kyo
computation does not become a new layer under `pure`: it is that
computation. kyo refuses such an `A` with `Flat`/`WeakFlat` at concrete
call sites. A generic instance cannot ask for that evidence, so the
laws hold for every `A` that is not a `<`.

## Further reading

- Conor McBride and Ross Paterson, *Applicative Programming with
  Effects*, JFP 2008: the rung below the monad.
- Andrey Mokhov, Georgy Lukyanov, Simon Marlow and Jeremie Dimino,
  *Selective Applicative Functors*, ICFP 2019: `select`, and the
  `Validation` instance used here for cats' `Validated`.
- Simon Marlow et al., *There is no Fork: an Abstraction for Efficient,
  Concurrent, and Concise Data Access*, ICFP 2014: why a parallel
  applicative is not a monad.
- cats: [Parallel](https://typelevel.org/cats/typeclasses/parallel.html),
  [Alternative](https://typelevel.org/cats/typeclasses/alternative.html).
- In this repository: [effect interop](effect-interop.md), the specs
  `interop-classes`, `applicative-static`, `validated`.
