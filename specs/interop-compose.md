# interop-compose — one expression across cats, ZIO, kyo and okay

Status: done, 2026-10-02. Owner lane: `interop-compose`.
Follows `specs/interop-classes.md` (the class ladder both ways). The
operator's goal, in their words: compose functions written with all
these libraries in ONE expression, and call okay functions from inside
functions written with each of them, without friction.

## Interface

In (one name for every library — okay's own extension, okay-async):

```scala
trait ToOkay[-T, +A]:
  def apply(t: T): A ! Async
extension [T, A](t: T)(using ToOkay[T, A]) def asOkay: A ! Async
extension [X, T, A](f: X => T)(using ToOkay[T, A]) def asOkay: X => A ! Async
```

Instances: `Future` (core), `IO` (`okay.cats.given`, needs an
`IORuntime`), `Task` (`okay.zio.given`), kyo `A < S` for any `S` an
async run discharges (`okay.kyo.given`).

Out (one name per library, value and function forms):
`asIO` (okay.cats), `asZIO` (okay.zio, value form existed), `asKyo`
(okay.kyo).

Composition: okay's `>=>` over the `A => B ! Async` the function forms
produce; a `direct` block marks an IO and a ZIO directly (`.?`,
specs/direct-foreign-mark.md) and a kyo value after `asOkay`.

## Behavior

- [x] `parse.asOkay >=> double.asOkay >=> inc.asOkay >=> show` — cats,
      ZIO, kyo and okay functions in one chain
- [x] one `direct` block binding an IO, a ZIO, a kyo value and an okay
      program
- [x] okay called inside an IO for-comprehension (`asIO`, value and
      function), a ZIO chain (`asZIO`), a kyo for-comprehension (`asKyo`)
- [x] a round trip okay → cats → ZIO → kyo → okay in one expression
- [x] one file imports all three interops and `asOkay` resolves for
      each library's values
- [x] every existing suite of okay-cats, okay-zio, okay-kyo green (148,
      32, 18)

## Decisions

- **ONE `asOkay`, chosen by a class — not one extension per module.**
  The first cut put an `asOkay` extension in each of okay.cats, okay.zio
  and okay.kyo. Imported together they did NOT overload: the compiler
  tried only the last import's, and kyo's — whose receiver `A < S`
  every plain value converts to through kyo's implicit `lift` — claimed
  an `A => IO[B]` and a ZIO (`okay.kyo.asOkay[String, IO[Int]](
  liftPureFunction1(parse))`). One extension in okay, one `ToOkay`
  instance per library behind its given import, has neither problem.
- **Chosen by the WHOLE type, not an `M[_]` hole.** `ForeignEffect[M]`
  is keyed on `M[_]`, which kyo's `<[+A, -S]` (value first) cannot fill;
  `ToOkay[T, A]` is keyed on `T` and finds `A < S` as easily as `IO[A]`.
- **`z.asOkay` moved from okay.zio to okay.** Same behaviour
  (`ZioInterop.fromZIO`), but a caller now imports `okay.asOkay` (or
  `okay.*`); TestZioDirect's import is the one in-repo call site, and it
  is the reason the three modules' full suites ran rather than only the
  new one. Kept as a forwarder it would have collided again.
- **Everything crosses as `A ! Async`.** A typed ZIO error or
  environment is a row (`ZioRow`) and crosses by the `direct` mark;
  `asOkay` stays one shape so `>=>` can chain any two.

## Results

- TestMixed (okay-kyo, which depends on okay-cats and okay-zio in test
  scope for it), 6 tests, green; the composed answers (`"<41>"`,
  `"<20><41>"`) cannot come out if a crossing drops or reorders a step.
