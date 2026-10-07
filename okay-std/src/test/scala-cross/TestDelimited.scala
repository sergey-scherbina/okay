package okay.freer

/**
 * specs/delimited.md: the machine through its interface. Every program
 * here is written against `LambdaDollar[M]` ALONE, and runs on two
 * instances — the frame machine and the reference (DelimitedReference:
 * a list context, nothing subtle) — which must agree. The one thing the
 * interface adds over `k(a)` is pinned too: RESUMING WITH A COMPUTATION
 * (DPJS's `pushSubCont`, "throwing into a continuation"), whose
 * operations run inside the stack `k` carries, its delimiters in force.
 */
abstract class DelimitedLaws[M[_, _, _]](name: String, val D: LambdaDollar[M]) extends munit.FunSuite:

  type Str = M[String, String, String]

  extension [A, S, R](m: M[S, R, A])
    def map[B](f: A => B): M[S, R, B] = D.bind[A, B, S, S, R](m)(a => D.pure[B, S](f(a)))
    def flatMap[B, S2](f: A => M[S2, S, B]): M[S2, R, B] = D.bind(m)(f)

  def outcome(p: Str): String = try D.run(p) catch case _: NoPrompt => "NoPrompt"

  test(s"$name: k(a) is resume(k)(pure(a))") {
    def prog(useResume: Boolean): Str =
      val p = D.delimiter[String, String]
      D.reset[String, String, String](p)(
        D.shift0[String, String, String, String, String](p)(k =>
          if useResume then D.resume(k)(D.pure[String, String]("a")) else k("a")).map(_ + "!"))
    assertEquals(D.run(prog(true)), "a!")
    assertEquals(D.run(prog(false)), "a!")
  }

  test(s"$name: k twice, and the dollar's ret once per resumption") {
    val p = D.delimiter[String, String]
    val body: Str = D.shift0[String, String, String, String, String](p)(k => k("a").flatMap(x => k("b").map(y => x + y))).map(_ + "!")
    assertEquals(D.run(D.dollar[String, String, String, String](p)(s => D.pure(s"<$s>"))(body)), "<a!><b!>")
  }

  test(s"$name: shift runs its body under the delimiter, shift0 does not") {
    def probe(zero: Boolean): String =
      val p = D.delimiter[String, String]
      val inner: Str = D.shift[String, String, String, String, String](p)(_ => D.pure("inner-caught"))
      val body: Str =
        if zero then D.shift0[String, String, String, String, String](p)(_ => inner)
        else D.shift[String, String, String, String, String](p)(_ => inner)
      outcome(D.reset[String, String, String](p)(body))
    assertEquals(probe(zero = false), "inner-caught")
    assertEquals(probe(zero = true), "NoPrompt")
  }

  test(s"$name: abort leaves the delimiter with its value and skips a dollar's ret") {
    val p = D.delimiter[String, String]
    val body: Str = D.abort[String, String, String](p)("gone").map(_ => "never")
    assertEquals(D.run(D.dollar[String, String, String, String](p)(s => D.pure(s"ret($s)"))(body)), "gone")
  }

  test(s"$name: RESUME WITH A COMPUTATION — an abort to a delimiter only k carries, run inside k") {
    // reset_p ( q-ret $ ( shift0_p k. resume k m ) ): `q` is installed
    // INSIDE the captured context, so no `k(a)` could reach it; the
    // computation handed to resume runs where q is in force
    def prog(m: Str => Str): Str =
      val p = D.delimiter[String, String]
      val q = D.delimiter[String, String]
      D.reset[String, String, String](p)(
        D.dollar[String, String, String, String](q)(s => D.pure("q:" + s))(
          D.shift0[String, String, String, String, String](p)(k => D.resume(k)(m(D.pure("x"))))))
    assertEquals(D.run(prog(identity)), "q:x", "resumed with a value: q's ret on the way out")
    // the abort needs q itself: build the program around one q
    val p = D.delimiter[String, String]
    val q = D.delimiter[String, String]
    val thrown: Str =
      D.reset[String, String, String](p)(
        D.dollar[String, String, String, String](q)(s => D.pure("q:" + s))(
          D.shift0[String, String, String, String, String](p)(k => D.resume(k)(D.abort[String, String, String](q)("thrown")))))
    assertEquals(D.run(thrown), "thrown")
  }

  test(s"$name: RESUME WITH A COMPUTATION — a capture inside it finds k's delimiter, twice resumed") {
    val p = D.delimiter[String, String]
    val q = D.delimiter[String, String]
    val inside: Str =
      D.shift0[String, String, String, String, String](q)(k2 => k2("y").flatMap(a => k2("z").map(b => a + b)))
    val prog: Str =
      D.reset[String, String, String](p)(
        D.dollar[String, String, String, String](q)(s => D.pure(s"<$s>"))(
          D.shift0[String, String, String, String, String](p)(k => D.resume(k)(inside))))
    assertEquals(D.run(prog), "<y><z>")
  }

  test(s"$name: a capture naming a delimiter nobody installed is NoPrompt") {
    val p = D.delimiter[String, String]
    assertEquals(outcome(D.shift0[String, String, String, String, String](p)(k => k("a"))), "NoPrompt")
  }

/** Shift on the machine through the interface */
class TestDelimitedMachine extends DelimitedLaws[LambdaDollar.OnShift]("machine", LambdaDollar.machine)

/** the reference: the same laws, the same answers */
class TestDelimitedReference extends DelimitedLaws[DelimitedReference.P]("reference", DelimitedReference.Ref)
