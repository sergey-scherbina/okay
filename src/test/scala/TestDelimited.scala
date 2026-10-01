package okay

import okay.Freer.Return

/**
 * specs/delimited.md: the machine through its interface. The four
 * primitives and the derived operators, and the one thing the interface
 * adds over `k(a)`: RESUMING WITH A COMPUTATION (DPJS's `pushSubCont`,
 * "throwing into a continuation"), whose operations run inside the
 * stack `k` carries — its delimiters in force.
 */
class TestDelimited extends munit.FunSuite:

  type F = Freer.Lift[Pure]
  val M = Delimited.machine[F]
  type P[S, R, A] = Freer[Cont0.Row[F], S, R, A]
  type Str = P[String, String, String]

  def run(p: Str): String =
    Frames.run[F, String, String, String](M.reset[String, String, String](Cont0.boundary[String, String])(p)) match
      case Return(a) => a
      case other => fail(s"not a value: $other")

  test("k(a) is resume(k)(pure(a))") {
    def prog(useResume: Boolean): Str =
      val p = M.delimiter[String, String]
      M.reset[String, String, String](p)(
        M.shift0[String, String, String, String, String](p)(k =>
          if useResume then M.resume(k)(M.pure[String, String]("a")) else k("a")).map(_ + "!"))
    assertEquals(run(prog(true)), "a!")
    assertEquals(run(prog(false)), "a!")
  }

  test("the derived operators over the instance: shift runs its body under the delimiter, shift0 does not") {
    def probe(zero: Boolean): String =
      val p = M.delimiter[String, String]
      val inner: Str = M.shift[String, String, String, String, String](p)(_ => M.pure("inner-caught"))
      val body: Str =
        if zero then M.shift0[String, String, String, String, String](p)(_ => inner)
        else M.shift[String, String, String, String, String](p)(_ => inner)
      try run(M.reset[String, String, String](p)(body)) catch case _: NoPrompt => "NoPrompt"
    assertEquals(probe(zero = false), "inner-caught")
    assertEquals(probe(zero = true), "NoPrompt")
  }

  test("RESUME WITH A COMPUTATION: an abort to a delimiter that only k carries, run inside k") {
    // reset_p ( q-ret $ ( shift0_p k. resume k (abort_q "thrown") ) )
    // `q` is installed INSIDE the captured context; outside it nothing
    // names q, so no `k(a)` could reach it. The computation handed to
    // resume runs where q is in force: the abort leaves q with
    // "thrown", and — an abort drops its continuation, and q's `ret`
    // rides in it — q's ret is skipped, as for any abort to a `$`.
    def prog(m: P[String, String, String] => Str, q: Cont0.Delimiter[String, String]): Str =
      val p = M.delimiter[String, String]
      M.reset[String, String, String](p)(
        M.dollar[String, String, String, String](q)(s => M.pure("q:" + s))(
          M.shift0[String, String, String, String, String](p)(k => M.resume(k)(m(M.pure("x"))))))
    val q1 = M.delimiter[String, String]
    assertEquals(run(prog(identity, q1)), "q:x", "resumed with a value: q's ret on the way out")
    val q2 = M.delimiter[String, String]
    assertEquals(run(prog(_ => M.abort[String, String, String](q2)("thrown"), q2)), "thrown")
  }

  test("RESUME WITH A COMPUTATION: a capture inside it finds k's delimiter, twice resumed") {
    // the computation captures to q, which k carries, and resumes that
    // inner continuation twice: q's ret runs once per resumption ($/S0)
    val p = M.delimiter[String, String]
    val q = M.delimiter[String, String]
    val inside: Str =
      M.shift0[String, String, String, String, String](q)(k2 => k2("y").flatMap(a => k2("z").map(b => a + b)))
    val prog: Str =
      M.reset[String, String, String](p)(
        M.dollar[String, String, String, String](q)(s => M.pure(s"<$s>"))(
          M.shift0[String, String, String, String, String](p)(k => M.resume(k)(inside))))
    assertEquals(run(prog), "<y><z>")
  }
