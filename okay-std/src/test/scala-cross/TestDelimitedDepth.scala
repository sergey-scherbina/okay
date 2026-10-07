package okay.freer



import okay.std.*
import okay.std.given
/**
 * The machine's depth, through `Delimited` ALONE, on every platform
 * (specs/cont-js-depth.md, stage 2). A continuation here is DATA — `k(a)`
 * is `resume(k)(pure(a))`, a program — so nothing nests the host stack:
 * a million captures whose bodies USE their continuation's answer, a
 * million nested delimiters, a million binds, and multi-shot resumption
 * at depth. Each program's answer is a formula, checked first on the
 * reference (DelimitedReference, NOT stack-safe) at small sizes.
 */
object DelimitedDepth:

  /** a million captures in a row under one delimiter, each body binding
   * on its resumption's answer — `k(1) + 1` written as data */
  def answerUsing[M[_, _, _]](D: LambdaDollar[M])(n: Int): M[Int, Int, Int] =
    extension [A, S, R](m: M[S, R, A])
      def map[B](f: A => B): M[S, R, B] = D.bind[A, B, S, S, R](m)(a => D.pure[B, S](f(a)))
    val p = D.delimiter[Int, Int](using At("TestDelimitedDepth"))
    def loop(i: Int): M[Int, Int, Int] =
      if i == 0 then D.pure[Int, Int](0)
      else D.bind[Int, Int, Int, Int, Int](
        D.shift0[Int, Int, Int, Int, Int](p)(k => k(1).map(_ + 1))(using At("TestDelimitedDepth")))(
        x => loop(i - 1).map(_ + x))
    D.reset[Int, Int, Int](p)(loop(n))

  /** a million delimiters, each inside the last */
  def nestedResets[M[_, _, _]](D: LambdaDollar[M])(n: Int): M[Int, Int, Int] =
    val p = D.delimiter[Int, Int](using At("TestDelimitedDepth"))
    def nest(i: Int): M[Int, Int, Int] =
      if i == 0 then D.pure[Int, Int](0)
      else D.reset[Int, Int, Int](p)(D.bind[Int, Int, Int, Int, Int](D.pure[Int, Int](i))(_ => D.bind[Int, Int, Int, Int, Int](nest(i - 1))(x => D.pure[Int, Int](x + 1))))
    nest(n)

  /** a million binds, left-nested */
  def leftBinds[M[_, _, _]](D: LambdaDollar[M])(n: Int): M[Int, Int, Int] =
    (1 to n).foldLeft(D.pure[Int, Int](0))((m, _) => D.bind[Int, Int, Int, Int, Int](m)(x => D.pure[Int, Int](x + 1)))

  /** every level resumes its continuation TWICE: 2^depth runs of the rest */
  def multiShot[M[_, _, _]](D: LambdaDollar[M])(depth: Int): M[Int, Int, Int] =
    val p = D.delimiter[Int, Int](using At("TestDelimitedDepth"))
    def level(i: Int): M[Int, Int, Int] =
      if i == 0 then D.pure[Int, Int](1)
      else D.bind[Int, Int, Int, Int, Int](
        D.shift0[Int, Int, Int, Int, Int](p)(k =>
          D.bind[Int, Int, Int, Int, Int](k(0))(a => D.bind[Int, Int, Int, Int, Int](k(1))(b => D.pure[Int, Int](a + b))))(using At("TestDelimitedDepth")))(
        _ => level(i - 1))
    D.reset[Int, Int, Int](p)(level(depth))

class TestDelimitedDepth extends munit.FunSuite:
  import DelimitedDepth.*

  val M = LambdaDollar.machine
  val R = DelimitedReference.Ref
  val n = 1000000

  def onReference(name: String, sizes: Seq[Int])(prog: Int => DelimitedReference.P[Int, Int, Int])(formula: Int => Int): Unit =
    test(s"the formula of $name, on the reference at small sizes") {
      for s <- sizes do assertEquals(R.run(prog(s)), formula(s), s"$name($s)")
    }

  onReference("answerUsing", Seq(1, 2, 3, 10, 50))(answerUsing(R))(2 * _)
  onReference("nestedResets", Seq(1, 2, 3, 10, 50))(nestedResets(R))(identity)
  onReference("leftBinds", Seq(1, 2, 3, 10, 50))(leftBinds(R))(identity)
  // the reference recurses once per STEP of the whole run (it is CPS with no
  // trampoline), and multi-shot runs 2^d·d steps: small d only
  onReference("multiShot", Seq(1, 2, 3, 5))(multiShot(R))(1 << _)

  test("a million captures whose bodies use their continuation's answer") {
    assertEquals(M.run(answerUsing(M)(n)), 2 * n)
  }

  test("a million nested delimiters") {
    assertEquals(M.run(nestedResets(M)(n)), n)
  }

  test("a million left-nested binds") {
    assertEquals(M.run(leftBinds(M)(n)), n)
  }

  test("multi-shot at depth: every one of 20 levels resumed twice, 2^20 runs") {
    assertEquals(M.run(multiShot(M)(20)), 1 << 20)
  }
