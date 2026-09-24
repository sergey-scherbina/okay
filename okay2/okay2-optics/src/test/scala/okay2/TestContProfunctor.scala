package okay2

import okay2.Optic._
import OpticsFixtures._

/** A TYPESTATE TRANSITION IS A PROFUNCTOR IN ITS STATE — the Scala 3
 * core's TestContProfunctor: `Cont[X, B => R, A => R]` is `Strong`, so
 * `PState.zoom` is the optic run at this carrier; there is no `Choice`,
 * and `zoomCase` is the prism's door */
class TestContProfunctor extends munit.FunSuite {

  def render[R]: Cont[Int, String => R, Int => R] =
    PState.get[Int, R].flatMap(n => PState.set[Int, String, R](s"n=$n").map(_ => n))

  def bump[R](by: Int): Cont[Int, Int => R, Int => R] =
    PState.get[Int, R].flatMap(n => PState.set[Int, Int, R](n + by).map(_ => n + by))

  val item: Lens[Box[Int], Box[String], Int, String] = Lens[Box[Int], Box[String], Int, String](_.item, (b, s) => Box(s, b.tag))

  test("the `lens` override agrees with the derivation it replaces, value for value") {
    type R = (Box[String], Int)
    val P = PState.strong[Int, R]
    val direct: Cont[Int, Box[String] => R, Box[Int] => R] =
      P.lens[Box[Int], Box[String], Int, String](_.item, (b, s) => Box(s, b.tag))(render[R])
    val derived: Cont[Int, Box[String] => R, Box[Int] => R] =
      P.dimap(P.first[Int, String, Box[Int]](render[R]))((b: Box[Int]) => (b.item, b), (sb: (String, Box[Int])) => Box(sb._1, sb._2.tag))
    val start = Box(7, "t")
    assertEquals(PState.run[Box[Int], Box[String], Int](start)(direct), PState.run[Box[Int], Box[String], Int](start)(derived))
    assertEquals(PState.run[Box[Int], Box[String], Int](start)(direct), (Box("n=7", "t"), 7))
  }

  test("`first`: the state is a pair, the program works on the left half, the right rides along") {
    type R = ((String, Boolean), Int)
    val paired: Cont[Int, ((String, Boolean)) => R, ((Int, Boolean)) => R] = PState.strong[Int, R].first[Int, String, Boolean](render[R])
    assertEquals(PState.run[(Int, Boolean), (String, Boolean), Int]((3, true))(paired), (("n=3", true), 3))
  }

  test("an ISO zooms too — it asks only for `dimap`") {
    type R = (Wrapped, Int)
    val unwrap: Iso[Wrapped, Wrapped, Int, Int] = Iso[Wrapped, Wrapped, Int, Int](_.v, Wrapped(_))
    val zoomed: Cont[Int, Wrapped => R, Wrapped => R] = unwrap[PState.Zooming[Int, R]#L](bump[R](5))
    assertEquals(PState.run[Wrapped, Wrapped, Int](Wrapped(1))(zoomed), (Wrapped(6), 6))
  }

  test("a COMPOSED optic zooms") {
    type R = (Box[Box[String]], Int)
    val outer: Lens[Box[Box[Int]], Box[Box[String]], Box[Int], Box[String]] =
      Lens[Box[Box[Int]], Box[Box[String]], Box[Int], Box[String]](_.item, (b, i) => Box(i, b.tag))
    val zoomed = PState.zoom[Box[Box[Int]], Box[Box[String]], Int, String, Int, R](outer.andThen(item))(render[R])
    assertEquals(PState.run[Box[Box[Int]], Box[Box[String]], Int](Box(Box(4, "in"), "out"))(zoomed), (Box(Box("n=4", "in"), "out"), 4))
  }

  test("zoomCase: the case is there — the program runs and the answer is Some; absent — nothing runs, None") {
    type R = (Option[String], Option[Int])
    var ran = 0
    def counted[Rr]: Cont[Int, String => Rr, Int => Rr] =
      PState.get[Int, Rr].flatMap { n => ran += 1; PState.set[Int, String, Rr](s"n=$n").map(_ => n) }
    val zoomed = PState.zoomCase[Option[Int], Option[String], Int, String, Int, R](Prism.some[Int, String])(counted[R])
    assertEquals(PState.run[Option[Int], Option[String], Option[Int]](None)(zoomed), (None, None))
    assertEquals(ran, 0, "the inner program ran on an absent case")
    assertEquals(PState.run[Option[Int], Option[String], Option[Int]](Some(5))(zoomed), (Some("n=5"), Some(5)))
    assertEquals(ran, 1)
  }

  test("there is no `Choice` for this carrier, and a LENS does resolve at it") {
    assert(compileErrors("implicitly[okay2.Optic.Choice[okay2.PState.Zooming[Int, Int]#L]]").nonEmpty,
      "a Choice for the zooming carrier resolved — the refutation is stale")
    assertEquals(compileErrors("implicitly[okay2.Optic.Strong[okay2.PState.Zooming[Int, Int]#L]]"), "")
  }
}
