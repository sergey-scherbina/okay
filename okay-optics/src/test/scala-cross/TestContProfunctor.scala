package okay

import okay.given
import scala.compiletime.testing.typeCheckErrors

/**
 * A TYPESTATE TRANSITION IS A PROFUNCTOR IN ITS STATE
 * (specs/optics.md stage 12, `optics-cont-profunctor`).
 *
 * `Cont[X, B => R, A => R]` computes an `X` and takes the state from
 * `A` to `B`. Read as `P[A, B]` it is `Strong`, so `PState.zoom` is
 * no longer a hand-written `shift`: it is the optic run at this
 * carrier, and `TestZoom` passing unchanged is that claim's evidence.
 *
 * This file adds what the instance made possible and what it did not:
 * the `lens` override held to the default it replaces, an iso and a
 * raw `first` (the two roads `zoom` never used), the prism door whose
 * type says what a missing case costs, and the REFUSAL — there is no
 * `Choice` here, and a test that only accepted things could never
 * show that.
 */
class TestContProfunctor extends munit.FunSuite {

  // the inner program, in both directions of the typestate: read the
  // Int state, leave a String state, answer the Int that was there
  def render[R]: Cont[Int, String => R, Int => R] =
    PState.get[Int, R].flatMap(n => PState.set[Int, String, R](s"n=$n").map(_ => n))

  // a state-preserving one, for the optics that do not change types
  def bump[R](by: Int): Cont[Int, Int => R, Int => R] =
    PState.get[Int, R].flatMap(n => PState.set[Int, Int, R](n + by).map(_ => n + by))

  final case class Wrapped(v: Int)
  final case class Box[A](item: A, tag: String)
  val item: Lens[Box[Int], Box[String], Int, String] =
    Lens(_.item, (b, s) => Box(s, b.tag))

  // ---------------------------------------------------------------- the override, against the default it replaces

  test("the `lens` override agrees with the derivation it replaces, value for value") {
    type R = (Box[String], Int)
    val P = PState.strong[Int, R]

    // the direct road this instance takes
    val direct: Cont[Int, Box[String] => R, Box[Int] => R] =
      P.lens[Box[Int], Box[String], Int, String](_.item, (b, s) => Box(s, b.tag))(render[R])

    // the textbook derivation `Strong` would have used: pair the focus
    // with the whole, run `first`, put it back
    val derived: Cont[Int, Box[String] => R, Box[Int] => R] =
      P.dimap(P.first[Int, String, Box[Int]](render[R]))(
        (b: Box[Int]) => (b.item, b),
        (sb: (String, Box[Int])) => Box(sb._1, sb._2.tag))

    val start = Box(7, "t")
    assertEquals(PState.run[Box[Int], Box[String], Int](start)(direct),
                 PState.run[Box[Int], Box[String], Int](start)(derived))
    assertEquals(PState.run[Box[Int], Box[String], Int](start)(direct), (Box("n=7", "t"), 7))
  }

  // ---------------------------------------------------------------- the roads zoom never used

  test("`first`: the state is a pair, the program works on the left half, the right rides along") {
    type R = ((String, Boolean), Int)
    val P = PState.strong[Int, R]
    val paired: Cont[Int, ((String, Boolean)) => R, ((Int, Boolean)) => R] =
      P.first[Int, String, Boolean](render[R])

    assertEquals(PState.run[(Int, Boolean), (String, Boolean), Int]((3, true))(paired),
                 (("n=3", true), 3))
  }

  test("an ISO zooms too — it asks only for `dimap`, which no zoom had ever exercised") {
    type R = (Wrapped, Int)
    val unwrap: Iso[Wrapped, Wrapped, Int, Int] = Iso(_.v, Wrapped(_))

    val zoomed: Cont[Int, Wrapped => R, Wrapped => R] = unwrap[PState.Zooming[Int, R]](bump[R](5))
    assertEquals(PState.run[Wrapped, Wrapped, Int](Wrapped(1))(zoomed), (Wrapped(6), 6))
  }

  test("a COMPOSED optic zooms, and composition is the only thing that made it work") {
    type R = (Box[Box[String]], Int)
    val outer: Lens[Box[Box[Int]], Box[Box[String]], Box[Int], Box[String]] =
      Lens(_.item, (b, i) => Box(i, b.tag))
    val zoomed = PState.zoom[Box[Box[Int]], Box[Box[String]], Int, String, Int, R](
      outer.andThen(item))(render[R])

    assertEquals(PState.run[Box[Box[Int]], Box[Box[String]], Int](Box(Box(4, "in"), "out"))(zoomed),
                 (Box(Box("n=4", "in"), "out"), 4))
  }

  // ---------------------------------------------------------------- the prism door, and what it costs

  test("zoomCase: the case is there — the program runs and the answer is Some") {
    type R = (Option[String], Option[Int])
    val zoomed = PState.zoomCase[Option[Int], Option[String], Int, String, Int, R](
      Prism.some[Int, String])(render[R])

    assertEquals(PState.run[Option[Int], Option[String], Option[Int]](Some(5))(zoomed),
                 (Some("n=5"), Some(5)))
  }

  test("zoomCase: the case is NOT there — nothing runs, the state passes through, the answer is None") {
    type R = (Option[String], Option[Int])
    val zoomed = PState.zoomCase[Option[Int], Option[String], Int, String, Int, R](
      Prism.some[Int, String])(render[R])

    assertEquals(PState.run[Option[Int], Option[String], Option[Int]](None)(zoomed),
                 (None, None))
  }

  test("zoomCase: the inner program is not merely skipped — it never runs at all") {
    type R = (Option[String], Option[Int])
    var ran = 0
    def counted[Rr]: Cont[Int, String => Rr, Int => Rr] =
      PState.get[Int, Rr].flatMap { n => ran += 1; PState.set[Int, String, Rr](s"n=$n").map(_ => n) }

    val zoomed = PState.zoomCase[Option[Int], Option[String], Int, String, Int, R](
      Prism.some[Int, String])(counted[R])
    val _ = PState.run[Option[Int], Option[String], Option[Int]](None)(zoomed)
    assertEquals(ran, 0, "the inner program ran on an absent case")

    val _ = PState.run[Option[Int], Option[String], Option[Int]](Some(1))(zoomed)
    assertEquals(ran, 1, "the inner program did not run on a present case")
  }

  // ---------------------------------------------------------------- THE REFUSAL

  test("there is no `Choice` for this carrier, and a prism therefore cannot be an instance") {
    // The mechanism, stated in State.scala where it is paid: on the
    // absent case `right` must still answer the program's `X`, and `X`
    // is universally quantified — there is no `X` to make and no
    // continuation to get one from. So the summon must fail, and a
    // test that only accepted things could never show it.
    val e = typeCheckErrors("summon[okay.Optic.Choice[okay.PState.Zooming[Int, Int]]]")
    assert(e.nonEmpty, "a Choice for the zooming carrier resolved — the refutation is stale")

    // and therefore a prism cannot be run at the carrier the way a
    // lens can: this is the same refusal one level up, where a user
    // would meet it
    val p = typeCheckErrors(
      "okay.Prism.some[Int, String].apply[okay.PState.Zooming[Int, Int]](???)")
    assert(p.nonEmpty, "a prism ran at the zooming carrier — the refutation is stale")
  }

  test("a LENS does resolve at the same carrier — the refusal above is about `Choice`, not about the carrier") {
    val e = typeCheckErrors("summon[okay.Optic.Strong[okay.PState.Zooming[Int, Int]]]")
    assertEquals(e, Nil, "the Strong instance stopped resolving")
  }
}
