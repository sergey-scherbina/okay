package okay

import okay.given
import okay.Eager.given

/**
 * `runWith` is DEFINED as `m.foldCont(handler[F, A]) / identity` — the
 * program lowered into Cont and run against the identity continuation.
 * Free and Eager both OVERRIDE it with a one-pass fast path (`runFree`,
 * the eager fold), and the comment on each says "the same answer as the
 * foldCont definition". That equation was, until this suite, stated and
 * never checked: TestReflect proves the encodings agree with EACH OTHER,
 * but both of its sides call `runWith`, so a fast path that drifted from
 * the definition would take every encoding with it and pass.
 *
 * Here the two paths are run against each other, per encoding, over the
 * shapes a bind tree can take — and the ANSWER is not the whole claim:
 * the effects must also happen in the same order, so every case compares
 * the value together with the trace the handler wrote.
 */
class TestLowering extends munit.FunSuite {

  enum Op[+A]:
    case Get() extends Op[Int]
    case Say(s: String) extends Op[Unit]

  given TypeableK[Op] = typeableK(classOf[Op[?]])

  private val said = scala.collection.mutable.Buffer[String]()
  private var gets = 0
  given Handler[Op] with
    def handle[A](o: Op[A]): A = o match
      case Op.Get() => gets += 1; gets
      case Op.Say(s) => said += s; ()

  /** one program shape, written once and read at every encoding */
  trait Shape:
    def name: String
    def apply[M[_[+_], _]](using E: Effects[M]): M[Op, Int]

  private def shape(n: String)(f: [M[_[+_], _]] => Effects[M] ?=> M[Op, Int]): Shape =
    new Shape:
      def name = n
      def apply[M[_[+_], _]](using Effects[M]): M[Op, Int] = f[M]

  val shapes: List[Shape] = List(
    shape("pure")([M[_[+_], _]] => (E: Effects[M]) ?=> E.pure(42)),
    shape("one operation")([M[_[+_], _]] => (E: Effects[M]) ?=> E.perform(Op.Get())),
    shape("map only")([M[_[+_], _]] => (E: Effects[M]) ?=> E.perform(Op.Get()).map(_ * 2)),
    shape("pure bound")([M[_[+_], _]] => (E: Effects[M]) ?=> E.pure(1).flatMap(a => E.pure(a + 1))),
    // right-nested: the shape a for-comprehension writes
    shape("right-nested")([M[_[+_], _]] => (E: Effects[M]) ?=>
      E.perform(Op.Get()).flatMap(a =>
        E.perform(Op.Say(s"got $a")).flatMap(_ =>
          E.perform(Op.Get()).map(b => a + b)))),
    // left-nested: the shape a fold writes — the case the runners re-associate
    shape("left-nested")([M[_[+_], _]] => (E: Effects[M]) ?=>
      E.perform(Op.Get())
        .flatMap(a => E.perform(Op.Get()).map(_ + a))
        .flatMap(a => E.perform(Op.Say(s"sum $a")).map(_ => a))
        .flatMap(a => E.perform(Op.Get()).map(_ + a))),
    // an operation whose continuation is dropped
    shape("continuation discards its input")([M[_[+_], _]] => (E: Effects[M]) ?=>
      E.perform(Op.Get()).flatMap(_ => E.pure(0))),
    // both associations in one tree
    shape("mixed associations")([M[_[+_], _]] => (E: Effects[M]) ?=>
      E.perform(Op.Get())
        .flatMap(a => E.perform(Op.Say("l")).flatMap(_ => E.pure(a)))
        .flatMap(b => E.perform(Op.Get()).map(_ + b))),
    shape("deep left-nested chain")([M[_[+_], _]] => (E: Effects[M]) ?=>
      (1 to 5000).foldLeft(E.pure[Op, Int](0))((m, _) =>
        m.flatMap(x => E.perform(Op.Get()).map(_ => x + 1)))),
    shape("deep right-nested chain")([M[_[+_], _]] => (E: Effects[M]) ?=>
      def go(i: Int): M[Op, Int] =
        if i == 0 then E.pure(0)
        else E.perform(Op.Get()).flatMap(_ => go(i - 1).map(_ + 1))
      go(5000))
  )

  /** the answer AND the trace, with the handler's counters reset */
  private def observe(run: => Int): (Int, List[String], Int) =
    said.clear(); gets = 0
    val v = run
    (v, said.toList, gets)

  /** the definition: lower into Cont, run against identity */
  private def byDefinition[M[_[+_], _] : Effects](m: M[Op, Int]): Int =
    m.foldCont(handler[Op, Int]) / identity

  private def agree[M[_[+_], _] : Effects](enc: String, s: Shape): Unit =
    val m = s[M]
    val defn = observe(byDefinition(m))
    val fast = observe(s[M].runWith)
    assertEquals(fast, defn, s"$enc / ${s.name}: runWith disagrees with foldCont / identity")

  for s <- shapes do
    test(s"runWith is foldCont / identity — ${s.name}") {
      agree[Free]("Free", s)
      agree[Eager]("Eager", s)
      agree[Eff]("Eff", s)
    }

  test("the encodings agree with the definition, not only with each other") {
    // the same shape lowered from each encoding must give one answer
    for s <- shapes do
      val viaFree = observe(byDefinition(s[Free]))
      val viaEager = observe(byDefinition(s[Eager]))
      val viaEff = observe(byDefinition(s[Eff]))
      assertEquals(viaEager, viaFree, s"Eager lowering differs on ${s.name}")
      assertEquals(viaEff, viaFree, s"Eff lowering differs on ${s.name}")
  }
}
