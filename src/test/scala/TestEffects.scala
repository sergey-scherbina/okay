package okay

import !.*

case class Op[+A](a: A)

class TestEffects extends munit.FunSuite {

  def prog[M[_[+_], _]](using E: Effects[M]): M[Produce, Int] =
    E.perform[Produce, Int](1).flatMap(x => E.perform[Produce, Int](x + 1).map(y => x + y))

  test("tagless Effects: Free and Eff agree") {
    assertEquals(prog[Free].runWith, 3)
    assertEquals(prog[Eff].runWith, 3)
  }

  test("initial and final: reify materializes, fromFree interprets back") {
    val t: Int ! Produce = reify(prog[Eff])
    assertEquals(t.?, 1)      // the tree can be stepped; the Eff function cannot
    assertEquals(t.runWith, 3)
    assertEquals(toEff(t).runWith, 3)
    assertEquals(fromFree[Eff, Produce, Int](t).runWith, 3)
  }

  test("stack safety: runWith over a 1M bind chain (foldCont)") {
    val n = 1000000
    val e = (1 to n).foldLeft(pure[Produce, Int](0)): (m, _) =>
      m.flatMap(x => produce(x + 1))
    assertEquals(e.runWith, n)
  }

  test("Effects.handle: abort and forwarding (Throws)") {
    type F = Throws % String + Produce
    def calc(b: Boolean): Int ! F =
      effect[F, Int](2).flatMap: x =>
        (if b then effect[F, Int](Throws("boom")) else effect[F, Int](3)).map(_ + x)

    val E = summon[Effects[Free]]
    def run(b: Boolean): Int =
      E.handle[Throws % String, Produce, Int, Int](calc(b))(a => pure(a)):
        [X] => _ => shift(_ => pure(-1))
      .runWith

    assertEquals(run(false), 5)
    assertEquals(run(true), -1)

    assertEquals(runEither(calc(false)).runWith, Right(5))
    assertEquals(runEither(calc(true)).runWith, Left("boom"))
  }

  test("stack safety: a 1M tail-resumptive relay with forwarding") {
    val n = 1000000
    type FG = Op + Produce
    val prog = (1 to n).foldLeft(effect[FG, Int](Op(0))): (m, i) =>
      m.flatMap(x => effect[FG, Int](if i % 2 == 0 then Op(x + 1) else x + 1))
    val handled: Int ! Produce = relay[Int, Int, Op, Produce](prog)(pure(_)):
      [X, Y] => o => Cont.Pure(o.a)
    assertEquals(handled.runWith, n)
  }

}
