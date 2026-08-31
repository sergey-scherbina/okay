package okay

import !.*

case class Op[+A](a: A)
/** the class is Op's whole identity, so the row test is total — the
 * same instance every signature in the library now carries, said here
 * for this test's own effect */
given okay.TypeableK[Op] = okay.typeableK(classOf[Op[?]])

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

  test("staged effects: one inline program, the run carrier is chosen") {
    inline def sprog[M[_[+_], _]]: M[Produce, Int] =
      val E = Effects[M]
      E.flatMap(E.perform[Produce, Int](1))(x => E.perform[Produce, Int](x + 1))
    assertEquals(sprog[Free].runIn[Cont], 2)
    assertEquals(sprog[Free].runIn[Func], 2)   // fused: no Cont tree in between
    assertEquals(sprog[Eff].runWith, 2)
    assertEquals(sprog[Eff].runIn[Func], 2)    // reifies, then fuses
  }

  test("staged effects, fully fused: inline handler-passing over Control") {
    inline def sprog[C[_, _, _]](h: Interpr[Produce, C, Int]): C[Int, Int, Int] =
      val C = Control[C]
      C.flatMap(h(1))(x => h(x + 1))
    assertEquals(sprog[Cont](handler[Produce, Int]) / identity, 2)
    assertEquals(sprog[Func](interpr[Func, Produce, Int])(identity), 2)
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


  test("translate: a handler valued in ANOTHER ROW, not in a value") {
    // Handler[F] is F ==> Id, and Id is where a suspension cannot go.
    // translate takes the general form — F ==> ([X] =>> X ! G) — so an
    // operation may answer with more computation.
    type Row = Reader % Int + (Writer % String + okay.Pure)

    val prog: Int ! Row =
      effect[Row, Int](Reader.Ask()).flatMap(x =>
        effect[Row, Int](Reader.Ask()).map(_ + x))

    // the Reader is answered by a program that TELLS on the way
    val told: Int ! (Writer % String + okay.Pure) =
      !.translate[Int, Reader % Int, Writer % String + okay.Pure](prog) {
        [X] => (e: (Reader % Int)[X]) => e match
          case Reader.Ask() =>
            effect[Writer % String + okay.Pure, String](Writer("asked"))
              .map(_ => 21.asInstanceOf[X])
      }

    val (ws, a) = !.run(Writer.run[String, Int, okay.Pure](told))
    assertEquals(a, 42)
    assertEquals(ws, Seq("asked", "asked"))
  }

  test("translate with a pure transformation IS a comonadic handler") {
    type Row = Reader % Int + okay.Pure
    val prog: Int ! Row = effect[Row, Int](Reader.Ask()).map(_ * 2)

    val viaTranslate = !.run(!.translate[Int, Reader % Int, okay.Pure](prog) {
      [X] => (e: (Reader % Int)[X]) => e match
        case Reader.Ask() => okay.pure(7.asInstanceOf[X])
    })
    val viaHandler = !.run(Reader.run[Int, Int, okay.Pure](7)(prog))
    assertEquals(viaTranslate, viaHandler)
  }

  test("translate forwards the effects it was not given") {
    type Row = Reader % Int + (Writer % String + okay.Pure)
    val prog: Int ! Row =
      effect[Row, String](Writer("before")).flatMap(_ =>
        effect[Row, Int](Reader.Ask())).flatMap(x =>
        effect[Row, String](Writer("after")).map(_ => x))

    val told = !.translate[Int, Reader % Int, Writer % String + okay.Pure](prog) {
      [X] => (e: (Reader % Int)[X]) => e match
        case Reader.Ask() => okay.pure(5.asInstanceOf[X])
    }
    val (ws, a) = !.run(Writer.run[String, Int, okay.Pure](told))
    assertEquals(a, 5)
    assertEquals(ws, Seq("before", "after"))
  }
}
