package okay2

/** TestStackSafeLoops on a 128 KB thread, where a loop that holds a frame per iteration overflows at a few
 * thousand: the carriers' loops and `foldMap` hold none (specs/eager-carrier-depth.md) */
class TestStackSafeLoopsSmall extends munit.FunSuite {
  val n = 1000000

  test("a million iterations of Option's and a program's loop on a 128 KB thread") {
    SmallStack.run() {
      assertEquals(TailRecM[Option].tailRecM(0)(i => Some(if (i < n) Left(i + 1) else Right(i))), Some(n))
      type P[X] = Free[Pure, X]
      assertEquals(!.run(TailRecM[P].tailRecM(0)(i => pure[Pure, Either[Int, Int]](if (i < n) Left(i + 1) else Right(i)))), n)
    }
  }

  test("TailRecM.deferring on a program, whose flatMap defers: a million on a 128 KB thread (its inventory row)") {
    type P[X] = Free[Pure, X]
    val deferring = TailRecM.deferring[P]
    SmallStack.run() {
      assertEquals(!.run(deferring.tailRecM(0)(i => pure[Pure, Either[Int, Int]](if (i < n) Left(i + 1) else Right(i)))), n)
    }
  }

  test("foldMap into Option, a million operations, on a 128 KB thread") {
    val asOption: Static.To[Produce, Option] = new Static.To[Produce, Option] {
      def apply[X](op: Produce.Emit[X]): Option[X] = Some(op.a)
    }
    SmallStack.run() {
      val left: Int ! Produce = (1 to n).foldLeft(pure[Produce, Int](0))((m, _) => m.flatMap(x => produce(x + 1)))
      assertEquals(Effects.free.foldMap(left)(asOption), Some(n))
    }
  }
}
