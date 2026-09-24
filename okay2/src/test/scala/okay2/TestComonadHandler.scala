package okay2

object ComonadFixtures {
  /** a row whose operation IS its answer, in a labelled box */
  sealed trait Boxed extends Row { type Op[+A] = Boxed.Box[A] }
  object Boxed {
    final case class Box[+A](label: String, value: A)
    implicit val effect: Effect[Boxed] = Effect.of[Boxed]
    implicit val comonad: Comonad[Box] = new Comonad[Box] {
      def fmap[A, B](a: Box[A], f: A => B): Box[B] = Box(a.label, f(a.value))
      def extract[A](a: Box[A]): A = a.value
      def coflatMap[A, B](a: Box[A])(f: Box[A] => B): Box[B] = Box(a.label, f(a))
    }
    def box[A](label: String, a: A): A ! Boxed = Free.inject[Boxed, A](Box(label, a))
  }
}

/** the Scala 3 core's `given [F[_]: Comonad]: Handler[F]` — a row whose
 * operations form a comonad needs no handler of its own; and the
 * `Safe`/`Unsafe` aliases */
class TestComonadHandler extends munit.FunSuite {
  import ComonadFixtures.Boxed

  test("a comonadic row is run by extract, with no handler written") {
    val prog = for { a <- Boxed.box("a", 20); b <- Boxed.box("b", 22) } yield a + b
    assertEquals(Effects.runFree(prog), 42)
    assert(implicitly[Handler[Boxed]].isInstanceOf[Handler.ComonadHandler[_]])
  }

  test("Safe is Nothing and Unsafe is Throwable") {
    implicitly[Safe =:= Nothing]: Unit
    implicitly[Unsafe =:= Throwable]: Unit
    val never: Either[Safe, Int] = Right(1)
    assertEquals(never.toOption, Some(1))
  }
}
