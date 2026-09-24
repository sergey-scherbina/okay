package okay2

/**
 * The shared tree under both encodings, and the LAW that licenses
 * `Cont`'s runner to hold its own copy of the rotation: agreeing with
 * `Func` — which composes closures and NEVER rotates — over every
 * bind-tree shape, on the answer AND on the order the effects happened
 * in. The Scala 3 core's TestFree, shape for shape.
 */
class TestFree extends munit.FunSuite {

  /** one program, written once, runnable at any Control carrier */
  trait Shape {
    def apply[M[_, _, _]](log: Int => Unit)(implicit C: Control[M]): M[Int, Int, Int]
  }

  /** a shift that records itself before resuming */
  def op[M[_, _, _]](i: Int, log: Int => Unit)(implicit C: Control[M]): M[Int, Int, Int] =
    C.shift[Int, Int, Int](k => { log(i); k(i) })

  /** small enough that `Func`'s nested closures are safe */
  val N = 64

  val shapes: List[(String, Shape)] = List(
    "a value" -> new Shape {
      def apply[M[_, _, _]](log: Int => Unit)(implicit C: Control[M]) = C.pure(7) },

    "one operation" -> new Shape {
      def apply[M[_, _, _]](log: Int => Unit)(implicit C: Control[M]) = op(1, log) },

    "map only" -> new Shape {
      def apply[M[_, _, _]](log: Int => Unit)(implicit C: Control[M]) = C.map(C.pure[Int, Int](1))(_ + 1) },

    "a pure bound" -> new Shape {
      def apply[M[_, _, _]](log: Int => Unit)(implicit C: Control[M]) =
        C.flatMap(C.pure[Int, Int](1))(x => C.pure[Int, Int](x + 1)) },

    "an operation, then a bind" -> new Shape {
      def apply[M[_, _, _]](log: Int => Unit)(implicit C: Control[M]) =
        C.flatMap(op(1, log))(x => C.pure[Int, Int](x + 1)) },

    "an operation, then TWO binds (the second cannot be absorbed)" -> new Shape {
      def apply[M[_, _, _]](log: Int => Unit)(implicit C: Control[M]) =
        C.flatMap(C.flatMap(op(1, log))(x => C.pure[Int, Int](x + 1)))(x => C.pure[Int, Int](x * 2)) },

    "right-nested operations" -> new Shape {
      def apply[M[_, _, _]](log: Int => Unit)(implicit C: Control[M]) = {
        def go(i: Int): M[Int, Int, Int] =
          if (i > N) C.pure(0) else C.flatMap(op(i, log))(x => C.map(go(i + 1))(_ + x))
        go(1)
      } },

    "left-nested binds over an operation" -> new Shape {
      def apply[M[_, _, _]](log: Int => Unit)(implicit C: Control[M]) =
        (1 to N).foldLeft(op(0, log))((m, _) => C.flatMap(m)(x => C.pure[Int, Int](x + 1))) },

    "left-nested binds over a value" -> new Shape {
      def apply[M[_, _, _]](log: Int => Unit)(implicit C: Control[M]) =
        (1 to N).foldLeft(C.pure[Int, Int](0))((m, _) => C.flatMap(m)(x => C.pure[Int, Int](x + 1))) },

    "mixed associations" -> new Shape {
      def apply[M[_, _, _]](log: Int => Unit)(implicit C: Control[M]) = {
        val left = (1 to 8).foldLeft(op(1, log))((m, _) => C.flatMap(m)(x => C.pure[Int, Int](x + 1)))
        def right(i: Int): M[Int, Int, Int] =
          if (i > 8) left else C.flatMap(op(i + 100, log))(x => C.map(right(i + 1))(_ + x))
        C.flatMap(right(1))(x => C.flatMap(op(999, log))(y => C.pure[Int, Int](x + y)))
      } },

    "a continuation that drops its input" -> new Shape {
      def apply[M[_, _, _]](log: Int => Unit)(implicit C: Control[M]) =
        C.flatMap(op(1, log))(_ => C.flatMap(op(2, log))(_ => C.pure[Int, Int](42))) },

    "an operation that does NOT resume" -> new Shape {
      def apply[M[_, _, _]](log: Int => Unit)(implicit C: Control[M]) =
        C.flatMap(C.shift[Int, Int, Int] { _ => log(-1); 5 })(x => C.pure[Int, Int](x + 1)) }
  )

  for ((name, shape) <- shapes)
    test(s"rotation law: Cont's runner agrees with Func — $name") {
      val tc = List.newBuilder[Int]
      val answerCont = Control[Cont.Rep].reset(shape[Cont.Rep](tc += _))
      val tf = List.newBuilder[Int]
      val answerFunc = Control[Func].reset(shape[Func](tf += _))
      assertEquals(answerCont, answerFunc, "answer")
      assertEquals(tc.result(), tf.result(), "effect trace")
    }

  test("resume normalizes every shape to a head form") {
    import Free._
    def headForm(c: Any): Boolean = c match {
      case Return(_) => true
      case Inject(_) => true
      case Bind(a, _) => a match { case Inject(_) => true; case _ => false }
      case _ => false
    }

    def op(i: Int): Int ! Produce = effect[Produce, Int](Produce.Emit(i))
    val shapes: List[(String, Int ! Produce)] = List(
      "a value" -> pure(7),
      "one operation" -> op(1),
      "a pure bound" -> pure[Produce, Int](1).flatMap(x => pure(x + 1)),
      "an operation then a bind" -> op(1).flatMap(x => pure(x + 1)),
      "left-nested over an operation" -> (1 to 64).foldLeft(op(0))((m, _) => m.flatMap(x => pure(x + 1))),
      "left-nested over a value" -> (1 to 64).foldLeft(pure[Produce, Int](0))((m, _) => m.flatMap(x => pure(x + 1))),
      "a deferred left side" -> !.tailcall(op(3)).flatMap(x => pure(x + 1))
    )
    for ((name, p) <- shapes) {
      assert(headForm(p.resume), s"$name: resume left a head form")
      assert(headForm(p.resume.resume), s"$name: resume is idempotent at the head")
    }
  }
}
