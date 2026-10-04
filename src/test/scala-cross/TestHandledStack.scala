package okay

import okay.Row.at

/**
 * handler-single-pass stage 2 (specs/handler-single-pass.md): `handle` REGISTERS a stepped handler on the
 * program's stack, and one walk answers the whole stack. THE LAW: a stack answers exactly what its handlers'
 * own runs answer nested (`h2.run(h1.run(p))`, which registers nothing), for every shape below.
 */
class TestHandledStack extends munit.FunSuite:

  /** right-nested in the order the handlers take the effects off: Reader first, then Writer, then State */
  type RSW = Reader % Int + (Writer % String + State % Int)

  /** a mixed program: reads, state, tells */
  private def mixed(n: Int): Int ! RSW =
    (1 to n).foldLeft(pure[RSW, Int](0)): (m, i) =>
      m.flatMap: acc =>
        (i % 4) match
          case 0 => Reader.ask[Int].at[RSW].map(acc + _)
          case 1 => State.get[Int].at[RSW].flatMap(s => State.set[Int](s + i).at[RSW]).map(_ => acc)
          case 2 => Writer.tell(s"t$i").at[RSW].map(_ => acc + 1)
          case _ => State.get[Int].at[RSW].map(acc + _)

  /** Reader, then Writer, then State: the middle types written, since a third chained `handle` infers its rest
   * as the row itself (Distinct then refuses it), a limit of `handle`'s inference, not of the stack */
  private def three(p: Int ! RSW, r: Int): (Int, (Seq[String], Int)) ! Pure =
    val a: Int ! (Writer % String + State % Int) = p.handle(Reader(r))
    val b: (Seq[String], Int) ! State % Int = a.handle(Writer.log[String])
    b.handle(State(0))

  test("registration: a chain of stepped handles is ONE node holding the stack, innermost first") {
    val p = three(mixed(10), 2)
    p match
      case Freer.Delay(st: HandleFrames.Handled) => assertEquals(st.stack.length, 3)
      case other => fail(s"not one stack node: $other")
  }

  test("law: three stepped handlers, the stack against their own runs nested") {
    val fused = three(mixed(200), 2).run
    val nested = State(0).run[(Seq[String], Int), Pure](Writer.log[String].run[Int, State % Int](Reader(2).run[Int, Writer % String + State % Int](mixed(200)))).run
    assertEquals(fused, nested)
  }

  test("law: a map between two handles leaves a node of its own, and the answer the same") {
    val a: Int ! (Writer % String + State % Int) = mixed(30).handle(Reader(2)).map(_ * 3)
    val b: (Seq[String], Int) ! State % Int = a.handle(Writer.log[String])
    val fused = b.handle(State(0)).run
    val nested = State(0).run[(Seq[String], Int), Pure](Writer.log[String].run[Int, State % Int](Reader(2).run[Int, Writer % String + State % Int](mixed(30)).map(_ * 3))).run
    assertEquals(fused, nested)
  }

  /** set, dictate, read, halt: at the row `R`, whichever order it lists the two effects in */
  private def halting[R[+_]](using Row.In[State % Int, R], Row.In[Chronicle % String, R]): Int ! R =
    State.set[Int](5).at[R].flatMap(_ => Chronicle.dictate("w").at[R])
      .flatMap(_ => State.get[Int].at[R]).flatMap(_ => Chronicle.halt[String, Int].at[R]).map(_ + 1)

  test("law: a halt INSIDE the stack drops the handlers inside it, and the outside walks on") {
    // State inside, Chronicle outside: State's answer is dropped by the halt
    val sc = halting[State % Int + Chronicle % String]
    assertEquals(sc.handle(State(0)).handle(Chronicle.verdict[String]).run,
      Chronicle.verdict[String].run[(Int, Int), Pure](State(0).run[Int, Chronicle % String](sc)).run)
    // Chronicle inside, State outside: State answers around the halted verdict
    val cs = halting[Chronicle % String + State % Int]
    assertEquals(cs.handle(Chronicle.verdict[String]).handle(State(0)).run,
      State(0).run[Chronicle.Verdict[String, Int], Pure](Chronicle.verdict[String].run[Int, State % Int](cs)).run)
  }

  test("law: an outer multi-shot handler resumes each branch with the states as they were") {
    type CSW = State % Int + (Writer % String + Choose)
    val p: Int ! CSW =
      for
        _ <- State.set[Int](1).at[CSW]
        x <- Choose(Seq(10, 20, 30)).perform.at[CSW]
        s <- State.get[Int].at[CSW]
        _ <- State.set[Int](s + x).at[CSW]
        _ <- Writer.tell(s"x=$x").at[CSW]
        t <- State.get[Int].at[CSW]
      yield t
    val a: (Int, Int) ! (Writer % String + Choose) = p.handle(State(0))
    val b: (Seq[String], (Int, Int)) ! Choose = a.handle(Writer.log[String])
    val fused = b.handle(Choose.all).run
    val nested = runChoice[(Seq[String], (Int, Int)), Pure](Writer.log[String].run[(Int, Int), Choose](State(0).run[Int, Writer % String + Choose](p))).run
    assertEquals(fused, nested)
    assertEquals(fused.map(_._2._2), Seq(11, 21, 31))
  }

  test("law: a handler that is not stepped (Throws) inside the stack is a run of its own the walk forces") {
    type TSW = Throws % String + (State % Int + Writer % String)
    val ok: Int ! TSW = State.set[Int](3).at[TSW].flatMap(_ => Writer.tell("a").at[TSW]).flatMap(_ => State.get[Int].at[TSW])
    val bad: Int ! TSW = Writer.tell("b").at[TSW].flatMap(_ => raise[String, Int]("no").at[TSW])
    for p <- List(ok, bad) do
      val a: Either[String, Int] ! (State % Int + Writer % String) = p.handle(Throws.either[String])
      val b: (Int, Either[String, Int]) ! Writer % String = a.handle(State(0))
      assertEquals(b.handle(Writer.log[String]).run,
        Writer.log[String].run[(Int, Either[String, Int]), Pure](State(0).run[Either[String, Int], Writer % String](runEither[Int, State % Int + Writer % String, String](p))).run)
  }

  /** a stepped handler whose `ret` PERFORMS: it counts Fresh draws and tells the count to the Writer outside */
  private object Counting extends Handler.Stepped[Fresh, Long, [A] =>> A]:
    def run[A, F[+_]](p: A ! Fresh + F)(using A <:< Any, Distinct[Fresh + F], Handler.Nothing[F]): A ! F =
      HandleFrames.stateRun[Fresh, Long, A, A, F](summon[TypeableK[Fresh]], (n, a) => ret[A, F](n, a))(
        (n, _) => (n + 1, n))(0L, p)
    def takes: TypeableK[Fresh] = summon[TypeableK[Fresh]]
    def init: Long = 0L
    def step(n: Long, op: Any): (Long, Any) | Handler.Halt[Long] = (n + 1, n)
    def ret[A, F[+_]](n: Long, a: A): A ! F =
      // the claim of the test: this handler is only ever used under a Writer % String outside it
      Writer.tell(s"drew $n").asInstanceOf[Unit ! F].map(_ => a)

  test("law: an operation a handler's ret performs goes OUT, to the handlers outside it") {
    type FW = Fresh + Writer % String
    val p: Long ! FW = Fresh.next.at[FW].flatMap(a => Writer.tell(s"a=$a").at[FW]).flatMap(_ => Fresh.next.at[FW])
    assertEquals(p.handle(Counting).handle(Writer.log[String]).run,
      Writer.log[String].run[Long, Pure](Counting.run[Long, Writer % String](p)).run)
    assertEquals(p.handle(Counting).handle(Writer.log[String]).run._1, Seq("a=0", "drew 2"))
  }

  test("stack: 100 000 operations through a stack of three on a 256 KB thread") {
    var out: Either[Throwable, Int] = Left(IllegalStateException("never ran"))
    val t = new Thread(null, () => out =
      try Right(three(mixed(100000), 1).run._2._2)
      catch case e: Throwable => Left(e), "small", 256L * 1024)
    t.start()
    t.join()
    assert(out.isRight, s"$out")
  }
