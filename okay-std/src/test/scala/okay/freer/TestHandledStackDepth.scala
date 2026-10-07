package okay.freer



import okay.std.*
import okay.std.given
import okay.freer.Row.at

/** handler-single-pass stage 2, the stack's walk in constant host stack: JVM-only (a thread of a chosen size) */
class TestHandledStackDepth extends munit.FunSuite:

  type RSW = Reader % Int + (Writer % String + State % Int)

  private def mixed(n: Int): Int ! RSW =
    (1 to n).foldLeft(pure[RSW, Int](0)): (m, i) =>
      m.flatMap: acc =>
        (i % 4) match
          case 0 => Reader.ask[Int].at[RSW].map(acc + _)
          case 1 => State.get[Int].at[RSW].flatMap(s => State.set[Int](s + i).at[RSW]).map(_ => acc)
          case 2 => Writer.tell(s"t$i").at[RSW].map(_ => acc + 1)
          case _ => State.get[Int].at[RSW].map(acc + _)

  test("stack: 100 000 operations through a stack of three on a 256 KB thread") {
    var out: Either[Throwable, Int] = Left(IllegalStateException("never ran"))
    val t = new Thread(null, () => out =
      try
        val a: Int ! (Writer % String + State % Int) = mixed(100000).handle(Reader(1))
        val b: (Seq[String], Int) ! State % Int = a.handle(Writer.log[String])
        Right(b.handle(State(0)).run._2._2)
      catch case e: Throwable => Left(e), "small", 256L * 1024)
    t.start()
    t.join()
    assert(out.isRight, s"$out")
  }
