package okay.std.tests

import okay.*
import okay.std.*


/**
 * specs/context-functions.md, ctx-reader-bridge: `Reader.lift` and
 * `Reader.unlift` — the round trips, and one Ask per lift.
 */
class TestReaderBridge extends munit.FunSuite:

  test("lift: a context function is a Reader program that asks once") {
    var applied = 0
    val cf: Int ?=> Int = { applied += 1; summon[Int] * 2 }
    val p: Int ! Reader % Int = Reader.lift(cf)
    assertEquals(applied, 0)                                   // nothing ran at lift
    assertEquals(Reader.run(21)(p).run, 42)
    assertEquals(applied, 1)
    // one Ask per lift: count the operations by stepping
    var asks = 0
    val counted = p.handle(Handler.answering[Reader % Int]([X] => (e: Reader[Int, X]) => e match
      case Reader.Ask() => asks += 1; 5
      case Reader.Asks(f) => asks += 1; f(5)))
    assertEquals(counted.run, 10)
    assertEquals(asks, 1)
  }

  // reader-asks-op: lift/read were the shared Ask plus a map
  test("lift is ONE operation, Asks, and local answers it through f") {
    val p: Int ! Reader % Int = Reader.lift[Int, Int]((e: Int) ?=> e * 10)
    assertEquals(Reader.run(4)(p).run, 40)
    val scoped = Reader.local[Int, Int, Reader % Int](_ + 1)(p)
    assertEquals(Reader.run(4)(scoped).run, 50)
  }

  test("a forwarded Asks is answered by the outer Reader") {
    type Fx = Reader % Int + Writer % String
    val p: Int ! Fx =
      Reader.lift[Int, Int]((e: Int) ?=> e + 2).at[Fx].flatMap(n => Writer.tell(s"n=$n").at[Fx].map(_ => n))
    assertEquals((Reader.run(5)(Writer.run(p))).run, (Seq("n=7"), 7))
  }

  test("unlift: a Reader program is a context function run under the ambient E, the rest forwarded") {
    type R = Reader % Int + Writer % String
    val p: Int ! R =
      Reader.ask[Int].at[R].flatMap(e =>
        Writer.tell(s"env=$e").at[R].map(_ => e + 1))
    val cf: Int ?=> Int ! Writer % String = Reader.unlift[Int, Int, R](p)
    val (log, a) = Writer.run(provide(41)(cf)).run
    assertEquals((log, a), (Seq("env=41"), 42))
  }

  test("the round trips: unlift(lift(cf)) is cf; lift(unlift(p)) at e is p at e") {
    val cf: String ?=> Int = summon[String].length
    val back: String ?=> Int ! Pure = Reader.unlift[String, Int, Reader % String](Reader.lift(cf))
    assertEquals(provide("four")(back).run, provide("four")(cf))
    val p: Int ! Reader % Int = Reader.ask[Int].map(_ * 3)
    val again: Int ! Reader % Int = Reader.lift(Reader.unlift[Int, Int, Reader % Int](p) match { case prog => prog.run })
    assertEquals(Reader.run(7)(again).run, Reader.run(7)(p).run)
  }
