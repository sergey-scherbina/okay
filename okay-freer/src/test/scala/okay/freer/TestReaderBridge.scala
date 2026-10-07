package okay.freer


import okay.freer.!.*

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
    assertEquals(!.run(Reader.run[Int, Int, Pure](21)(p)), 42)
    assertEquals(applied, 1)
    // one Ask per lift: count the operations by stepping
    var asks = 0
    val counted = relay[Int, Int, Reader % Int, Pure](p)(pure(_)):
      [X, Y] => e => e match
        case Reader.Ask() => asks += 1; Cps.Pure(5)
        case Reader.Asks(f) => asks += 1; Cps.Pure(f(5))
    assertEquals(!.run(counted), 10)
    assertEquals(asks, 1)
  }

  // reader-asks-op: lift/read were the shared Ask plus a map
  test("lift is ONE operation, Asks, and local answers it through f") {
    Reader.lift[Int, Int]((e: Int) ?=> e + 1) match
      case Free.Inject(Reader.Asks(_)) => ()
      case other => fail(s"lift is not one operation: $other")
    val p: Int ! Reader % Int = Reader.lift[Int, Int]((e: Int) ?=> e * 10)
    assertEquals(!.run(Reader.run[Int, Int, Pure](4)(p)), 40)
    val scoped = Reader.local[Int, Int, Pure](_ + 1)(p)
    assertEquals(!.run(Reader.run[Int, Int, Pure](4)(scoped)), 50)
  }

  test("a forwarded Asks is answered by the outer Reader") {
    type Fx = Reader % Int + Writer % String
    import okay.freer.Row.at
    val p: Int ! Fx =
      Reader.lift[Int, Int]((e: Int) ?=> e + 2).at[Fx].flatMap(n => Writer.tell(s"n=$n").at[Fx].map(_ => n))
    assertEquals(!.run(Reader.run[Int, (Seq[String], Int), Pure](5)(Writer.run[String, Int, Reader % Int](p))), (Seq("n=7"), 7))
  }

  test("unlift: a Reader program is a context function run under the ambient E, the rest forwarded") {
    type R = Reader % Int + Writer % String
    val p: Int ! R =
      !.widen[Int, Reader % Int, Writer % String](Reader.ask[Int]).flatMap(e =>
        !.widen[Unit, Writer % String, Reader % Int](Writer.tell(s"env=$e")).map(_ => e + 1))
    val cf: Int ?=> Int ! Writer % String = Reader.unlift[Int, Int, Writer % String](p)
    val (log, a) = !.run(Writer.run[String, Int, Pure](provide(41)(cf)))
    assertEquals((log, a), (Seq("env=41"), 42))
  }

  test("the round trips: unlift(lift(cf)) is cf; lift(unlift(p)) at e is p at e") {
    val cf: String ?=> Int = summon[String].length
    val back: String ?=> Int ! Pure = Reader.unlift[String, Int, Pure](Reader.lift(cf))
    assertEquals(!.run(provide("four")(back)), provide("four")(cf))
    val p: Int ! Reader % Int = Reader.ask[Int].map(_ * 3)
    val again: Int ! Reader % Int = Reader.lift(Reader.unlift[Int, Int, Pure](p) match { case prog => !.run(prog) })
    assertEquals(!.run(Reader.run[Int, Int, Pure](7)(again)), !.run(Reader.run[Int, Int, Pure](7)(p)))
  }
