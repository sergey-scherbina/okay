package okay

import okay.Rowlift.{at, plus}

/**
 * Declaring an effect: what still has to be written by hand, and what
 * does not.
 */
class TestDeriveEffect extends munit.FunSuite {

  /** no companion, no given, no constructors — the operations and
   * their answer types, and that is the whole declaration */
  enum Db[+A] derives TypeableK:
    case Get(k: String) extends Db[Option[Int]]
    case Put(k: String, v: Int) extends Db[Unit]

  val handler: Handler[Db] = new:
    var store = Map("a" -> 1)
    def handle[A](e: Db[A]): A = e match
      case Db.Get(k)    => store.get(k)
      case Db.Put(k, v) => store = store + (k -> v); ()

  test("perform recovers the answer type from the case") {
    val p: Option[Int] ! Db = Db.Get("a").perform
    assertEquals(p.runWith(using handler), Some(1))
    assertEquals(Db.Get("zz").perform.runWith(using handler), None)
  }

  test("the derived TypeableK splits a row, which is what it is for") {
    type Row = Db + Writer % String
    val p: Option[Int] ! Row =
      for
        _ <- Writer.tell("asking").at[Row]
        r <- Db.Get("a").perform.plus[Writer % String]
      yield r
    val handled: Option[Int] ! (Writer % String) =
      !.relay[Option[Int], Option[Int], Db, Writer % String](p)(pure):
        [X, Y] => e => Cont.Pure(handler.handle(e))
    assertEquals(!.run(Writer.run[String, Option[Int], Pure](handled)),
      (Seq("asking"), Some(1)))
  }

  test("derived and hand-written agree") {
    val derived = summon[TypeableK[Db]]
    val written = typeableK[Db](classOf[Db[?]])
    assertEquals(derived.unapply[Option[Int]](Db.Get("a")).isDefined,
      written.unapply[Option[Int]](Db.Get("a")).isDefined)
    assertEquals(derived.unapply[Option[Int]]("not an operation").isDefined, false)
  }
}
