package okay

import okay.Rowlift.{at, plus}
import scala.annotation.nowarn
import okay.Direct.{direct, given}
import scala.language.implicitConversions

/**
 * Declaring an effect: what still has to be written by hand, and what
 * does not.
 */
class TestDeriveEffect extends munit.FunSuite {

  /** no companion, no given, no constructors — the operations and
   * their answer types, and that is the whole declaration */
  enum Db[+A] derives Effect:
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

  @nowarn("msg=cannot be checked at runtime")
  def rowInstance: TypeableK[Db + Writer % String] = summon

  test("a row needs no derived instance, and the generic one is right") {
    // `TypeableK.derived[Db + Writer % String]` does not compile, and
    // the message says why: a ClassTag of a union is its LUB — an
    // interface every operation matches (measured: Serializable for
    // two case classes, scala.reflect.Enum for two enums), so the
    // split would send all of them left and say nothing. The check
    // is in the macro; here is the reason it costs nothing to have.
    val t = rowInstance
    assertEquals(t.unapply[Option[Int]](Db.Get("a")).isDefined, true)
    assertEquals(t.unapply[Option[Int]](Writer("x")).isDefined, true)
    assertEquals(t.unapply[Option[Int]]("neither").isDefined, false)
  }

  test("any handler can record: tracing is a decorator, not a second handler") {
    val log = scala.collection.mutable.ListBuffer[Any]()
    val p: Option[Int] ! Db =
      for
        _ <- Db.Put("b", 2).perform
        r <- Db.Get("b").perform
      yield r
    assertEquals(p.runWith(using handler.tracing(log += _)), Some(2))
    assertEquals(log.map(_.toString).toSeq, Seq("Put(b,2)", "Get(b)"))
  }

  test("derives Effect is the TypeableK a row split asks for") {
    // the instance in Db's companion is an Effect; everything that
    // wants a TypeableK finds it, because Effect IS one
    val asEffect = summon[Effect[Db]]
    val asTypeable: TypeableK[Db] = summon[TypeableK[Db]]
    assert(asTypeable eq asEffect)
    assertEquals(asTypeable.unapply[Option[Int]](Db.Get("a")).isDefined, true)
    assertEquals(asTypeable.unapply[Option[Int]](Writer("x")).isDefined, false)
  }

  test("derives Effect registers the signature for direct auto-coloring") {
    // no `.!?`, no `.reflect`: the marker comes with the declaration,
    // and the DirectCtx gate still means this colors only in here
    def get(k: String): Option[Int] ! Db = Db.Get(k).perform
    val prog: Option[Int] ! Db = direct {
      val a: Option[Int] = get("a")
      a.map(_ * 10)
    }
    assertEquals(prog.runWith(using handler), Some(10))
  }
}
