package okay


import okay.Direct.{direct, given}
import scala.language.implicitConversions

/**
 * Declaring an effect: what still has to be written by hand, and what
 * does not. (On the root's words: the row-splitting `TypeableK`, `relay`,
 * `interpret` and `tracing` tests went with okay-freer's classic road.)
 */
class TestDeriveEffect extends munit.FunSuite {

  /** no companion, no given, no constructors — the operations and
   * their answer types, and that is the whole declaration */
  enum Db[+A] derives Effect:
    case Get(k: String) extends Db[Option[Int]]
    case Put(k: String, v: Int) extends Db[Unit]

  val handler: Answers[Db] = new:
    var store = Map("a" -> 1)
    def handle[A](e: Db[A]): A = e match
      case Db.Get(k)    => store.get(k)
      case Db.Put(k, v) => store = store + (k -> v); ()

  test("an operation is a program needing its effect") {
    val p: Option[Int] ! Db = effect[Db, Option[Int]](Db.Get("a"))
    assertEquals(p.runWith(using handler), Some(1))
    assertEquals(effect[Db, Option[Int]](Db.Get("zz")).runWith(using handler), None)
  }

  test("derives Effect registers the signature for direct auto-coloring") {
    // no `.?`, no `.reflect`: the marker comes with the declaration,
    // and the DirectCtx gate still means this colors only in here
    def get(k: String): Option[Int] ! Db = effect[Db, Option[Int]](Db.Get(k))
    val prog: Option[Int] ! Db = direct {
      val a: Option[Int] = get("a")
      a.map(_ * 10)
    }
    assertEquals(prog.runWith(using handler), Some(10))
  }
}
