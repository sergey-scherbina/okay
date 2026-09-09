package okay

import okay.RowLift.{at, plus}
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

  test("a row has no instance of its own, and needs none") {
    // the erasure-based fallback is gone: every signature declares its
    // test with `derives Effect`, and a ROW is split by testing its
    // PARTS — `Handler.union[F, G]` and `<|>` ask only about the left
    // side, so a composite instance is never needed. It is also not
    // available, deliberately:
    assert(!scala.compiletime.testing.typeChecks(
      "summon[okay.TypeableK[TestDeriveEffect.this.Db + okay.Writer % String]]"))
    // what IS available is each part, found with no import at all
    assertEquals(summon[TypeableK[Db]].unapply[Option[Int]](Db.Get("a")).isDefined, true)
    assertEquals(summon[TypeableK[Writer % String]].unapply[Unit](Writer("x")).isDefined, true)
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

  test("interpret: one effect into others, the rest of the row carried through") {
    type Store = Map[String, Int]
    type Tracked = State % Store + Writer % String

    def tracked[A, F[+_]](p: A ! (Db + F)): A ! (Tracked + F) =
      type R = Tracked + F
      !.interpret(p):
        [X] => (e: Db[X]) => e match
          case Db.Get(k) =>
            for
              s <- State.get[Store].at[R]
              _ <- Writer.tell(s"get($k)").at[R]
            yield s.get(k)
          case Db.Put(k, v) =>
            for
              s <- State.get[Store].at[R]
              _ <- State.set(s + (k -> v)).at[R]
              _ <- Writer.tell(s"put($k,$v)").at[R]
            yield ()

    val prog: Option[Int] ! Db =
      for
        _ <- Db.Put("b", 2).perform
        r <- Db.Get("b").perform
      yield r
    val (store, (told, answer)) =
      State.run[Store, (Seq[String], Option[Int])](Map.empty)(
        Writer.run[String, Option[Int], State % Store](tracked[Option[Int], Pure](prog)))
    assertEquals(answer, Some(2))
    assertEquals(told, Seq("put(b,2)", "get(b)"))
    assertEquals(store, Map("b" -> 2))
  }

  test("tracing records a program's asks and answers none of them") {
    type Store = Map[String, Int]
    type R = Db + Writer % String

    def stored[A, F[+_]](p: A ! (Db + F)): A ! (State % Store + F) =
      type S = State % Store + F
      !.interpret(p):
        [X] => (e: Db[X]) => e match
          case Db.Get(k)    => State.get[Store].at[S].map(_.get(k))
          case Db.Put(k, v) =>
            for
              s <- State.get[Store].at[S]
              _ <- State.set(s + (k -> v)).at[S]
            yield ()

    val prog: Option[Int] ! Db =
      for
        _ <- Db.Put("b", 2).perform
        r <- Db.Get("b").perform
      yield r

    // the recorder knows nothing about Db beyond toString, and the
    // interpreter knows nothing about the Writer: two layers, one job
    // each, composed
    val both: Option[Int] ! (State % Store + Writer % String) =
      stored[Option[Int], Writer % String](
        !.tracing(prog)([X] => (e: Db[X]) => e.toString))
    val (store, (told, answer)) =
      State.run[Store, (Seq[String], Option[Int])](Map.empty)(
        Writer.run[String, Option[Int], State % Store](both))
    assertEquals(answer, Some(2))
    assertEquals(told, Seq("Put(b,2)", "Get(b)"))
    assertEquals(store, Map("b" -> 2))

    // tracing alone answers nothing: the same program, still asking
    val traced: Option[Int] ! R = !.tracing(prog)([X] => (e: Db[X]) => e.toString)
    val (log, plain) =
      !.run(Writer.run[String, Option[Int], Pure](
        !.translate[Option[Int], Db, Writer % String](traced):
          [X] => (e: Db[X]) => pure(handler.handle(e))))
    assertEquals(plain, Some(2))
    assertEquals(log, Seq("Put(b,2)", "Get(b)"))
  }
}
