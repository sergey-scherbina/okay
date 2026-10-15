package okay.std.tests

import okay.*
import okay.std.*

import okay.core.Split

/**
 * docs/your-own-effect.md's handler forms, VERBATIM (doc-snippet-debt):
 * each line as the page prints it, then asserted. The four forms are the
 * root's `Handler.answering`, `.state`, `p.translate` and `Handler.deep`.
 */
object YourOwnAccounts:

  enum Accounts[+A] derives Effect:
    case Find(id: Long) extends Accounts[Option[String]]
    case Save(id: Long, name: String) extends Accounts[Option[String]]

  export Accounts.{Find, Save}

  def rename(id: Long, to: String): Option[String] ! Accounts =
    effect(Find(id)).flatMap(old => effect(Save(id, to)).map(_ => old))

  def stored[A, Es[_]](p: A ! Es)(using s: Split[Es, Accounts]): A ! (State % Map[Long, String] + s.Rest) =
    p.translate[Accounts, State % Map[Long, String]]:
      [X] => (e: Accounts[X]) => e match
        case Find(id) => State.get[Map[Long, String]].map(_.get(id))
        case Save(id, name) => State.update[Map[Long, String], Option[String]](m => (m.get(id), m.updated(id, name)))

class TestDocExamplesYourOwnEffect extends munit.FunSuite:
  import YourOwnAccounts.*

  test("your-own-effect: form 1, answer each operation") {
    val db = scala.collection.mutable.Map(7L -> "ada")
    val live: Handler[Accounts, [A] =>> A] = Handler.answering[Accounts]:
      [X] => (e: Accounts[X]) => e match
        case Find(id) => db.get(id)
        case Save(id, name) => db.put(id, name)
    assertEquals(rename(7, "grace").handle(live).run, Some("ada"))
    assertEquals(db.toMap, Map(7L -> "grace"))
  }

  test("your-own-effect: form 2, thread a state") {
    val store = Handler.state[Accounts, Map[Long, String]](Map(7L -> "ada")):
      [X] => (m: Map[Long, String], e: Accounts[X]) => e match
        case Find(id) => (m, m.get(id))
        case Save(id, name) => (m.updated(id, name), m.get(id))
    val renamed = rename(7, "grace").handle(store).run   // (Map(7 -> grace), Some(ada))
    assertEquals(renamed, (Map(7L -> "grace"), Some("ada")))
  }

  test("your-own-effect: form 3, interpret into other effects") {
    assertEquals(State.handle[Map[Long, String]](Map(7L -> "ada"))(stored(rename(7, "grace"))).run,
      (Map(7L -> "grace"), Some("ada")))
  }

  test("your-own-effect: form 4, hold the continuation") {
    val asMaybe = Handler.deep[Maybe, Option]([A] => (a: A) => Some(a)):
      [A, X] => (e: Maybe[X], resume: X => Option[A] ! Pure) => e.value match
        case Some(x) => resume(x)
        case None => pure[Option[A], Pure](None)
    assertEquals(Option(2).maybe.map(_ + 1).handle(asMaybe).run, Some(3))
    assertEquals(Maybe.none[Int].map(_ + 1).handle(asMaybe).run, None)
  }
