package okay.freer

/** the helpers: Handler[F] names the effect once, every form delegates to its one implementation */
class TestHandlerFor extends munit.FunSuite:
  import Accounts.*

  def rename(id: Long, to: String): Option[String] ! Accounts =
    Find(id).perform.flatMap {
      case Some(_) => Save(id, to).perform
      case None => pure(None)
    }

  test("Handler[F].state(s0) { case … }: the effect named, the state's type from s0") {
    val r = rename(7, "grace").handle(Handler[Accounts].state(Map(7L -> "ada")) {
      case (m, Find(id)) => (m, m.get(id))
      case (m, Save(id, name)) => (m.updated(id, name), m.get(id))
    }).run
    assertEquals(r, (Map(7L -> "grace"), Some("ada")))
  }

  test("the explicit forms still stand beside them") {
    val h = Handler[Accounts].answer { case Find(_) => None; case Save(_, _) => None }
    assertEquals(rename(7, "grace").handle(h).run, None)
  }

  test("Handler[F] { case … }: the default form, the effect named") {
    val db = scala.collection.mutable.Map(7L -> "ada")
    val r = rename(7, "grace").handle(Handler[Accounts] {
      case Find(id) => db.get(id)
      case Save(id, name) => db.put(id, name)
    }).run
    assertEquals(r, Some("ada"))
    assert(compileErrors("Handler[Accounts] { case Accounts.Find(id) => None }").contains("is handled: Save"))
  }

  test("each helper is its form: Handler.answer[F], Handler.state[F, S], Handler.control[F, O]") {
    val a = rename(7, "grace").handle(Handler.answer[Accounts] { case Find(_) => Some("ada"); case Save(_, _) => Some("ada") }).run
    val b = rename(7, "grace").handle(Handler[Accounts] { case Find(_) => Some("ada"); case Save(_, _) => Some("ada") }).run
    assertEquals(a, b)
    val s1 = rename(7, "grace").handle(Handler.state[Accounts, Int](0) { case (n, Find(_)) => (n + 1, Some("x")); case (n, Save(_, _)) => (n + 1, None) }).run
    val s2 = rename(7, "grace").handle(Handler[Accounts].state(0) { case (n, Find(_)) => (n + 1, Some("x")); case (n, Save(_, _)) => (n + 1, None) }).run
    assertEquals(s1, s2)
  }
