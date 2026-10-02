package okay

/** handler-infer: the effect read off the cases */
class TestHandlerInfer extends munit.FunSuite:
  import Accounts.*

  def rename(id: Long, to: String): Option[String] ! Accounts =
    Find(id).perform.flatMap {
      case Some(_) => Save(id, to).perform
      case None => pure(None)
    }

  test("p.handle(Handler.answer { case … }): no [F]") {
    val db = scala.collection.mutable.Map(7L -> "ada")
    val r = rename(7, "grace").handle(Handler.answer {
      case Find(id) => db.get(id)
      case Save(id, name) => db.put(id, name)
    }).run
    assertEquals(r, Some("ada"))
    assertEquals(db(7L), "grace")
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

  test("the same checks: a wrong answer, a missing operation") {
    assert(compileErrors("Handler.answer { case Accounts.Find(id) => 42; case Accounts.Save(_, _) => None }")
      .contains("Find answers Option[String], but this case gives Int"))
    assert(compileErrors("Handler.answer { case Accounts.Find(id) => None }")
      .contains("not every operation of Accounts is handled: Save"))
  }

  test("an effect with parameters besides its answer keeps its [F]") {
    val errs = compileErrors("Handler.answer { case Reader.Ask() => 1; case Reader.Asks(g) => g(1) }")
    assert(errs.contains("Reader has parameters besides its answer"), errs)
  }
