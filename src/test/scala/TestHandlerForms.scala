package okay

import okay.Row.*

/** the effect of docs/your-own-effect.md, here for the forms */
enum Accounts[+A] derives Effect:
  case Find(id: Long) extends Accounts[Option[String]]
  case Save(id: Long, name: String) extends Accounts[Option[String]]

/** an environment with a generic constructor, for the case form */
enum Env[R, +A] derives Effect:
  case Get() extends Env[R, R]

/** specs/handler-forms.md: the author's four forms, each a level-1 value */
class TestHandlerForms extends munit.FunSuite:
  import Accounts.*

  def rename(id: Long, to: String): Option[String] ! Accounts =
    Find(id).perform.flatMap {
      case Some(_) => Save(id, to).perform
      case None => pure(None)
    }

  test("answer: an answer per operation") {
    val db = scala.collection.mutable.Map(7L -> "ada")
    val live: Handler[Accounts, [A] =>> A] = Handler.answer {
      case Find(id) => db.get(id)
      case Save(id, name) => db.put(id, name)
    }
    assertEquals(rename(7, "grace").handle(live).run, Some("ada"))
    assertEquals(db(7L), "grace")
  }

  test("answer: Reader re-expressed, the same as Reader(r)") {
    val p: Int ! Reader % Int + State % Int =
      for
        r <- Reader.ask[Int].plus[State % Int]
        s <- State.get[Int].plus[Reader % Int]
      yield r + s
    val asReader = Handler[Reader % Int].answer.poly { [X] => (e: Reader[Int, X]) => e match
      case Reader.Ask() => 40
      case Reader.Asks(g) => g(40)
    }
    assertEquals(p.handle(asReader).handle(State(2)).run, p.handle(Reader(40)).handle(State(2)).run)
  }

  test("state: a counter of operations, the store as the state") {
    val store = Handler[Accounts].state(Map(7L -> "ada")) {
      case (m, Find(id)) => (m, m.get(id))
      case (m, Save(id, name)) => (m.updated(id, name), m.get(id))
    }
    val renamed = rename(7, "grace").handle(store).run   // (Map(7 -> grace), Some(ada))
    assertEquals(renamed, (Map(7L -> "grace"), Some("ada")))
  }

  test("cases over a generic constructor: Env's Get[R] read at Get[Int]") {
    val asEnv = Handler[Env % Int].answer { case Env.Get() => 40 }
    assertEquals(effect[Env % Int, Int](Env.Get()).map(_ + 2).handle(asEnv).run, 42)
    assert(compileErrors("""Handler[Env % Int].answer { case Env.Get() => "no" }""").contains("Get answers Int, but this case gives String"))
  }

  test("an answer the caller chooses (Reader's Asks[R, A]): only the operation's own data can give it") {
    val asReader = Handler[Reader % Int].answer {
      case Reader.Ask() => 40
      case Reader.Asks(g) => g(40)
    }
    assertEquals(effect[Reader % Int, Int](Reader.Asks[Int, Int](_ + 2)).handle(asReader).run, 42)
    assertEquals(effect[Reader % Int, String](Reader.Asks[Int, String](_.toString)).handle(asReader).run, "40")
    val errs = compileErrors("""Handler[Reader % Int].answer { case Reader.Ask() => 1; case Reader.Asks(g) => "oops" }""")
    assert(errs.contains("Asks answers what its caller chose"), errs)
  }

  test("state: State re-expressed, the same as State(s)") {
    val p: Int ! State % Int = State.get[Int].flatMap(s => State.set(s + 1).map(_ => s * 2))
    val asState = Handler[State % Int].state[Int](5).poly { [X] => (s: Int, e: State[Int, X]) => e match
      case State.Get() => (s, s)
      case State.Set(n) => (n, n)
      case State.Modify(g) => { val n = g(s); (n, n) }
      case State.Update(g) => { val (b, n) = g(s); (n, b) }
    }
    assertEquals(p.handle(asState).run, p.handle(State(5)).run)
  }

  test("where the case form stops: State's Set(s: S) answers the type its field has, pointing at .poly") {
    val errs = compileErrors("""
      Handler[State % Int].state[Int](5) {
        case (s, State.Get()) => (s, s)
        case (_, State.Set(n)) => (n, n)
        case (s, State.Modify(g)) => (s, s)
        case (s, State.Update(g)) => (s, s)
      }""")
    assert(errs.contains("Set answers the type its field `s` has"), errs)
    assert(errs.contains(".poly"), errs)
  }

  test("into: each operation a program in State, which the rest of the row holds") {
    val stored = Handler[Accounts].into[State % Map[Long, String]] {
      case Find(id) => State.get[Map[Long, String]].map(_.get(id))
      case Save(id, name) => State.update[Map[Long, String], Option[String]](m => (m.get(id), m.updated(id, name)))
    }
    val p: Option[String] ! Accounts + State % Map[Long, String] = rename(7, "grace").plus[State % Map[Long, String]]
    assertEquals(p.handle(stored).handle(State(Map(7L -> "ada"))).run, (Map(7L -> "grace"), Some("ada")))
  }

  test("control: Maybe re-expressed (resume dropped), the same as Maybe.option") {
    val asMaybe = Handler.control[Maybe, Option]([A] => (a: A) => Some(a)):
      [X, A, G[+_]] => (e: Maybe[X], resume: X => Option[A] ! G) => e.value match
        case Some(x) => resume(x)
        case None => pure[G, Option[A]](None)
    val p: Int ! Maybe = Maybe.none[Int].map(_ + 1)
    val q: Int ! Maybe = pure[Maybe, Int](41).map(_ + 1)
    assertEquals(p.handle(asMaybe).run, p.handle(Maybe.option).run)
    assertEquals(q.handle(asMaybe).run, Some(42))
  }

  test("control: Choose re-expressed (resume for every branch), the same as Choose.all") {
    val asChoose = Handler.control[Choose, Seq]([A] => (a: A) => Seq(a)):
      [X, A, G[+_]] => (e: Choose[X], resume: X => Seq[A] ! G) =>
        !.foldM(e.as)(Seq.empty[A])((acc, x) => resume(x).map(acc ++ _))
    val c: Int ! Choose = choose(1, 2).flatMap(x => choose(10, 20).map(_ + x))
    assertEquals(c.handle(asChoose).run, c.handle(Choose.all).run)
  }

  test("cases are checked: a case answering the wrong type is refused, naming the operation") {
    val errs = compileErrors("""
      Handler[Accounts].answer {
        case Accounts.Find(id) => 42
        case Accounts.Save(id, name) => None
      }""")
    assert(errs.contains("Find answers Option[String], but this case gives Int"), errs)
    val pairs = compileErrors("""
      Handler[Accounts].state[Int](0) {
        case (n, Accounts.Find(_)) => (n, "no")
        case (n, Accounts.Save(_, _)) => (n, None)
      }""")
    assert(pairs.contains("Find answers Option[String], but this case gives String"), pairs)
  }

  test("cases are checked: every operation handled, or a compile error naming the missing one") {
    val errs = compileErrors("""
      Handler[Accounts].answer {
        case Accounts.Find(id) => None
      }""")
    assert(errs.contains("not every operation of Accounts is handled: Save"), errs)
    val guarded = compileErrors("""
      Handler[Accounts].answer {
        case Accounts.Find(id) => None
        case Accounts.Save(id, _) if id > 0 => None
      }""")
    assert(guarded.contains("is handled: Save"), guarded)
  }

  test("cases are checked: a wildcard may only throw") {
    val errs = compileErrors("""
      Handler[Accounts].answer {
        case Accounts.Find(id) => None
        case _ => None
      }""")
    assert(errs.contains("only a `throw` may stand here"), errs)
    val ok = Handler[Accounts].answer {
      case Find(id) => None
      case _ => throw new IllegalStateException("not here")
    }
    assertEquals(Find(1).perform.handle(ok).run, None)
  }

  test("from: an existing Answers[F] as a handler value") {
    val answers: Answers[Accounts] = new Answers[Accounts]:
      def handle[A](e: Accounts[A]): A = e match
        case Find(_) => Some("ada")
        case Save(_, _) => Some("ada")
    assertEquals(rename(7, "grace").handle(Handler.from(answers)).run, Some("ada"))
  }

  test("forwarding: each form leaves the rest of the row, in order") {
    val p: Option[String] ! Accounts + Writer % String =
      for
        _ <- Writer.tell("before").plus[Accounts]
        r <- rename(7, "grace").plus[Writer % String]
        _ <- Writer.tell("after").plus[Accounts]
      yield r
    val live = Handler[Accounts].answer {
      case Find(_) => Some("ada")
      case Save(_, _) => Some("ada")
    }
    assertEquals(p.handle(live).handle(Writer.log).run, (Seq("before", "after"), Some("ada")))
  }

  test("stack: 100 000 operations through answer, state and control") {
    def many(n: Int): Int ! Accounts =
      if n == 0 then pure(0) else Find(n.toLong).perform.flatMap(_ => !.tailcall(many(n - 1)).map(_ + 1))
    val a = Handler[Accounts].answer.poly { [X] => (e: Accounts[X]) => e match
      case Find(_) => None
      case Save(_, _) => None
    }
    val s = Handler[Accounts].state[Int](0) {
      case (n, Find(_)) => (n + 1, None)
      case (n, Save(_, _)) => (n, None)
    }
    val c = Handler.control[Accounts, [A] =>> A]([A] => (a: A) => a):
      [X, A, G[+_]] => (e: Accounts[X], resume: X => A ! G) => e match
        case Find(_) => resume(None)
        case Save(_, _) => resume(None)
    assertEquals(many(100000).handle(a).run, 100000)
    assertEquals(many(100000).handle(s).run, (100000, 100000))
    assertEquals(many(100000).handle(c).run, 100000)
  }
