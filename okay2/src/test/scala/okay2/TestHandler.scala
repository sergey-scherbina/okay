package okay2

/** the effect of docs/your-own-effect.md, for the forms — the Scala 3 core's `enum Accounts` */
sealed trait Accounts extends Row { type Op[+A] = Accounts.Op[A] }

object Accounts {
  sealed trait Op[+A]
  final case class Find(id: Long) extends Op[Option[String]]
  final case class Save(id: Long, name: String) extends Op[Option[String]]

  implicit val effect: Effect[Accounts] = Effect.of[Accounts]

  def find(id: Long): Option[String] ! Accounts = Free.inject[Accounts, Option[String]](Find(id))
  def save(id: Long, name: String): Option[String] ! Accounts = Free.inject[Accounts, Option[String]](Save(id, name))
}

/** specs/handler-forms.md and api-levels.md, level 1 in Scala 2: handler values, `p.handle(h)`, the four forms */
class TestHandler extends munit.FunSuite {
  import Accounts._

  // Scala 2 does not refine `X` from `Find <: Op[Option[String]]` in a match, so a clause asserts its answer
  // (TestStatic's `answer`, the Scala 3 core's derivation does not need it)
  def answer[X](x: Any): X = x.asInstanceOf[X]

  def rename(id: Long, to: String): Option[String] ! Accounts =
    find(id).flatMap {
      case Some(_) => save(id, to)
      case None => pure[Accounts, Option[String]](None)
    }

  val counter: Int ! (State[Int] + Reader[Int]) =
    for {
      r <- Reader.ask[Int].plus[State[Int]]
      s <- State.get[Int].plus[Reader[Int]]
      _ <- State.set(s + r).plus[Reader[Int]]
    } yield s * 10

  test("a handler value takes its effect off the row, in any order, one or several at a call") {
    assertEquals(counter.handle(State(1)).handle(Reader(5)).run, (6, 10))
    assertEquals(counter.handle(Reader(5)).handle(State(1)).run, (6, 10))
    assertEquals(counter.handle(Reader(5), State(1)).run, (6, 10))
    val e: Either[String, (Int, Int)] = Throws.raise[String, Int]("no").plus[State[Int]].handle(State(0), Throws.either[String]).run
    assertEquals(e, Left("no"))
  }

  test("the rest of the row is what the type says: one handler leaves the other effect") {
    val half: (Int, Int) ! Reader[Int] = counter.handle(State(1))
    assertEquals(half.handle(Reader(5)).run, (6, 10))
  }

  test("a handler whose effect the row does not hold is a compile error") {
    val errs = compileErrors("Reader.ask[Int].handle(State(0))")
    assert(errs.contains("does not hold"), errs)
  }

  test("the ready values: Choose.all, Throws.option, Writer.log, Once.memo") {
    val branches: Int ! Choose = choose(1, 2).flatMap(a => choose(10, 20).map(_ + a))
    assertEquals(branches.handle(Choose.all).run, Seq(11, 21, 12, 22))
    assertEquals(abort[Int].handle(Throws.option).run, None)
    val told: Int ! Writer[String] = Writer.tell("a").flatMap(_ => Writer.tell("b")).map(_ => 3)
    assertEquals(told.handle(Writer.log[String]).run, (Vector("a", "b"), 3))
    var runs = 0
    val shared = Once.once[Int, Pure](Free.delay(() => { runs += 1; pure[Pure, Int](7) }))
    assertEquals(shared.flatMap(a => shared.map(_ + a)).handle(Once.memo).run, 14)
    assertEquals(runs, 1)
  }

  test("Reset as a value: the continuation's reset") {
    val p: Int ! Shift[Int] = shift[Int, Int, Pure](k => for { a <- k(1); b <- k(10) } yield a + b).map(_ * 2)
    assertEquals(p.handle(Reset[Int]).run, 22)
  }

  test("answer: an answer per operation") {
    val db = scala.collection.mutable.Map(7L -> "ada")
    val live: Handler[Accounts, Handler.Id] = Handler[Accounts](new Answers[Accounts] {
      def handle[X](e: Op[X]): X = e match {
        case Find(id) => answer[X](db.get(id))
        case Save(id, name) => answer[X](db.put(id, name))
      }
    })
    assertEquals(rename(7, "grace").handle(live).run, Some("ada"))
    assertEquals(db(7L), "grace")
  }

  test("answer: Reader re-expressed, the same as Reader(r)") {
    val asReader = Handler[Reader[Int]].answer(new Answers[Reader[Int]] {
      def handle[X](e: Reader.Op[Int, X]): X = answer[X](40)
    })
    assertEquals(counter.handle(asReader, State(2)).run, counter.handle(Reader(40), State(2)).run)
  }

  test("state: the store as the state") {
    val store = Handler[Accounts].state(Map(7L -> "ada"))(new Handler.StateClause[Accounts, Map[Long, String]] {
      def apply[X](m: Map[Long, String], e: Op[X]): (Map[Long, String], X) = e match {
        case Find(id) => (m, answer[X](m.get(id)))
        case Save(id, name) => (m.updated(id, name), answer[X](m.get(id)))
      }
    })
    assertEquals(rename(7, "grace").handle(store).run, (Map(7L -> "grace"), Some("ada")))
  }

  test("state: State re-expressed by its two operations, the same as State(s)") {
    val asState = Handler[State[Int]].state(1)(new Handler.StateClause[State[Int], Int] {
      def apply[X](s: Int, e: State.Op[Int, X]): (Int, X) = e match {
        case _: State.Get[_] => (s, answer[X](s))
        case State.Update(f) => val (b, s2) = f(s); (s2, b)
      }
    })
    assertEquals(counter.handle(asState, Reader(5)).run, counter.handle(State(1), Reader(5)).run)
  }

  test("into: each operation a program in effects the rest of the row holds") {
    val viaState = Handler[Accounts].into[State[Map[Long, String]]](new Interpret[Accounts, State[Map[Long, String]]] {
      def apply[X](e: Op[X]): X ! State[Map[Long, String]] = e match {
        case Find(id) => State.get[Map[Long, String]].map(m => answer[X](m.get(id)))
        case Save(id, name) => State.update[Map[Long, String], X](m => (answer[X](m.get(id)), m.updated(id, name)))
      }
    })
    val p: Option[String] ! (Accounts + State[Map[Long, String]]) = rename(7, "grace")
    assertEquals(p.handle(viaState, State(Map(7L -> "ada"))).run, (Map(7L -> "grace"), Some("ada")))
  }

  test("control: a clause that resumes once, in tail position, captures nothing — 100 000 asks deep") {
    val asReader = Handler[Reader[Int]].control[Handler.Id](new Handler.Ret[Handler.Id] { def apply[A](a: A): A = a })(
      new Handler.Control[Reader[Int], Handler.Id] {
        def apply[X, A, G <: Row](e: Reader.Op[Int, X], k: X => A ! G): A ! G = k(answer[X](7))
      })
    def asks(k: Int): Int ! Reader[Int] = if (k == 0) pure[Reader[Int], Int](0) else Reader.ask[Int].flatMap(r => asks(k - 1).map(_ + r))
    assertEquals(asks(100000).handle(asReader).run, 700000)
    assertEquals(asks(3).handle(asReader).run, asks(3).handle(Reader(7)).run)
  }

  test("control: a clause that resumes and then works on the answer still captures, and is right") {
    val twice = Handler[Reader[Int]].control[Handler.Id](new Handler.Ret[Handler.Id] { def apply[A](a: A): A = a })(
      new Handler.Control[Reader[Int], Handler.Id] {
        def apply[X, A, G <: Row](e: Reader.Op[Int, X], k: X => A ! G): A ! G = k(answer[X](1)).flatMap(_ => k(answer[X](2)))
      })
    val p: Int ! Reader[Int] = Reader.ask[Int].map(_ * 10)
    assertEquals(p.handle(twice).run, 20)
  }

  test("control: abort, and resume many times") {
    val maybe = Handler[Abort].control[Option](new Handler.Ret[Option] { def apply[A](a: A): Option[A] = Some(a) })(
      new Handler.Control[Abort, Option] {
        def apply[X, A, G <: Row](e: Throws.Op[Unit, X], k: X => Option[A] ! G): Option[A] ! G = pure[G, Option[A]](None)
      })
    assertEquals(abort[Int].map(_ + 1).handle(maybe).run, None)
    assertEquals(pure[Abort, Int](1).handle(maybe).run, Some(1))
    val all = Handler.control[Choose, List](new Handler.Ret[List] { def apply[A](a: A): List[A] = List(a) })(
      new Handler.Control[Choose, List] {
        def apply[X, A, G <: Row](e: Choose.Op[X], k: X => List[A] ! G): List[A] ! G =
          !.traverse[X, List[A], G](e.as)(k).map(_.toList.flatten)
      })
    val branches: Int ! Choose = choose(1, 2).flatMap(a => choose(10, 20).map(_ + a))
    assertEquals(branches.handle(all).run, List(11, 21, 12, 22))
  }
}
