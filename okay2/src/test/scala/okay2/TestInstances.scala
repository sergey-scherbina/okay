package okay2

/**
 * Several instances of ONE signature in one row (spec stage 13): the
 * Scala 3 core's TestTag and TestInstances, and Writer.byValue, and the
 * `Distinct` that reads all three.
 */
class TestInstances extends munit.FunSuite {
  import TestInstances._

  type Small = Tag["small", State[Int]]
  type Big = Tag["big", State[Int]]

  /** an ordinary function, written against a plain State and knowing
   * nothing about keys */
  def bump(by: Int): Int ! State[Int] =
    State.get[Int].flatMap(n => State.set(n + by).map(_ => n))

  test("Tag: one function, two states, one row — and it was not written for it") {
    val p: (Int, Int) ! (Small + Big) =
      for {
        a <- Tag.tag["small", State[Int], Int, Pure](bump(1))
        b <- Tag.tag["big", State[Int], Int, Pure](bump(10))
      } yield (a, b)

    // handling is the effect's OWN: untag one key, run State, repeat
    val afterSmall: (Int, (Int, Int)) ! Big =
      State.handle[Int, (Int, Int), Big](1)(Tag.untag["small", State[Int], (Int, Int), Big](p))
    val (big, (small, answer)) =
      !.run(State.handle[Int, (Int, (Int, Int)), Pure](100)(Tag.untag["big", State[Int], (Int, (Int, Int)), Pure](afterSmall)))
    assertEquals(answer, (1, 100))
    assertEquals(small, 2)  // 1 + 1
    assertEquals(big, 110)  // 100 + 10
  }

  test("Tag: a key tells apart two instances of a signature that carries nothing") {
    type A = Tag["a", Reader[Int]]
    type B = Tag["b", Reader[Int]]
    val p: (Int, Int) ! (A + B) =
      for {
        x <- Tag.one["a", Reader[Int], Int](Reader.Ask[Int]())
        y <- Tag.one["b", Reader[Int], Int](Reader.Ask[Int]())
      } yield (x, y)
    val inner = Reader.run[Int, (Int, Int), A](7)(Tag.untag["b", Reader[Int], (Int, Int), A](p))
    val out = !.run(Reader.run[Int, (Int, Int), Pure](1)(Tag.untag["a", Reader[Int], (Int, Int), Pure](inner)))
    assertEquals(out, (1, 7))
  }

  test("Tag: one key, two SIGNATURES — the signature test tells them apart") {
    type A = Tag["same", Beep]
    type B = Tag["same", Buzz]
    val p: (Int, String) ! (A + B) =
      for {
        x <- Tag.one["same", Beep, Int](Beep.Boop)
        y <- Tag.one["same", Buzz, String](Buzz.Bzz)
      } yield (x, y)
    implicit val ha: Handler[A] = Tag.handler["same", Beep](beep)
    implicit val hb: Handler[B] = Tag.handler["same", Buzz](buzz)
    assertEquals(p.runWith(Handler.union[A, B]), (42, "ada"))
  }

  test("Tag: the SAME signature class under one key is refused at compile time") {
    val errors = compileErrors(
      "okay2.Handler.union[okay2.Tag[\"same\", okay2.Reader[Int]], okay2.Tag[\"same\", okay2.Reader[String]]](" +
        "okay2.Tag.effect, okay2.Tag.handler(null), okay2.Tag.handler(null), implicitly)")
    assert(errors.contains("no runtime test can tell apart"), errors)
    // two keys over one class, and one key over two classes, are good rows
    val _ = implicitly[Distinct[Tag["a", Reader[Int]] + Tag["b", Reader[String]]]]
    val _ = implicitly[Distinct[Tag["same", Beep] + Tag["same", Buzz]]]
    // and a tag beside its own plain signature is too: the key is the difference
    val _ = implicitly[Distinct[Tag["a", Reader[Int]] + Reader[String]]]
  }

  test("Instances: two instances of one signature, told apart by a run-time handle") {
    val alice = Instances.handle("alice")
    val bob = Instances.handle("bob")
    val p: (String, String) ! Instances[Store] =
      for {
        a <- Instances.at(alice)(Store.get)
        b <- Instances.at(bob)(Store.get)
      } yield (a, b)
    val rows = Map(alice -> "ada", bob -> "grace")
    val h = Instances.handler[Store](i => store(rows(i)))
    assertEquals(p.runWith(h), ("ada", "grace"))
  }

  test("Instances: made in a LOOP — what a type could not have listed") {
    val tenants = List("alpha", "beta", "gamma", "delta")
    val handles = tenants.map(Instances.handle)
    val rows = handles.zip(tenants).toMap
    val p: List[String] ! Instances[Store] =
      handles.foldRight(pure[Instances[Store], List[String]](Nil)) { (h, rest) =>
        Instances.at(h)(Store.get).flatMap(x => rest.map(x :: _))
      }
    assertEquals(p.runWith(Instances.handler[Store](i => store(rows(i)))), tenants)
  }

  test("Instances: route sends an ALREADY WRITTEN program to one instance") {
    val one = Instances.handle("one")
    val two = Instances.handle("two")
    val p: (String, String) ! Instances[Store] =
      for {
        a <- Instances.route[Store, String, Pure](one)(Store.get)
        b <- Instances.route[Store, String, Pure](two)(Store.get)
      } yield (a, b)
    val rows = Map(one -> "первый", two -> "второй")
    assertEquals(p.runWith(Instances.handler[Store](i => store(rows(i)))), ("первый", "второй"))
  }

  test("Instances: only strips one to the effect's OWN runner; exhausted discharges the rest") {
    val small = Instances.handle("small")
    val big = Instances.handle("big")
    val p: (Int, Int) ! Instances[State[Int]] =
      for {
        a <- Instances.at(small)(State.get[Int])
        b <- Instances.at(big)(State.get[Int])
      } yield (a, b)
    type I = Instances[State[Int]]
    val afterSmall: (Int, (Int, Int)) ! I =
      State.handle[Int, (Int, Int), I](1)(Instances.only[State[Int], (Int, Int), Pure](small)(p))
    val done: (Int, (Int, (Int, Int))) ! I =
      State.handle[Int, (Int, (Int, Int)), I](10)(Instances.only[State[Int], (Int, (Int, Int)), Pure](big)(afterSmall))
    val (bigEnd, (smallEnd, answer)) = !.run(Instances.exhausted[State[Int], (Int, (Int, (Int, Int))), Pure](done))
    assertEquals(answer, (1, 10))
    assertEquals(smallEnd, 1)
    assertEquals(bigEnd, 10)
  }

  test("Instances: exhausted names the handle that was never stripped") {
    val left = Instances.handle("left-behind")
    val p: Int ! Instances[State[Int]] = Instances.at(left)(State.get[Int])
    val e = intercept[IllegalStateException](!.run(Instances.exhausted[State[Int], Int, Pure](p)))
    assert(e.getMessage.contains("instance(left-behind)"), e.getMessage)
  }

  test("Instances: two SIGNATURES under instances is an ordinary row") {
    val s = Instances.handle("s")
    val r = Instances.handle("r")
    val p: (String, Int) ! (Instances[Store] + Instances[Reader[Int]]) =
      for {
        a <- Instances.at(s)(Store.get)
        b <- Instances.at(r)(Reader.ask[Int])
      } yield (a, b)
    implicit val hs: Handler[Instances[Store]] = Instances.handler[Store](_ => store("ada"))
    implicit val hr: Handler[Instances[Reader[Int]]] = Instances.handler[Reader[Int]](_ => new Handler.Of[Reader[Int]] {
      def handle[A](a: Reader.Op[Int, A]): A = (7: Any).asInstanceOf[A]
    })
    assertEquals(p.runWith(Handler.union[Instances[Store], Instances[Reader[Int]]]), ("ada", 7))
    // and one signature class under two Instances members is refused
    assert(compileErrors("implicitly[okay2.Distinct[okay2.Instances[okay2.Reader[Int]] + okay2.Instances[okay2.Reader[String]]]]")
      .contains("no runtime test can tell apart"))
  }

  test("Writer.byValue: two Writers in one row, routed by the told value's class") {
    import Writer.byValue._
    val p: Unit ! (Writer[String] + Writer[Int]) =
      Writer.tell[String]("a").flatMap(_ => Writer.tell[Int](1)).flatMap(_ => Writer.tell[String]("b")).flatMap(_ => Writer.tell[Int](2))
    val inner: (Vector[String], Unit) ! Writer[Int] = Writer.collect[String, Unit, Writer[Int]](p)
    val (ints, (strings, _)) = !.run(Writer.collect[Int, (Vector[String], Unit), Pure](inner))
    assertEquals(strings, Vector("a", "b"))
    assertEquals(ints, Vector(1, 2))
  }

  test("Writer.byValue: without the import the same row is refused, and the message names the ways out") {
    val errors = compileErrors("okay2.Writer.collect[String, Unit, okay2.Writer[Int]](okay2.TestInstances.twoWriters)")
    assert(errors.contains("no runtime test can tell apart"), errors)
    assert(errors.contains("Writer.byValue"), errors)
    assert(errors.contains("Tag["), errors)
    assert(errors.contains("Instances[F]"), errors)
    assert(!errors.contains("Distinct.unchecked"), errors)
  }
}

object TestInstances {
  /** an effect that carries nothing to compare — the hard case */
  sealed trait Store extends Row { type Op[+A] = Store.Get.type }
  object Store {
    case object Get
    implicit val effect: Effect[Store] = Effect.byClass[Store](Get.getClass)
    def get: String ! Store = Free.inject[Store, String](Get)
  }
  def store(answer: String): Handler[Store] = new Handler.Of[Store] {
    def handle[A](a: Store.Get.type): A = (answer: Any).asInstanceOf[A]
  }

  sealed trait Beep extends Row { type Op[+A] = Beep.Boop.type }
  object Beep {
    case object Boop
    implicit val effect: Effect[Beep] = Effect.byClass[Beep](Boop.getClass)
  }
  sealed trait Buzz extends Row { type Op[+A] = Buzz.Bzz.type }
  object Buzz {
    case object Bzz
    implicit val effect: Effect[Buzz] = Effect.byClass[Buzz](Bzz.getClass)
  }
  val beep: Handler[Beep] = new Handler.Of[Beep] { def handle[A](a: Beep.Boop.type): A = (42: Any).asInstanceOf[A] }
  val buzz: Handler[Buzz] = new Handler.Of[Buzz] { def handle[A](a: Buzz.Bzz.type): A = ("ada": Any).asInstanceOf[A] }

  val twoWriters: Unit ! (Writer[String] + Writer[Int]) = Writer.tell[String]("a").flatMap(_ => Writer.tell[Int](1))
}
