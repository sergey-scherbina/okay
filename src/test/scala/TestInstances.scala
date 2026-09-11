package okay

import okay.RowLift.{at as liftAt, plus}

/**
 * Instances of an ARBITRARY effect, made at run time — the corner
 * `Tag` and `Refs` leave open (docs/many-instances.md).
 */
class TestInstances extends munit.FunSuite:

  /** an effect that carries nothing to compare — the hard case */
  enum Store[+A] derives okay.Effect:
    case Get() extends Store[String]

  test("two instances of one signature, told apart by a run-time handle") {
    val alice = Instances.handle("alice")
    val bob = Instances.handle("bob")

    val p: (String, String) ! Instances.Of[Store] =
      for
        a <- Instances.at[Store](alice)(Store.Get())
        b <- Instances.at[Store](bob)(Store.Get())
      yield (a, b)

    val rows = Map(alice -> "ada", bob -> "grace")
    val h = Instances.handler[Store](i => new:
      def handle[A](e: Store[A]): A = e match { case Store.Get() => rows(i) })

    assertEquals(p.runWith(using h), ("ada", "grace"))
  }

  test("instances made in a LOOP — what a type could not have listed") {
    // the tenants are data: a type cannot name them, and `Tag` would
    // need a literal per tenant written by hand
    val tenants = List("alpha", "beta", "gamma", "delta")
    val handles = tenants.map(Instances.handle)
    val rows = handles.zip(tenants).toMap

    val p: List[String] ! Instances.Of[Store] =
      handles.foldRight(pure[Instances.Of[Store], List[String]](Nil)) { (h, rest) =>
        Instances.at[Store](h)(Store.Get()).flatMap(x => rest.map(x :: _))
      }

    val h = Instances.handler[Store](i => new:
      def handle[A](e: Store[A]): A = e match { case Store.Get() => rows(i) })

    assertEquals(p.runWith(using h), tenants)
  }

  test("route sends an ALREADY WRITTEN program to one instance") {
    // written against a plain Store by someone who never heard of
    // instances — and run twice, at two of them
    def read: String ! Store = effect[Store, String](Store.Get())

    val one = Instances.handle("one")
    val two = Instances.handle("two")

    val p: (String, String) ! Instances.Of[Store] =
      for
        a <- Instances.route[Store](one)[String, okay.Pure](read)
        b <- Instances.route[Store](two)[String, okay.Pure](read)
      yield (a, b)

    val rows = Map(one -> "первый", two -> "второй")
    val h = Instances.handler[Store](i => new:
      def handle[A](e: Store[A]): A = e match { case Store.Get() => rows(i) })

    assertEquals(p.runWith(using h), ("первый", "второй"))
  }

  test("only: one instance goes to the effect's OWN runner, the rest stay") {
    // two counters, each run by State.run at its own initial state —
    // the case a single comonadic handler cannot serve, because the
    // state has to be threaded
    val small = Instances.handle("small")
    val big = Instances.handle("big")

    val p: (Int, Int) ! Instances.Of[State % Int] =
      for
        a <- Instances.at[State % Int](small)(State.Get())
        b <- Instances.at[State % Int](big)(State.Get())
      yield (a, b)

    // strip "small" and run it at 1 — `State.handle` keeps the rest of
    // the row, which is exactly the shape `only` hands it
    val afterSmall: (Int, (Int, Int)) ! (Instances.Of[State % Int] + okay.Pure) =
      State.handle(1)(Instances.only[State % Int](small)(p.plus[okay.Pure]))

    // now strip "big" and run it at 10
    val done: (Int, (Int, (Int, Int))) ! (Instances.Of[State % Int] + okay.Pure) =
      State.handle(10)(Instances.only[State % Int](big)(afterSmall))

    // nothing is left wrapped: the residual member has no operations
    val (bigEnd, (smallEnd, answer)) =
      !.run(Instances.exhausted[State % Int, (Int, (Int, (Int, Int))), okay.Pure](done))
    assertEquals(answer, (1, 10))
    assertEquals(smallEnd, 1)
    assertEquals(bigEnd, 10)
  }

  test("two SIGNATURES under instances is an ordinary row") {
    // the test asks the signature first, so these are two members
    val s = Instances.handle("s")
    val w = Instances.handle("w")

    val p: (String, Int) ! (Instances.Of[Store] + Instances.Of[Reader % Int]) =
      for
        a <- Instances.at[Store](s)(Store.Get()).plus[Instances.Of[Reader % Int]]
        b <- Instances.at[Reader % Int](w)(Reader.Ask())
               .liftAt[Instances.Of[Store] + Instances.Of[Reader % Int]]
      yield (a, b)

    val hs = Instances.handler[Store](_ => new:
      def handle[A](e: Store[A]): A = e match { case Store.Get() => "ada" })
    val hr = Instances.handler[Reader % Int](_ => new:
      def handle[A](e: Reader[Int, A]): A = e match { case Reader.Ask() => 7 })

    // an Effect IS a TypeableK, so the union's split is the row's own
    assertEquals(
      p.runWith(using Handler.union[Instances.Of[Store], Instances.Of[Reader % Int]](
        using summon[okay.Effect[Instances.Of[Store]]], hs, hr)),
      ("ada", 7))
  }
