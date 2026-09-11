package okay

import okay.RowLift.{at, plus}

/** Several instances of ONE signature in one row. */
class TestTag extends munit.FunSuite {

  type Small = Tag.Of["small", State % Int]
  type Big   = Tag.Of["big", State % Int]
  type Both  = Small + Big

  /** an ordinary function, written against a plain State and knowing
   * nothing about keys */
  def bump(by: Int): Int ! (State % Int) =
    for
      n <- State.get[Int]
      _ <- State.set(n + by)
    yield n

  test("one function, two states, one row — and it was not written for it") {
    val p: (Int, Int) ! Both =
      for
        a <- Tag.tag["small", State % Int](bump(1)).plus[Big]
        // the second clause, written: `G` infers from the program,
        // and a program of State alone gives back State rather than
        // Pure — `.at[Both]` needs the Pure row (generalized-method-syntax)
        b <- Tag.tag["big", State % Int][Int, okay.Pure](bump(10)).at[Both]
      yield (a, b)

    // handling is the effect's OWN: untag one key, run State, repeat
    val afterSmall: (Int, (Int, Int)) ! Big =
      State.handle[Int](1)(Tag.untag["small", State % Int](p))
    val (big, (small, answer)) =
      !.run(State.handle[Int](100)(
        Tag.untag["big", State % Int](afterSmall)))
    assertEquals(answer, (1, 100))
    assertEquals(small, 2)     // 1 + 1
    assertEquals(big, 110)     // 100 + 10
  }

  test("a key tells apart two instances of a signature that carries nothing") {
    type A = Tag.Of["a", Reader % Int]
    type B = Tag.Of["b", Reader % Int]
    val p: (Int, Int) ! (A + B) =
      for
        x <- Tag.one["a", Reader % Int](Reader.Ask()).plus[B]
        y <- Tag.one["b", Reader % Int](Reader.Ask()).at[A + B]
      yield (x, y)
    val inner = Reader.run[Int, (Int, Int), A](7)(
      Tag.untag["b", Reader % Int](p.at[B + A]))
    val out = !.run(Reader.run[Int, (Int, Int), okay.Pure](1)(
      Tag.untag["a", Reader % Int](inner)))
    assertEquals(out, (1, 7))
  }

  test("a tagged effect can also be handled by its own comonadic handler") {
    enum Beep[+A] derives okay.Effect:
      case Boop() extends Beep[Int]
    val h: Handler[Beep] = new:
      def handle[A](e: Beep[A]): A = e match { case Beep.Boop() => 42 }
    val p: Int ! Tag.Of["x", Beep] = Tag.one["x", Beep](Beep.Boop())
    assertEquals(p.runWith(using Tag.handler["x", Beep](h)), 42)
  }

  /**
   * THE KEY'S OWN LIMIT, pinned the way `TestRowIdentity` pins the
   * bare row's (tag-key-collision, 2026-09-11).
   *
   * `Tag`'s test is BY KEY — "everything else about F is already
   * erased" — which is what lets one signature appear twice. Read the
   * other way it is a requirement: two members that share a key have
   * nothing left to compare, and the row is back where it started.
   * Measured: this misroutes into the same ClassCastException the key
   * was introduced to prevent.
   *
   * So the rule is not "use a key" but "use a DISTINCT key per member",
   * and nothing checks it today — docs/many-instances.md says so, and
   * `tag-distinct-keys` in BACKLOG.md is the compile-time check.
   */
  test("two members under ONE key are not told apart — keys must be distinct") {
    type A = Tag.Of["same", Reader % Int]
    type B = Tag.Of["same", Reader % String]
    val p: (Int, String) ! (A + B) =
      for
        x <- Tag.one["same", Reader % Int](Reader.Ask()).plus[B]
        y <- Tag.one["same", Reader % String](Reader.Ask()).at[A + B]
      yield (x, y)

    intercept[ClassCastException] {
      val inner = Reader.run[String, (Int, String), A](
        "ada")(Tag.untag["same", Reader % String](p.at[B + A]))
      !.run(Reader.run[Int, (Int, String), okay.Pure](
        7)(Tag.untag["same", Reader % Int](inner)))
    }
  }

  /**
   * THE HALF THE SIGNATURE TEST BUYS (tag-test-the-signature-too,
   * 2026-09-11). `Tag`'s test used to be the key ALONE, so two
   * members sharing a key collided however different their effects
   * were. It now asks the key AND the signature — which is what
   * `Instances` does — so a shared key across DIFFERENT signatures
   * routes correctly.
   *
   * The other half is unreachable by any runtime test and the suite
   * above still proves it: same signature, same key, still a
   * ClassCastException, because there is nothing left to compare.
   */
  test("one key, two SIGNATURES: the signature test tells them apart") {
    enum Beep[+A] derives okay.Effect:
      case Boop() extends Beep[Int]
    enum Buzz[+A] derives okay.Effect:
      case Bzz() extends Buzz[String]

    type A = Tag.Of["same", Beep]
    type B = Tag.Of["same", Buzz]

    val p: (Int, String) ! (A + B) =
      for
        x <- Tag.one["same", Beep](Beep.Boop()).plus[B]
        y <- Tag.one["same", Buzz](Buzz.Bzz()).at[A + B]
      yield (x, y)

    val hb: Handler[Beep] = new:
      def handle[X](e: Beep[X]): X = e match { case Beep.Boop() => 42 }
    val hz: Handler[Buzz] = new:
      def handle[X](e: Buzz[X]): X = e match { case Buzz.Bzz() => "ada" }

    assertEquals(
      p.runWith(using Handler.union[A, B](
        using summon[okay.Effect[A]], Tag.handler["same", Beep](hb),
        Tag.handler["same", Buzz](hz))),
      (42, "ada"))
  }
}
