package okay

import okay.Bisim.{Answers, Verdict}
import okay.Row.{at, plus}
import okay.Writer.byValue.given

/**
 * row-coercion-coherence-law (specs/widen-split.md, Coherence): a
 * program reaches a wider row by a COERCION (`!.widen`, which is
 * `Row.into`) or by a WALK (`!.normalize`), and what it means must not
 * depend on which. That is coherence of effect subtyping (Biernacki &
 * Polesiuk, LMCS 2018). A member of a row is found by a runtime test on
 * the operation's VALUE, so coherence is exactly "neither road changes
 * an operation". It is checked as a tree equivalence over sampled
 * answers (`Bisim.check`), not as one run's result.
 */
class TestRowCoherence extends munit.FunSuite:

  /** the member widening adds: never performed by any program here */
  type Added = Reader % Boolean
  val added: Answers[Added] = Answers.reader(true)

  /** a `Tag`'s operations answered as the signature under it answers */
  def tagged[K, F[+_]](a: Answers[F]): Answers[Tag.Of[K, F]] = new Answers[Tag.Of[K, F]]:
    def apply[X](op: Tag[K, F, X]): List[X] = a(op.op)

  /** the law: the coercion and the walk are the same tree, and some
   * sampled path of it ended */
  def coherent[A, F[+_]](name: String, p: A ! F, answers: Answers[F + Added], depth: Int = 32): Unit =
    given Answers[F + Added] = answers
    Bisim.check(!.widen[A, F, Added](p), !.normalize[A, F, Added](p), depth) match
      case Verdict.Same(paths, _) => assert(paths > 0, s"$name: no sampled path ended")
      case d => fail(s"$name: the widened and the normalised program differ: $d")

  // ---- one signature at a time --------------------------------------------

  val counter: Int ! State % Int =
    for
      s <- State.get[Int]
      _ <- State.set(s + 1)
      t <- State.get[Int]
      r <- if t > 1 then State.set(0).map(_ => -1) else pure[State % Int, Int](t)
    yield r

  val greeting: String ! Reader % String =
    Reader.ask[String].flatMap(n => Reader.ask[String].map(m => s"$n/$m"))

  val told: Unit ! Writer % Int =
    (1 to 5).foldLeft(pure[Writer % Int, Unit](()))((p, i) => p.flatMap(_ => Writer.tell(i)))

  val stops: Int ! Writer % Int + Stop =
    Writer.tell(1).at[Writer % Int + Stop].flatMap(_ => effect[Writer % Int + Stop, Unit](Stop.Now)).map(_ => 2)

  test("each core signature: widen and normalize are one tree") {
    coherent("State", counter, Answers.state(0, 1, 2) + added)
    coherent("Reader", greeting, Answers.reader("a", "b") + added)
    coherent("Writer", told, Answers.writer[Int] + added)
    coherent("Writer + Stop", stops, Answers.writer[Int] + (Answers.stop + added))
  }

  test("every tree shape: a deferred head, a deferred bind, a deep left fold") {
    val delayed: Int ! State % Int = Free.delay(() => counter)
    val bindDelay: Int ! State % Int = Free.delay(() => State.get[Int]).flatMap(s => State.set(s * 2).map(_ => s))
    val deep: Int ! State % Int = (1 to 200).foldLeft(pure[State % Int, Int](0))((p, _) => p.flatMap(_ => counter))
    coherent("delay", delayed, Answers.state(0, 1, 2) + added)
    coherent("bind-delay", bindDelay, Answers.state(0, 1, 2) + added)
    // ~800 operations on one path: one answer per operation, and the depth to reach the end
    coherent("deep", deep, Answers.state(0) + added, depth = 1000)
  }

  // ---- mixed rows -----------------------------------------------------------

  type Mixed = State % Int + Writer % String
  val mixed: Int ! Mixed =
    for
      s <- State.get[Int].at[Mixed]
      _ <- Writer.tell(s"saw $s").at[Mixed]
      _ <- State.set(s + 10).at[Mixed]
    yield s

  type Small = Tag.Of["small", State % Int]
  type Big = Tag.Of["big", State % Int]
  def bump(by: Int): Int ! State % Int = State.get[Int].flatMap(n => State.set(n + by).map(_ => n))
  val keyed: (Int, Int) ! Small + Big =
    for
      a <- Tag.tag["small", State % Int](bump(1)).plus[Big]
      b <- Tag.tag["big", State % Int][Int, okay.Pure](bump(10)).at[Small + Big]
    yield (a, b)
  // `+` takes its test from its LEFT member, so a row of answers nests
  // to the right: A + (B + added)
  val keyedAnswers: Answers[Small + (Big + Added)] =
    tagged["small", State % Int](Answers.state(0, 1)) + (tagged["big", State % Int](Answers.state(100, 200)) + added)

  type Pair = Writer % Int + Writer % String
  val pair: Unit ! Pair =
    for
      _ <- Writer.tell(1).at[Pair]
      _ <- Writer.tell("one").at[Pair]
      _ <- Writer.tell(2).at[Pair]
    yield ()

  test("mixed rows: State + Writer, two Tag keys over one signature, a Writer.byValue pair") {
    coherent("State + Writer", mixed, Answers.state(0, 7) + (Answers.writer[String] + added))
    coherent("two keys", keyed, keyedAnswers)
    coherent("byValue pair", pair, Answers.writer[Int] + (Answers.writer[String] + added))
  }

  // ---- and it runs as it ran before widening --------------------------------

  test("the widened program runs as the program did: State, two keys, the byValue pair") {
    def wide[A, F[+_]](p: A ! F): A ! Added + F = !.widen[A, F, Added](p)
    assertEquals(State.run(1)(Reader.run(true)(wide(counter))), State.run(1)(counter))
    def keys(p: (Int, Int) ! Small + Big) =
      val afterSmall: (Int, (Int, Int)) ! Big = State.handle[Int](1)(Tag.untag["small", State % Int](p))
      !.run(State.handle[Int](100)(Tag.untag["big", State % Int](afterSmall)))
    assertEquals(keys(Reader.run(true)(wide(keyed))), keys(keyed))
    def both(p: Unit ! Pair) = !.run(Writer.run(Writer.run(p)))
    assertEquals(both(Reader.run(true)(wide(pair))), both(pair))
  }
