package okay

import Bisim.{Answers, Verdict}
import okay.Row.at
import okay.Writer.byValue.given

/**
 * row-parametricity-forwarding-law (specs/row-parametricity-forwarding-law.md):
 * a handler polymorphic in the rest of the row cannot touch an operation
 * it does not own (Biernacki, Piróg, Polesiuk & Sieczkowski, POPL 2018),
 * so its forwarding arm is FORCED: perform the operation once, in place,
 * hand the answer to the program's own continuation. The law compares a
 * library handler's residual tree with `reference`, the smallest
 * interpreter the theorem allows, under a spy row of two signatures the
 * handler has no business with.
 */
class TestRowForwarding extends munit.FunSuite:

  /** the unrelated signatures every handler here must forward */
  type Spy = Writer % String + Reader % Boolean
  given Answers[Spy] = Answers.writer[String] + Answers.reader(true, false)

  /**
   * THE REFERENCE: `F` answered in place by a pure `step`, every other
   * operation re-emitted with the rest of the walk as its continuation.
   * The recursion lives in the `flatMap` closures the runner forces, as
   * `State.handle`'s does (specs/stack-safety.md: trampolined).
   */
  def reference[F[+_]: TypeableK, S, A, G[+_]](s0: S)(step: [X] => (S, F[X]) => (S, X))(p: A ! F + G): (S, A) ! G =
    def go(s: S, x: A ! F + G): (S, A) ! G =
      def on[X](e: (F + G)[X], k: X => A ! F + G): (S, A) ! G =
        split[F, G](e)(f => { val (s1, v) = step(s, f); okay.pure[G, X](v).flatMap(v => go(s1, k(v))) })(
          g => effect[G, X](g).flatMap(v => go(s, k(v))))
      (x.resume: @unchecked) match
        case Free.Return(a) => okay.pure((s, a))
        case Free.Inject(e) => on[A](e, okay.pure)
        case Free.Bind(Free.Inject(e), k) => on(e, k)
    go(s0, p)

  // ---- the pure steps of the three tail-resumptive signatures ----------------

  def stateStep[S]: [X] => (S, State[S, X]) => (S, X) = [X] => (s: S, e: State[S, X]) => stateOn(s, e)
  private def stateOn[S, X](s: S, e: State[S, X]): (S, X) = e match
    case State.Get() => (s, s)
    case State.Set(s1) => (s1, s1)

  def readerStep[R]: [X] => (R, Reader[R, X]) => (R, X) = [X] => (r: R, e: Reader[R, X]) => readerOn(r, e)
  private def readerOn[R, X](r: R, e: Reader[R, X]): (R, X) = e match
    case Reader.Ask() => (r, r)

  def writerStep[W]: [X] => (Vector[W], Writer[W, X]) => (Vector[W], X) =
    [X] => (ws: Vector[W], e: Writer[W, X]) => writerOn(ws, e)
  private def writerOn[W, X](ws: Vector[W], e: Writer[W, X]): (Vector[W], X) = e match
    case Writer.Say(w) => (ws :+ w, ())

  // ---- the programs: the handled signature interleaved with both spies ------

  type RowS = State % Int + Spy
  val stateProg: Int ! RowS =
    for
      s <- State.get[Int].at[RowS]
      _ <- Writer.tell(s"saw $s").at[RowS]
      b <- Reader.ask[Boolean].at[RowS]
      _ <- State.set(if b then s + 1 else s + 10).at[RowS]
      t <- State.get[Int].at[RowS]
      _ <- Writer.tell(s"now $t").at[RowS]
    yield t
  def stateLoop(n: Int): Int ! RowS =
    (1 to n).foldLeft(pure[RowS, Int](0))((p, _) => p.flatMap(_ => stateProg))

  /** Reader's spy cannot hold a second Reader: `Distinct` refuses two of
   * one class in a row (the guard at work), so its spy is State */
  type SpyR = Writer % String + State % Boolean
  given Answers[SpyR] = Answers.writer[String] + Answers.state(true, false)
  type RowR = Reader % String + SpyR
  val readerProg: String ! RowR =
    for
      n <- Reader.ask[String].at[RowR]
      _ <- Writer.tell(s"env $n").at[RowR]
      b <- State.get[Boolean].at[RowR]
      _ <- State.set(!b).at[RowR]
      m <- Reader.ask[String].at[RowR]
    yield if b then n + m else m

  type RowW = Writer % Int + Spy
  val writerProg: Int ! RowW =
    for
      _ <- Writer.tell(1).at[RowW]
      b <- Reader.ask[Boolean].at[RowW]
      _ <- Writer.tell(s"spy $b").at[RowW]
      _ <- Writer.tell(if b then 2 else 3).at[RowW]
    yield if b then 20 else 30

  def same[A, G[+_]](name: String, left: A ! G, right: A ! G, depth: Int = 32)(using Answers[G]): Unit =
    Bisim.check(left, right, depth) match
      case Verdict.Same(paths, _) => assert(paths > 0, s"$name: no sampled path ended")
      case d => fail(s"$name: the handler and the reference differ: $d")

  // ---- the law ----------------------------------------------------------------

  test("State.handle forwards the spy row unchanged: Same as the reference, two paths (Reader's samples)") {
    same("State", State.handle[Int](0)[Int, Spy](stateProg), reference[State % Int, Int, Int, Spy](0)(stateStep[Int])(stateProg))
    // ~1200 operations on one path
    same("State loop", State.handle[Int](0)[Int, Spy](stateLoop(200)), reference[State % Int, Int, Int, Spy](0)(stateStep[Int])(stateLoop(200)), depth = 2000)
  }

  test("Reader.run forwards the spy row unchanged") {
    same("Reader", Reader.run[String, String, SpyR]("e")(readerProg), reference[Reader % String, String, String, SpyR]("e")(readerStep[String])(readerProg).map(_._2))
  }

  test("Writer.run and Writer.collect forward the spy row unchanged") {
    val ref = reference[Writer % Int, Vector[Int], Int, Spy](Vector.empty[Int])(writerStep[Int])(writerProg)
    same("Writer.collect", Writer.collect[Int, Int, Spy](writerProg), ref)
    same("Writer.run", Writer.run[Int, Int, Spy](writerProg).map((ws, a) => (ws.toVector, a)), ref)
  }

  test("FOUND BY THE LAW: Writer.map and Writer.expand at the identity leave the two-Writer row as it was") {
    // before this lane, map/expand/uncons/collect summoned Writer's class
    // test inside the companion, and a byValue row's other Writer was
    // taken as their own (the collect case is the test above)
    // map/expand take ONE signature as the rest (G: TypeableK), so the spy is the other Writer alone
    type WW = Writer % Int + Writer % String
    given Answers[WW] = Answers.writer[Int] + Answers.writer[String]
    val both: Int ! WW =
      Writer.tell(1).at[WW].flatMap(_ => Writer.tell("spy").at[WW]).flatMap(_ => Writer.tell(2).at[WW]).map(_ => 3)
    same("map id", Writer.map[Int, Int, Int, Writer % String](both)(identity), both)
    same("expand id", Writer.expand[Int, Int, Int, Writer % String](both)(v => IndexedSeq(v)), both)
  }

  // ---- the two ways a forwarding arm can be wrong, both seen -------------------

  /** the reference for State, with its forwarding arm replaced: `arm`
   * gets the foreign operation and "resume with this answer" */
  def mutant[A](arm: [X] => (Spy[X], X => (Int, A) ! Spy) => (Int, A) ! Spy)(p: A ! RowS): (Int, A) ! Spy =
    def go(s: Int, x: A ! RowS): (Int, A) ! Spy =
      def on[X](e: RowS[X], k: X => A ! RowS): (Int, A) ! Spy =
        split[State % Int, Spy](e)(f => { val (s1, v) = stateOn(s, f); okay.pure[Spy, X](v).flatMap(v => go(s1, k(v))) })(
          g => arm(g, v => go(s, k(v))))
      (x.resume: @unchecked) match
        case Free.Return(a) => okay.pure((s, a))
        case Free.Inject(e) => on[A](e, okay.pure)
        case Free.Bind(Free.Inject(e), k) => on(e, k)
    go(0, p)

  /** a Say answered `()` and never performed; anything else forwarded */
  private def swallowOn[X](g: Spy[X], resume: X => (Int, Int) ! Spy): (Int, Int) ! Spy =
    split[Writer % String, Reader % Boolean](g)(w => sayOn(w, resume))(r => effect[Spy, X](r).flatMap(resume))
  private def sayOn[X](w: Writer[String, X], resume: X => (Int, Int) ! Spy): (Int, Int) ! Spy = w match
    case Writer.Say(_) => resume(())

  test("MUTANT: forwarding a foreign operation TWICE fails the law, and the path names it") {
    val twice = mutant[Int]([X] => (g: Spy[X], resume: X => (Int, Int) ! Spy) =>
      effect[Spy, X](g).flatMap(_ => effect[Spy, X](g)).flatMap(resume))(stateProg)
    Bisim.check(State.handle[Int](0)[Int, Spy](stateProg), twice) match
      case Verdict.Differ(path, l, r) => assert((path.mkString + l + r).contains("Say"), s"$path / $l / $r")
      case v => fail(s"the duplicated operation was not seen: $v")
  }

  test("MUTANT: swallowing a foreign Say (answered without being performed) fails the law") {
    val swallow = mutant[Int]([X] => (g: Spy[X], resume: X => (Int, Int) ! Spy) => swallowOn(g, resume))(stateProg)
    Bisim.check(State.handle[Int](0)[Int, Spy](stateProg), swallow) match
      case Verdict.Differ(_, _, _) => ()
      case v => fail(s"the dropped Say was not seen: $v")
  }
