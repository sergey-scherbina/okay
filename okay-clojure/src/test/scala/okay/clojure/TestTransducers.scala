package okay.clojure

import okay.{!, %, Stage, Writer, pure, through}
import clojure.lang.{IFn, PersistentVector}
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*

/**
 * A `Stage` IS a Clojure transducer (specs/clojure.md), as LAWS both
 * ways against Clojure's own `(into [] xf coll)`; then what a finite law
 * cannot show — early termination from either side, `comp` in both
 * orders, the lazy `sequence` road, one-shot pipelines, refusals.
 * Every test that could loop is bounded, so a broken bridge FAILS it
 * (a lesson of java-gatherers, whose first short-circuit test hung).
 */
class TestTransducers extends munit.FunSuite {

  // ------------------------------------------------------------ harness

  def clj(src: String): AnyRef = Clj.eval(src).fold(e => fail(e), identity)
  def core(name: String): IFn = Clj.fn("clojure.core", name).fold(e => fail(e), identity)

  /** a Clojure value that must be a function (a transducer, a comp) */
  def fn(x: AnyRef): IFn = x match
    case f: IFn => f
    case other => fail(s"expected a Clojure function, got $other")
  def xform(src: String): IFn = fn(clj(src))

  /** Clojure's `(into [] xf coll)`, as a Scala list */
  def into(xf: IFn, coll: AnyRef): List[Any] =
    core("into").invoke(PersistentVector.EMPTY, xf, coll) match
      case v: java.util.List[?] => v.asScala.toList
      case other => fail(s"into answered $other")

  def vec(xs: Seq[Long]): AnyRef = PersistentVector.create(xs.map(Long.box).asJava)

  def emit[I](xs: Seq[I]): Unit ! Writer % I =
    xs.foldRight(pure[Writer % I, Unit](()))((x, p) => Writer.tell(x).flatMap(_ => p))

  def own[I, O, A](src: Unit ! Writer % I, s: Stage[I, O, A]): List[O] =
    !.run(Writer.run(through(src)(s)))._1.toList

  // ------------------------------------------------------------- stages

  val id: Stage[Long, Long, Unit] = Stage.id[Long]

  val chunks: Stage[Long, List[Long], List[Long]] =
    Stage.transduce[Long, List[Long], List[Long]](Nil)(
      (buf, i) =>
        val b = i :: buf
        if b.sizeIs == 3 then Stage.tell[Long, List[Long]](b.reverse).map(_ => Nil) else pure(b),
      buf => if buf.isEmpty then pure(buf) else Stage.tell[Long, List[Long]](buf.reverse).map(_ => Nil))

  val runningSum: Stage[Long, Long, Long] =
    Stage.mapAccumulate[Long, Long, Long](0L)((s, i) => (s + i, s + i))

  val firstThree: Stage[Long, Long, Unit] =
    Stage.transduceUntil[Long, Long, Int, Unit](0)(
      (n, i) => Stage.tell[Long, Long](i).map(_ => if n + 1 >= 3 then Right(()) else Left(n + 1)),
      _ => ())

  val framed: Stage[Long, String, Int] =
    Stage.tell[Long, String]("head").flatMap(_ =>
      Stage.transduce[Long, String, Int](0)(
        (n, i) => Stage.tell[Long, String](i.toString).map(_ => n + 1),
        n => Stage.tell[Long, String](s"n=$n").map(_ => n)))

  val inputs: List[List[Long]] = List(Nil, List(7L), (1L to 10L).toList, (1L to 100L).toList)

  // --------------------------------------------------- stage -> Clojure

  test("law: (into [] (Transducers.of s) coll) tells what okay's own run tells") {
    for xs <- inputs do
      assertEquals(into(Transducers.of(id), vec(xs)), own(emit(xs), id), s"id over $xs")
      assertEquals(into(Transducers.of(chunks), vec(xs)), own(emit(xs), chunks), s"chunks over $xs")
      assertEquals(into(Transducers.of(runningSum), vec(xs)), own(emit(xs), runningSum), s"runningSum over $xs")
      assertEquals(into(Transducers.of(firstThree), vec(xs)), own(emit(xs), firstThree), s"firstThree over $xs")
      assertEquals(into(Transducers.of(framed), vec(xs)), own(emit(xs), framed), s"framed over $xs")
  }

  test("comp in both orders: a stage composes with Clojure's own transducers") {
    val xs = (1L to 20L).toList
    val inc = core("map").invoke(core("inc"))
    val after = core("comp").invoke(Transducers.of(runningSum), inc)     // stage, then inc
    val before = core("comp").invoke(inc, Transducers.of(runningSum))    // inc, then stage
    assertEquals(into(fn(after), vec(xs)), own(emit(xs), runningSum).map(_ + 1))
    assertEquals(into(fn(before), vec(xs)), own(emit(xs.map(_ + 1)), runningSum))
  }

  test("a stage that answers stops the process: (range) is pulled no further") {
    // (range 1000), not (range): a bridge that fails to stop must FAIL
    // this (1000 pulled), not hang the suite
    val pulled = AtomicInteger()
    val counting = core("map").invoke(new clojure.lang.AFn:
      override def invoke(x: AnyRef): AnyRef = { pulled.incrementAndGet(): Unit; x })
    val xf = fn(core("comp").invoke(counting, Transducers.of(firstThree)))
    assertEquals(into(xf, clj("(range 1000)")), List(0L, 1L, 2L))
    assertEquals(pulled.get, 3)
  }

  /** each input told `n` times, counting the tells actually made */
  def copies(n: Int, told: AtomicInteger): Stage[Long, Long, Unit] =
    Stage.transduce[Long, Long, Unit](())(
      (_, i) => (1 to n).foldLeft(pure(()): Stage[Long, Long, Unit])((p, _) =>
        p.flatMap(_ => { told.incrementAndGet(): Unit; Stage.tell[Long, Long](i) })),
      pure)

  test("a downstream (take 2) stops the stage mid-element: the rest of its tells are never made") {
    val told = AtomicInteger()
    val xf = core("comp").invoke(Transducers.of(copies(1000, told)), core("take").invoke(Long.box(2L)))
    assertEquals(into(fn(xf), vec(List(5L, 6L))), List(5L, 5L))
    assert(told.get <= 3, s"${told.get} tells made for a downstream that took 2")
  }

  test("sequence (lazy, element at a time) and transduce agree with into") {
    val xs = vec((1L to 50L).toList)
    val xf = Transducers.of(chunks)
    val lazily = core("sequence").invoke(xf, xs) match
      case s: java.util.List[?] => s.asScala.toList
      case other => fail(s"sequence answered $other")
    assertEquals(lazily, into(xf, xs))
    val summed = core("transduce").invoke(Transducers.of(runningSum), core("+"), xs)
    assertEquals(summed, Long.box(own(emit((1L to 50L).toList), runningSum).sum))
  }

  test("one transducer value, two processes: no state shared") {
    val xf = Transducers.of(runningSum)
    assertEquals(into(xf, vec((1L to 10L).toList)), into(xf, vec((1L to 10L).toList)))
  }

  test("an element of the wrong type is refused by name") {
    val e = intercept[IllegalArgumentException](into(Transducers.of(id), clj("""["a"]""")))
    assert(e.getMessage.contains("java.lang.String"), e.getMessage)
  }

  // --------------------------------------------------- Clojure -> stage

  val clojureSide: List[String] = List(
    "(map inc)", "(filter odd?)", "(partition-all 3)", "(take 3)", "(dedupe)",
    "(comp (map inc) (filter even?) (partition-all 2))")

  test("law: Clojure's own transducers through Transducers.stage equal (into [] xf coll)") {
    for src <- clojureSide; xs <- inputs.map(_ ++ List(1L, 1L, 2L)) do
      val xf = xform(src)
      assertEquals(own(emit(xs), Transducers.stage[Long, AnyRef](xf)), into(xf, vec(xs)), s"$src over $xs")
  }

  /** 0 until n told, counting what the stage actually pulled */
  def nat(n: Long, pulled: AtomicInteger): Unit ! Writer % Long =
    def go(i: Long): Unit ! Writer % Long =
      if i >= n then pure(())
      else Writer.tell(i).flatMap(_ => { pulled.incrementAndGet(): Unit; go(i + 1) })
    go(0)

  test("a reduced from (take 3) stops the stage AWAITING: the producer is pulled no further") {
    val pulled = AtomicInteger()
    val out = own(nat(1000, pulled), Transducers.stage[Long, java.lang.Long](xform("(take 3)")))
    assertEquals(out.map(_.longValue), List(0L, 1L, 2L))
    assert(pulled.get <= 3, s"the producer was pulled ${pulled.get} times past a reduced")
  }

  test("the completion arity flushes after a reduced: (comp (take 5) (partition-all 3))") {
    val xf = xform("(comp (take 5) (partition-all 3))")
    val xs = (1L to 100L).toList
    assertEquals(own(emit(xs), Transducers.stage[Long, AnyRef](xf)), into(xf, vec(xs)))
  }

  test("round trip: Transducers.stage(Transducers.of(s)) runs as s") {
    val xs = (1L to 30L).toList
    assertEquals(own(emit(xs), Transducers.stage[Long, List[Long]](Transducers.of(chunks))), own(emit(xs), chunks))
  }

  test("a built pipeline over a Clojure transducer runs twice; a continuation resumed after its run finished is REFUSED by name") {
    // a STRICT transducer, which throws its own error if stepped after
    // completion: the refusal must come BEFORE the spent state is
    // touched (the JDK's windowFixed NPE'd there, java-gatherers), so
    // the message has to be ours, not this one
    val xf = xform("""
      (fn [rf]
        (let [done (volatile! false) buf (java.util.ArrayList.)]
          (fn ([] (rf))
              ([acc] (vreset! done true)
                     (let [acc (if (.isEmpty buf) acc (unreduced (rf acc (vec buf))))] (rf acc)))
              ([acc x] (when @done (throw (IllegalStateException. "stepped after completion")))
                       (.add buf x)
                       (if (= 2 (.size buf)) (let [v (vec buf)] (.clear buf) (rf acc v)) acc)))))""")
    val built = through(emit(List(1L, 2L, 3L, 4L, 5L)))(Transducers.stage[Long, AnyRef](xf))
    // a built program is a VALUE (through defers its drive,
    // windows-stage-rerun-loses-pane): each run applies `xf` afresh
    assertEquals(!.run(Writer.run(built))._1.size, 3)
    assertEquals(!.run(Writer.run(built))._1.size, 3)
    // what cannot be replayed is a continuation from INSIDE a run —
    // the rest after the first batch holds that run's `volatile!`s.
    // Resumed again after the run finished, it is refused by name
    // (five elements so that an await is left after the batch `rest`
    // is driven to when it is made: [3 4] told, 5 still to come)
    Writer.uncons(built) match
      case Right((_, rest)) =>
        assertEquals(!.run(Writer.run(rest))._1.size, 2)
        val again = intercept[IllegalStateException](!.run(Writer.run(rest)))
        assert(again.getMessage.contains("already ran"), again.getMessage)
      case Left(_) => fail("the pipeline ended without a batch")
  }

  // ---------------------------------------------------------------- Clj

  test("Clj.eval defines in `user` (or the namespace named), never inside clojure.core") {
    Clj.eval("(def okay-eval-probe 41)").fold(e => fail(e), _ => ())
    assertEquals(Clj.fn("clojure.core", "okay-eval-probe"), Left("no bound var clojure.core/okay-eval-probe"),
      "a def evaluated from okay must not land in clojure.core")
    assertEquals(Clj.value("user", "okay-eval-probe"), Right(Long.box(41L)))
    Clj.eval("(def here 1)", ns = "okay.test.scratch").fold(e => fail(e), _ => ())
    assertEquals(Clj.value("okay.test.scratch", "here"), Right(Long.box(1L)))
  }

  test("Clj refuses by name: an unbound var, a namespace that does not load, bad source") {
    assert(Clj.fn("clojure.core", "no-such-fn").left.exists(_.contains("clojure.core/no-such-fn")))
    assert(Clj.fn("no.such.namespace", "f").left.exists(_.contains("no.such.namespace")))
    assert(Clj.eval("(+ 1").isLeft)
    assertEquals(Clj.fn("clojure.string", "upper-case").map(_.invoke("okay")), Right("OKAY"))
  }
}
