package okay.clojure

import okay.{!, %, +, Chunks, Reader, Stage, State, Writer, pure, through}
import clojure.lang.{IFn, PersistentVector}
import scala.jdk.CollectionConverters.*

/**
 * The examples docs/modules/okay-clojure.md and docs/guide.md §5 print,
 * VERBATIM (the-record-outlives-the-truth): what the page shows, with
 * the answer the page claims.
 */
class TestDocExamplesClojure extends munit.FunSuite:

  def fn(x: AnyRef): IFn = x match
    case f: IFn => f
    case other => fail(s"expected a Clojure function, got $other")

  def lines(xs: String*): Unit ! Writer % String =
    xs.foldRight(pure[Writer % String, Unit](()))((x, p) => Writer.tell(x).flatMap(_ => p))

  def told[O](p: Unit ! Writer % O): Seq[O] = !.run(Writer.run(p))._1

  test("okay-clojure.md: calling Clojure, refusals by name") {
    val upper = Clj.fn("clojure.string", "upper-case")          // Right(#'clojure.string/upper-case)
    assertEquals(upper.map(_.invoke("okay")), Right("OKAY"))
    assertEquals(Clj.fn("clojure.core", "no-such-fn"), Left("no bound var clojure.core/no-such-fn"))
    assertEquals(Clj.eval("(reduce + (range 10))"), Right(java.lang.Long.valueOf(45L)))
  }

  test("okay-clojure.md / guide §5: an okay stage as a Clojure transducer, composed with comp") {
    // an okay stage: a running sum, one output per input
    val runningSum: Stage[Long, Long, Long] =
      Stage.mapAccumulate[Long, Long, Long](0L)((s, i) => (s + i, s + i))

    val into = Clj.fn("clojure.core", "into").toOption.get
    val comp = Clj.fn("clojure.core", "comp").toOption.get
    val odd = Clj.eval("(filter odd?)").toOption.get

    val xf = comp.invoke(odd, Transducers.of(runningSum))        // (comp (filter odd?) <stage>)
    val out = into.invoke(PersistentVector.EMPTY, xf, Clj.eval("(range 10)").toOption.get)
    // [1 4 9 16 25]
    assertEquals(out, PersistentVector.create(List(1L, 4L, 9L, 16L, 25L).map(Long.box).asJava))
  }

  test("okay-clojure.md / guide §5: a Clojure transducer as an okay stage") {
    val pairs = Transducers.stage[String, AnyRef](fn(Clj.eval("(comp (dedupe) (partition-all 2))").toOption.get))
    val windows = through(lines("a", "a", "b", "c", "c", "d"))(pairs)
    // Writer.run(windows) — (Seq([a b], [c d]), ())
    assertEquals(told(windows).map(_.toString), Seq("[\"a\" \"b\"]", "[\"c\" \"d\"]"))
  }

  def value(name: String) = Clj.value("okay.clojure.programs", name).fold(e => fail(e), identity)

  test("okay-clojure.md: okay's effects from Clojure, okay.core") {
    val prog = Program.run[Reader % Long + State % Long, java.lang.Long](value("reader-state"))(
      using summon, Program.Row.of[Reader % Long] | Program.Row.of[State % Long])
    val answer = !.run(State.handle(5L)(Reader.run(7L)(prog)))   // (6, 7006)
    assertEquals((answer._1, answer._2.longValue), (6L, 7006L))
  }

  test("okay-clojure.md: lazy seqs, both ways") {
    val range = Clj.eval("(range)").fold(e => fail(e), identity)
    val c = Program.chunks[java.lang.Long](range)            // a Clojure (range), infinite, as okay Chunks
    assertEquals(Chunks.foldLeft(Chunks.take(c)(3))(Vector.empty[Long])(_ :+ _.longValue), Vector(0L, 1L, 2L))
    val s = Program.seq(Chunks.map(Chunks.range(0, 5))(Long.box))   // okay Chunks as a Clojure lazy seq
    assertEquals(Clj.fn("clojure.core", "vec").fold(e => fail(e), identity).invoke(s).toString, "[0 1 2 3 4]")
  }
