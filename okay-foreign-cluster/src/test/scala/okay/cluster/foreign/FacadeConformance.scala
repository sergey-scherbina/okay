package okay.cluster.foreign

import munit.Assertions.*
import okay.codec.Schema
import okay.arrow.Rows
import okay.cluster.{Flow, Flows}
import okay.given
import okay.{!, %, Choose, Reader, effect, runChoice}

/**
 * THE CONFORMANCE SUITE (specs/foreign-facade.md): one body per
 * capability, run once per instance a language gives — the same text
 * for Python, R, the JVM and a test's own module type. A language that
 * claims an instance passes this; a language that passes this claims it.
 */
object FacadeConformance:
  /** a record every tier-1 road carries: an int, a double (R has no
   * 64-bit integer), a string */
  final case class Rec(key: Int, v: Double, name: String) derives Schema

  /**
   * `Calls`: a value goes in and comes back at its type through `echo`;
   * a function that raises is a REFUSAL by kind through `boom`, never an
   * exception on this side; a function the module does not have is a
   * refusal too.
   */
  def calls[M](module: M, echo: String, boom: String, missing: String = "no_such_function")(using c: Calls[M]): Unit =
    val rec = Rec(7, 2.5, "ann")
    assertEquals(c.call[Rec, Rec](module, echo)(rec), Right(rec), s"${c.name}: echo")
    c.call[Rec, Rec](module, boom)(rec) match
      case Left(Batcher.Failed(kind, message)) => assert(kind.nonEmpty, s"${c.name}: boom refused without a kind: $message")
      case Right(v) => fail(s"${c.name}: boom answered $v")
    c.call[Rec, Rec](module, missing)(rec) match
      case Left(_) => ()
      case Right(v) => fail(s"${c.name}: a missing function answered $v")

  /**
   * `Frames`: rows go over as ONE table and come back as one, equal row
   * for row through `echo` (a frame function that answers its frame);
   * `boom` is a refusal by kind; the empty table crosses too.
   */
  def frames[M](module: M, echo: String, boom: String)(using f: Frames[M]): Unit =
    val recs = Vector(Rec(1, 1.5, "ann"), Rec(2, -2.0, "bob"), Rec(3, 0.0, ""))
    assertEquals(Road.rows[M, Rec, Rec](module, echo)(recs), Right(recs), s"${f.name}: echo")
    assertEquals(Road.rows[M, Rec, Rec](module, echo)(Vector.empty), Right(Vector.empty), s"${f.name}: empty")
    f.frame(module, boom)(Rows.table(recs)) match
      case Left(Batcher.Failed(kind, message)) => assert(kind.nonEmpty, s"${f.name}: boom refused without a kind: $message")
      case Right(t) => fail(s"${f.name}: boom answered ${t.rows} rows")

  /**
   * `Streams`: `n` rows go through `echo` in frames of `batch`; every
   * row comes back, in order, and no frame ever held more than `batch`
   * rows — which is the memory bound on both sides. `seen` is how the
   * suite counts frames where it can (a fake, the JVM); a real far side
   * is checked by the rows alone.
   */
  def streams[M](module: M, echo: String, n: Int, batch: Int, seen: () => Vector[Int] = () => Vector.empty)(using s: Streams[M]): Unit =
    val recs = Vector.tabulate(n)(i => Rec(i, i * 0.5, s"r$i"))
    val out = Flows.collect(Road.flow[M, Rec, Rec](module, echo, batch)(Flow.slices(recs, 1))).runWith
    assertEquals(out.toVector, recs, s"${s.name}: every row back, in order")
    val sizes = seen()
    if sizes.nonEmpty then
      assert(sizes.forall(_ <= batch), s"${s.name}: a frame held more than $batch rows: $sizes")
      assertEquals(sizes.sum, n, s"${s.name}: the frames add up to the rows")

  /** an order a program prices through a callback */
  final case class Order(sku: String, qty: Long) derives Schema

  /**
   * `Programs`: a far-side program performs `price_of` by name and this
   * side answers it under a Reader (`priced`); and a program that
   * performs `choose` twice is continued as often as Choice asks —
   * MULTI-SHOT across the process (`pairs`) — the two dialogues of
   * specs/remote-foreign.md, over the facade.
   */
  def programs[M](module: M, priced: String, pairs: String)(using P: Programs[M]): Unit =
    val price = Cb[Reader % Map[String, Double], String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
    val order = P.run(module)(Reader.run(Map("tea" -> 4.0))(P.program[Order, Double, Reader % Map[String, Double]](module, priced, Vector(price))(Order("tea", 3L))))
    assertEquals(order, Right(12.0), s"${P.name}: priced")
    val choose = Cb[Choose, Vector[Long], Long]("choose")(xs => effect[Choose, Long](Choose(xs)))
    val all = P.run(module)(runChoice(P.program[Int, Long, Choose](module, pairs, Vector(choose))(0)))
    assertEquals(all.toList, List(Right(11L), Right(21L), Right(12L), Right(22L)), s"${P.name}: pairs, multi-shot")

  /**
   * `Holds`: `make` holds an object on the far side; `describe` takes the
   * handle as its first argument and a value as its second; the handle is
   * released. Two handles are two objects.
   */
  def holds[M](module: M, make: String, describe: String)(using H: Holds[M]): Unit =
    val a = H.hold[Long](module, make)(3L).fold(f => fail(s"${H.name}: hold: $f"), identity)
    val b = H.hold[Long](module, make)(10L).fold(f => fail(s"${H.name}: hold: $f"), identity)
    assertEquals(H.apply[Long, Long](module, describe)(a, 2L), Right(5L), s"${H.name}: describe a")
    assertEquals(H.apply[Long, Long](module, describe)(b, 2L), Right(12L), s"${H.name}: describe b")
    H.release(module)(a)
    H.release(module)(b)

  /** `Methods`: a held object's method and attribute, through the same
   * handle `Holds` gave */
  def methods[M](module: M, make: String, method: String, attr: String)(using H: Holds[M], Me: Methods[M] { type Ref = H.Ref }): Unit =
    val c = H.hold[Long](module, make)(3L).fold(f => fail(s"${H.name}: hold: $f"), identity)
    assertEquals(Me.method[Long, Long](module, c, method)(4L), Right(7L), s"${H.name}: method")
    assertEquals(Me.attr[Long](module, c, attr), Right(3L), s"${H.name}: attr")
    H.release(module)(c)

  /** `Speaks`: a report names the language and the link, and what it
   * says of frames is one of the three words the spec has */
  def speaks[M](module: M, language: String)(using s: Speaks[M]): Speaks.Report =
    val r = s.speaks(module)
    assertEquals(r.language, language)
    assert(Set("arrow", "columnar-json", "by-reference")(r.frames), r.toString)
    assert(Set("multi-shot", "one-shot", "in-jvm", "none")(r.programs), r.toString)
    r
