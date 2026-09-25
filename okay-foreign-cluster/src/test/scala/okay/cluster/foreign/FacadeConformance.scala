package okay.cluster.foreign

import munit.Assertions.*
import okay.codec.Schema
import okay.arrow.Rows
import okay.cluster.{Flow, Flows}
import okay.given

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

  /** `Speaks`: a report names the language and the link, and what it
   * says of frames is one of the three words the spec has */
  def speaks[M](module: M, language: String)(using s: Speaks[M]): Speaks.Report =
    val r = s.speaks(module)
    assertEquals(r.language, language)
    assert(Set("arrow", "columnar-json", "by-reference")(r.frames), r.toString)
    assert(Set("multi-shot", "one-shot", "in-jvm", "none")(r.programs), r.toString)
    r
