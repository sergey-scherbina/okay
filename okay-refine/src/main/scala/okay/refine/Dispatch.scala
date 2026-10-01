package okay.refine

import scala.annotation.unused
import scala.collection.mutable
import scala.reflect.TypeTest
import scala.util.control.NonFatal

/**
 * HIERARCHICAL ROUTING WRITTEN AS A `match` (specs/refine-dispatch.md):
 * the routing table is the user's own Scala `match` over what the
 * pattern recognised, so the compiler checks it —
 *
 * {{{
 * object Desk extends Dispatch(Fin.any):
 *   val eurSwaps = lane[Swap]("rates/swaps/eur")
 *   val swaps    = lane[Swap]("rates/swaps/other")
 *   val fx       = lane[FxForward]("fx")
 *
 *   def table(i: Instrument): To = i match
 *     case r: Rate      => rates(r)            // a sub-table is a method
 *     case f: FxForward => fx(f)              // fx(f) compiles only for an FxForward
 *     // a missing case over a sealed type: "match may not be exhaustive"
 *
 *   def rates(r: Rate): To = r match
 *     case s: Swap if s.ccy == "EUR" => eurSwaps(s)
 *     case s: Swap                   => swaps(s)
 *     case _: Cds                    => unrouted("CDS go to the credit desk")
 *
 * Desk.split(docs)       // any Routable: a Vector, Chunks, Spark, Flink, a Source
 * }}}
 *
 * A `To` is made only by a lane (typed: `swaps(s)` needs a `Swap`) or by
 * `unrouted(why)` — every case says where, or why not. A document the
 * PATTERN did not take never reaches the table: it is rejected with the
 * verdict. A table that throws for one document (a `MatchError` of a
 * partial match) rejects that document, named, and the rest is routed.
 * Lanes are named by path; `Routed.under("rates")` sums a subtree.
 */
abstract class Dispatch[A, B](val pattern: Refine[A, B]) extends Serializable:
  import Router.{Rejected, Routed}

  private val registry = mutable.ArrayBuffer.empty[Lane[?]]

  /** a lane: its name (a path, "rates/swaps/eur"), its place, and the
   * test that takes its values back out of a carrier that holds every lane */
  final class Lane[X] private[Dispatch] (val name: String, val index: Int, test: TypeTest[Any, X]) extends Serializable:
    /** deliver `x` to this lane — the table's way of saying where */
    def apply(x: X): To = new To(index, x, "")
    /** this lane's value back out of the carrier; `Some` for what `apply` put in */
    def take(v: Any): Option[X] = test.unapply(v)
    override def toString: String = s"Lane($name)"

  /** a value delivered to a lane, or a refusal: made only by a lane or `unrouted` */
  final class To private[Dispatch] (val lane: Int, val value: Any, val why: String) extends Serializable

  /** a lane named by a path, its values of type `X` */
  protected def lane[X](name: String)(using tt: TypeTest[Any, X]): Lane[X] =
    require(!registry.exists(_.name == name), s"a lane named $name is declared twice")
    val l = Lane[X](name, registry.length, tt)
    registry += l
    l

  /** the table's own refusal: this document is recognised, and goes nowhere, because `why` */
  protected def unrouted(why: String): To = new To(-1, (), why)

  /** THE TABLE: a `match` over what the pattern recognised */
  def table(b: B): To

  /** the table with the verdict's path, for routing on WHERE a value was read
   * (the same `Swap` from FpML or from CDM); by default the path is ignored */
  def table(b: B, @unused by: Path): To = table(b)

  /** the lanes, in declaration order */
  def lanes: Vector[Lane[?]] = registry.toVector

  /** one document: its lane and its delivered value, or why none */
  def tag(a: A): Either[Rejected[A, B], (Int, Any)] =
    pattern.run(a) match
      case v @ Verdict.Took(b, by, _) =>
        try
          val to = table(b, by)
          if to.lane < 0 then Left(Rejected(a, v, to.why)) else Right((to.lane, to.value))
        catch case NonFatal(e) => Left(Rejected(a, v, s"the table threw ${e.getClass.getSimpleName}: ${e.getMessage}"))
      case v @ Verdict.Unclear(cs, _) => Left(Rejected(a, v, s"unclear: ${cs.map(_._1).mkString(" | ")}"))
      case v @ Verdict.Declined(_) => Left(Rejected(a, v, "declined by every pattern"))

  /** the name of the lane `a` goes to, or why none */
  def decide(a: A): Either[Rejected[A, B], String] = tag(a).map((i, _) => registry(i).name)

  /** THE TABLE OVER ANY CARRIER (`Routable`): each document recognised and
   * dispatched once; each lane back as the carrier's own kind */
  def split[C, O[_], D[_]](c: C)(using r: Routable.Aux[C, A, O, D]): Split[O, D] =
    Split[O, D](r.fan(c, registry.length)(tag),
      [T, X] => (o: O[T], f: T => Option[X]) => r.select(o)(f),
      [X, Y] => (d: D[X], f: X => Y) => r.done(d)(f))

  final class Split[O[_], D[_]] private[Dispatch] (fanned: Routable.Fanned[O, D, Rejected[A, B], Any],
                                                  select: [T, X] => (O[T], T => Option[X]) => O[X],
                                                  done: [X, Y] => (D[X], X => Y) => D[Y]):
    /** one lane's values, typed by the lane */
    def apply[X](l: Lane[X]): O[X] = select(fanned.lane(l.index), l.take)
    /** what no lane took: not recognised, `unrouted`, or a table that threw — each with why */
    def rejected: O[Rejected[A, B]] = fanned.rejected
    /** how many went down each lane, and how many were rejected */
    def counts: D[Routed] = done(fanned.counts, (per, rj) => Routed(lanes.map(_.name).zip(per), rj))
    /** let go of what the dispatching holds (Spark's persisted rows) */
    def release(): Unit = fanned.release()
