package okay.refine

import scala.collection.mutable
import scala.reflect.TypeTest
import okay.{Async, Channel, Scheduler, Source, runForeach}
import okay.freer.{!, Aggregator, effect, pure}
/**
 * A ROUTING TABLE AS A VALUE, run anywhere (specs/refine.md,
 * refine-bulk). Where `Router` binds each rule to a channel as it is
 * written, `Routes` separates the two: the table names its LANES —
 * typed handles, declared like the cases of a `match` — and the same
 * table then runs over any `Bulk` (Chunks in one JVM, `SparkBulk` on a
 * cluster: nothing here names a platform) or into channels:
 *
 * {{{
 * object Kinds extends Routes(Fin.any):
 *   val swaps  = route[Swap]
 *   val rates  = route[Swap | Cds]
 *   val eurCds = route("eurCds") { case c: Cds if c.ccy == "EUR" => c }
 *
 * val out = Kinds.split(docs)                  // docs: D[A], any Bulk D
 * out(Kinds.swaps): D[Swap]                    // typed, no cast
 * out.rejected; out.counts                     // nothing lost, one pass
 *
 * Kinds.run(source)(Kinds.swaps ~> swapCh, Kinds.rejected ~> dead)
 * }}}
 *
 * EFFICIENCY: `split` recognises each document ONCE — the pattern is
 * the expensive part, a parse — and keeps (lane, value) cached; a
 * lane is then a filter on an Int over that, and `counts` is one
 * aggregate. On Spark the pattern runs on the executors, where the
 * documents are.
 *
 * Declare the table as an `object`: a lane's test travels to an
 * executor inside the task (a `TypeTest` is Serializable), and an
 * object is re-created there by reference rather than copied.
 *
 * The rules are `Router`'s: the FIRST lane that fits wins (declaration
 * order, like a `match`); an `Unclear` or declined document, and a
 * value no lane fits, is `rejected` with why. A lane `run` was given no
 * channel for rejects its values as "not bound here" — never drops them.
 */
abstract class Routes[A, B](val pattern: Refine[A, B]) extends Serializable:
  import Router.{Rejected, Routed}

  private val table = mutable.ArrayBuffer.empty[Lane[?]]

  /**
   * A lane: its name, its place in the table, whether it takes a
   * recognised value (`accepts`, which sees the verdict's path — a
   * `byName` lane needs it) and the value as this lane types it
   * (`project`).
   */
  final class Lane[X] private[Routes] (val name: String, val index: Int,
                                       accept: (B, Path) => Boolean, proj: B => Option[X]) extends Serializable:
    def accepts(b: B, by: Path): Boolean = accept(b, by)
    def project(b: B): Option[X] = proj(b)
    /** this lane's values go to `c`, when the table is `run` */
    def ~>(c: Channel[X]): Binding = Binding.ToLane(index, b => proj(b).fold(pure[Async, Unit](()))(x => c.send(x).map(_ => ())), c)
    override def toString: String = s"Lane($name)"

  private def lane[X](name: String, accept: (B, Path) => Boolean, proj: B => Option[X]): Lane[X] =
    val l = Lane[X](name, table.length, accept, proj)
    table += l
    l

  /** every recognised value of type `X` — a class, a case, or a union
   * (`route[Swap | Cds]`); named as the type is written */
  protected def route[X <: B](using tt: TypeTest[B, X], n: TypeName.Named[X]): Lane[X] = routeAs[X](n.name)

  /** `route[X]` under a name of the table's choosing */
  protected def routeAs[X <: B](name: String)(using tt: TypeTest[B, X]): Lane[X] =
    lane[X](name, (b, _) => tt.unapply(b).isDefined, b => tt.unapply(b))

  /** by PATTERN MATCHING: the values the partial function is defined at, as it maps them */
  protected def route[X](name: String)(pf: PartialFunction[B, X]): Lane[X] =
    lane[X](name, (b, _) => pf.isDefinedAt(b), pf.lift)

  /** by the NAME of the pattern that took the document (the verdict path's last step) */
  protected def byName(name: String): Lane[B] =
    lane[B](name, (_, by) => by.steps.lastOption.contains(name), Some(_))

  /** the lanes, in the table's order */
  def lanes: Vector[Lane[?]] = table.toVector

  /** where a lane's values go when the table is `run` — made by `lane ~> channel` */
  enum Binding:
    case ToLane(index: Int, send: B => Unit ! Async, channel: Channel[?])
    case ToRejected(channel: Channel[Rejected[A, B]])

  /** where what no lane takes goes, when the table is `run`: `rejected ~> channel` */
  object rejected:
    def ~>(c: Channel[Rejected[A, B]]): Binding = Binding.ToRejected(c)

  /** one document: its lane and its recognised value, or why it has none */
  def tag(a: A): Either[Rejected[A, B], (Int, B)] =
    pattern.run(a) match
      case v @ Verdict.Took(b, by, _) =>
        table.iterator.find(_.accepts(b, by)).map(l => (l.index, b)).toRight(Rejected(a, v, s"no route for ${by}"))
      case v @ Verdict.Unclear(cs, _) => Left(Rejected(a, v, s"unclear: ${cs.map(_._1).mkString(" | ")}"))
      case v @ Verdict.Declined(_) => Left(Rejected(a, v, "declined by every pattern"))

  /** the name of the lane `a` goes to, or why none */
  def decide(a: A): Either[Rejected[A, B], String] = tag(a).map((i, _) => table(i).name)

  /** THE TABLE OVER ANY CARRIER — a `Vector`, a `Bulk` collection (Chunks,
   * `SparkBulk`), a `Source` stream: whatever has a `Routable`. Each input
   * is tagged once; each lane comes back as the carrier's own kind */
  def split[C, O[_], D[_]](c: C)(using r: Routable.Aux[C, A, O, D]): Split[O, D] =
    Split[O, D](r.fan(c, table.length)(tag),
      [T, X] => (o: O[T], f: T => Option[X]) => r.select(o)(f),
      [X, Y] => (d: D[X], f: X => Y) => r.done(d)(f))

  /** a table's answer over a carrier: each lane's values, the rejects, the counts */
  final class Split[O[_], D[_]] private[Routes] (fanned: Routable.Fanned[O, D, Rejected[A, B], B],
                                                select: [T, X] => (O[T], T => Option[X]) => O[X],
                                                done: [X, Y] => (D[X], X => Y) => D[Y]):
    /** the values of one lane, as the lane types them */
    def apply[X](l: Lane[X]): O[X] = select(fanned.lane(l.index), l.project)
    /** everything no lane took, with why */
    def rejected: O[Rejected[A, B]] = fanned.rejected
    /** how many went down each lane, and how many were rejected (for a
     * stream: the program that moves the data — run it beside the readers) */
    def counts: D[Routed] = done(fanned.counts, (per, rj) => Routed(lanes.map(_.name).zip(per), rj))
    /** let go of what the tagging holds, when no lane will be read again
     * (on Spark: the persisted rows); a no-op for a Vector or a stream */
    def release(): Unit = fanned.release()

  /** THE TABLE INTO CHANNELS: `Kinds.run(source)(Kinds.swaps ~> c1, Kinds.rejected ~> dead)`.
   * Every channel given is closed once at the end, failed with the input's error */
  def run(source: Source[A])(bindings: Binding*)(using Scheduler): Routed ! Async =
    val sends = bindings.collect { case Binding.ToLane(i, s, _) => i -> s }.toMap
    val rejects = bindings.collectFirst { case Binding.ToRejected(c) => c }
    val channels = bindings.map {
      case Binding.ToLane(_, _, c) => c
      case Binding.ToRejected(c) => c
    }.foldLeft(Vector.empty[Channel[?]])((seen, c) => if seen.exists(_ eq c) then seen else seen :+ c)
    val counts = Array.fill(table.length)(0)
    var rejected = 0
    def reject(r: Rejected[A, B]): Unit ! Async =
      rejected += 1
      rejects.fold(pure[Async, Unit](()))(_.send(r).map(_ => ()))
    def one(a: A): Unit ! Async = tag(a) match
      case Right((i, b)) => sends.get(i) match
        case Some(send) =>
          counts(i) += 1
          send(b)
        case None => reject(Rejected(a, pattern.run(a), s"lane ${table(i).name} is not bound here"))
      case Left(r) => reject(r)
    Async.attempt(source.runForeach(one)).flatMap {
      case Right(()) =>
        channels.foreach(_.close())
        pure(Routed(lanes.map(_.name).zip(counts.toVector), rejected))
      case Left(e) =>
        channels.foreach(_.fail(e))
        effect(Async.Await[Routed](k => { k(Left(e)); () => () }))
    }

object Routes:
  /** per-lane counts and the rejects, as an Aggregator (Spark's zero / seqOp / combOp) */
  private[refine] def counting[R, B](n: Int): Aggregator[Either[R, (Int, B)], (Vector[Int], Int), (Vector[Int], Int)] =
    new Aggregator[Either[R, (Int, B)], (Vector[Int], Int), (Vector[Int], Int)]:
      def init = (Vector.fill(n)(0), 0)
      def add(acc: (Vector[Int], Int), in: Either[R, (Int, B)]) = in match
        case Right((i, _)) => (acc._1.updated(i, acc._1(i) + 1), acc._2)
        case Left(_) => (acc._1, acc._2 + 1)
      def merge(a: (Vector[Int], Int), b: (Vector[Int], Int)) = (a._1.lazyZip(b._1).map(_ + _), a._2 + b._2)
      def present(acc: (Vector[Int], Int)) = acc
