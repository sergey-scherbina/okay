package okay2.refine

import scala.collection.mutable
import scala.reflect.ClassTag
import okay2.{!, Aggregator, pure}
import okay2.async.{Async, Scheduler}
import okay2.stream.{Bulk, Channel, Source}
import okay2.stream.Source.SourceOps

/**
 * A ROUTING TABLE AS A VALUE, run anywhere — okay-refine's `Routes`
 * (okay's specs/refine.md, refine-bulk) on the Scala 2 core. The table
 * names its LANES — typed handles, declared like the cases of a
 * `match` — and the same table runs over any `Bulk` (`Chunks` in one
 * JVM, okay2-spark's `SparkBulk` on a cluster) or into channels:
 *
 * {{{
 * object Kinds extends Routes(any) {
 *   val swaps  = route[Swap]
 *   val eurCds = route("eurCds") { case c: Cds if c.ccy == "EUR" => c }
 * }
 * val out = Kinds.split(docs)            // docs: D[A], any Bulk D
 * out(Kinds.swaps): D[Swap]              // typed, no cast
 * out.rejected; out.counts               // nothing lost, one pass
 * Kinds.run(source)(Kinds.swaps ~> swapCh, Kinds.rejected ~> dead)
 * }}}
 *
 * `split` recognises each document ONCE and caches (lane, value); a
 * lane is then a filter on an Int, and `counts` is one aggregate. The
 * FIRST lane that fits wins; an `Unclear` or declined document, and a
 * value no lane fits, is `rejected` with why; a lane `run` was given no
 * channel for rejects its values as "not bound here". Declare the table
 * as an `object`: an executor re-creates it by reference. Scala 2 has
 * no union types: `route[X]` tests the class of `X` (a `ClassTag`,
 * Serializable); several kinds into one lane are a pattern with
 * alternatives, `route("rates") { case d @ (_: Fx | _: Cds) => d }`.
 */
abstract class Routes[A, B](val pattern: Refine[A, B]) extends Serializable {
  import Router.{Rejected, Routed}

  private val table = mutable.ArrayBuffer.empty[Lane[_]]

  /** a lane: its name, its place in the table, whether it takes a
   * recognised value (`accepts`, which sees the verdict's path) and the
   * value as this lane types it (`project`) */
  final class Lane[X] private[Routes] (val name: String, val index: Int,
                                       accept: (B, Path) => Boolean, proj: B => Option[X]) extends Serializable {
    def accepts(b: B, by: Path): Boolean = accept(b, by)
    def project(b: B): Option[X] = proj(b)
    /** this lane's values go to `c`, when the table is `run` */
    def ~>(c: Channel[X]): Binding =
      new Binding(Some((index, (b: B) => proj(b).fold(pure[Async, Unit](()))(x => c.send(x).map(_ => ())))), None, c)
    override def toString: String = s"Lane($name)"
  }

  private def lane[X](name: String, accept: (B, Path) => Boolean, proj: B => Option[X]): Lane[X] = {
    val l = new Lane[X](name, table.length, accept, proj)
    table += l
    l
  }

  /** every recognised value of class `X`, under the class's name */
  protected def route[X <: B](implicit ct: ClassTag[X]): Lane[X] =
    routeAs[X](ct.runtimeClass.getSimpleName.stripSuffix("$"))

  /** `route[X]` under a name of the table's choosing */
  protected def routeAs[X <: B](name: String)(implicit ct: ClassTag[X]): Lane[X] =
    lane[X](name, (b, _) => ct.unapply(b).isDefined, b => ct.unapply(b))

  /** by PATTERN MATCHING: the values the partial function is defined at, as it maps them */
  protected def route[X](name: String)(pf: PartialFunction[B, X]): Lane[X] =
    lane[X](name, (b, _) => pf.isDefinedAt(b), pf.lift)

  /** by the NAME of the pattern that took the document (the verdict path's last step) */
  protected def byName(name: String): Lane[B] =
    lane[B](name, (_, by) => by.steps.lastOption.contains(name), Some(_))

  /** the lanes, in the table's order */
  def lanes: Vector[Lane[_]] = table.toVector

  /** where a lane's values go (or the rejects) when the table is `run` —
   * made by `lane ~> channel` or `rejected ~> channel`; a plain class, as
   * a case class inside the table would carry an outer reference no
   * pattern could check (-Xlint) */
  final class Binding private[Routes] (val lane: Option[(Int, B => Unit ! Async)],
                                       val rejects: Option[Channel[Rejected[A, B]]], val channel: Channel[_])

  /** where what no lane takes goes, when the table is `run`: `rejected ~> channel` */
  object rejected {
    def ~>(c: Channel[Rejected[A, B]]): Binding = new Binding(None, Some(c), c)
  }

  /** one document: its lane and its recognised value, or why it has none */
  def tag(a: A): Either[Rejected[A, B], (Int, B)] =
    pattern.run(a) match {
      case v @ Verdict.Took(b, by, _) =>
        table.iterator.find(_.accepts(b, by)).map(l => (l.index, b)).toRight(Rejected(a, v, s"no route for $by"))
      case v @ Verdict.Unclear(cs, _) => Left(Rejected(a, v, s"unclear: ${cs.map(_._1).mkString(" | ")}"))
      case v @ Verdict.Declined(_) => Left(Rejected(a, v, "declined by every pattern"))
    }

  /** the name of the lane `a` goes to, or why none */
  def decide(a: A): Either[Rejected[A, B], String] = tag(a).map { case (i, _) => table(i).name }

  /** THE TABLE OVER ANY `Bulk`: each document recognised once, cached */
  def split[D[_]](docs: D[A])(implicit B: Bulk[D]): Split[D] = new Split[D](B.cache(B.map(docs)(tag)))

  /** a table's answer over a `Bulk`: each lane's values, the rejects, the counts */
  final class Split[D[_]] private[Routes] (val tagged: D[Either[Rejected[A, B], (Int, B)]])(implicit B: Bulk[D]) {
    /** the values of one lane, as the lane types them */
    def apply[X](l: Lane[X]): D[X] = {
      val i = l.index
      B.flatMap(tagged) {
        case Right((j, b)) if j == i => l.project(b)
        case _ => None
      }
    }

    /** everything no lane took, with why */
    def rejected: D[Rejected[A, B]] = B.flatMap(tagged)(_.left.toOption)

    /** how many went down each lane, and how many were rejected — ONE pass */
    def counts: Routed = {
      val (per, rj) = B.aggregate(tagged)(Routes.counting[Rejected[A, B], B](table.length))
      Routed(lanes.map(_.name).zip(per), rj)
    }
  }

  /** THE TABLE INTO CHANNELS; every channel given is closed once at the
   * end, failed with the input's error */
  def run(source: Source[A])(bindings: Binding*)(implicit S: Scheduler): Routed ! Async = {
    val sends = bindings.flatMap(_.lane).toMap
    val rejects = bindings.flatMap(_.rejects).headOption
    val channels = bindings.map(_.channel).foldLeft(Vector.empty[Channel[_]])((seen, c) => if (seen.exists(_ eq c)) seen else seen :+ c)
    val counts = Array.fill(table.length)(0)
    var rejected = 0
    def reject(r: Rejected[A, B]): Unit ! Async = {
      rejected += 1
      rejects.fold(pure[Async, Unit](()))(_.send(r).map(_ => ()))
    }
    def one(a: A): Unit ! Async = tag(a) match {
      case Right((i, b)) => sends.get(i) match {
        case Some(send) =>
          counts(i) += 1
          send(b)
        case None => reject(Rejected(a, pattern.run(a), s"lane ${table(i).name} is not bound here"))
      }
      case Left(r) => reject(r)
    }
    Async.attempt(source.runForeach(one)).flatMap {
      case Right(()) =>
        channels.foreach(_.close())
        pure[Async, Routed](Routed(lanes.map(_.name).zip(counts.toVector), rejected))
      case Left(e) =>
        channels.foreach(_.fail(e))
        Async.await[Routed] { k => k(Left(e)); () => () }
    }
  }
}

object Routes {
  /** per-lane counts and the rejects, as an Aggregator (Spark's zero / seqOp / combOp) */
  private[refine] def counting[R, B](n: Int): Aggregator[Either[R, (Int, B)], (Vector[Int], Int), (Vector[Int], Int)] =
    new Aggregator[Either[R, (Int, B)], (Vector[Int], Int), (Vector[Int], Int)] {
      def init: (Vector[Int], Int) = (Vector.fill(n)(0), 0)
      def add(acc: (Vector[Int], Int), in: Either[R, (Int, B)]): (Vector[Int], Int) = in match {
        case Right((i, _)) => (acc._1.updated(i, acc._1(i) + 1), acc._2)
        case Left(_) => (acc._1, acc._2 + 1)
      }
      def merge(a: (Vector[Int], Int), b: (Vector[Int], Int)): (Vector[Int], Int) = (a._1.lazyZip(b._1).map(_ + _), a._2 + b._2)
      def present(acc: (Vector[Int], Int)): (Vector[Int], Int) = acc
    }
}
