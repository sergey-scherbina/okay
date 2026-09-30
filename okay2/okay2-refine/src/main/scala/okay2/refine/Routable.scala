package okay2.refine

import okay2.{!, Writer, pure}
import okay2.async.{Async, Scheduler}
import okay2.stream.{Bulk, Channel, Source}
import okay2.stream.Channel.ChannelOps
import okay2.stream.Source.SourceOps

/**
 * WHAT A ROUTING TABLE CAN BE RUN OVER — okay-refine's `Routable`
 * (okay's specs/refine.md, refine-routable) on the Scala 2 core: a
 * carrier `C` of inputs (a `Vector`, any `Bulk` collection — `Chunks`,
 * okay2-spark's `Rows` — or a `Source` stream) and the three things
 * routing needs from it: every input TAGGED ONCE, each lane's values as
 * the carrier's own kind, the COUNTS. One table, one call,
 * `Kinds.split(c)`, for every carrier; a new carrier is one instance.
 * On the carrier VALUE, so an alias (`Source`, `Chunks`) resolves as
 * written — and unlike Scala 3, Scala 2 sees `Chunks[A]` through its
 * alias as the generic `D[A]`, so it needs no instance of its own.
 */
trait Routable[C] {
  type Elem
  type Out[X]
  type Done[X]
  def fan[R, T](c: C, lanes: Int)(tag: Elem => Either[R, (Int, T)]): Routable.Fanned[Out, Done, R, T]
  def select[T, X](o: Out[T])(f: T => Option[X]): Out[X]
  def done[X, Y](d: Done[X])(f: X => Y): Done[Y]
}

object Routable {
  type Id[X] = X
  type Later[X] = X ! Async
  /** a carrier with its lane and count shapes SAID, so `split` returns them */
  type Aux[C, A, O[_], D[_]] = Routable[C] { type Elem = A; type Out[X] = O[X]; type Done[X] = D[X] }

  /** lane `i`'s values, the rejects, the counts (per lane, rejected) */
  trait Fanned[Out[_], Done[_], R, T] {
    def lane(i: Int): Out[T]
    def rejected: Out[R]
    def counts: Done[(Vector[Int], Int)]
  }

  private def count[R, T](n: Int, tags: Iterable[Either[R, (Int, T)]]): (Vector[Int], Int) = {
    val per = Array.fill(n)(0)
    var rj = 0
    tags.foreach {
      case Right((i, _)) => per(i) += 1
      case Left(_) => rj += 1
    }
    (per.toVector, rj)
  }

  /** a Vector, in this JVM, now */
  implicit def vector[A]: Aux[Vector[A], A, Vector, Id] = new Routable[Vector[A]] {
    type Elem = A
    type Out[X] = Vector[X]
    type Done[X] = X
    def fan[R, T](c: Vector[A], lanes: Int)(tag: A => Either[R, (Int, T)]): Fanned[Vector, Id, R, T] = {
      val tagged = c.map(tag)
      new Fanned[Vector, Id, R, T] {
        def lane(i: Int): Vector[T] = tagged.collect { case Right((j, t)) if j == i => t }
        def rejected: Vector[R] = tagged.collect { case Left(r) => r }
        def counts: (Vector[Int], Int) = count(lanes, tagged)
      }
    }
    def select[T, X](o: Vector[T])(f: T => Option[X]): Vector[X] = o.flatMap(t => f(t))
    def done[X, Y](d: X)(f: X => Y): Y = f(d)
  }

  /** any `Bulk` collection — okay2-spark's `Rows` included: tagged once
   * and CACHED, a lane a filter over that, the counts one aggregate */
  implicit def bulk[D[_], A](implicit B: Bulk[D]): Aux[D[A], A, D, Id] = new Routable[D[A]] {
    type Elem = A
    type Out[X] = D[X]
    type Done[X] = X
    def fan[R, T](c: D[A], lanes: Int)(tag: A => Either[R, (Int, T)]): Fanned[D, Id, R, T] = {
      val tagged = B.cache(B.map(c)(tag))
      new Fanned[D, Id, R, T] {
        def lane(i: Int): D[T] = B.flatMap(tagged) {
          case Right((j, t)) if j == i => Some(t)
          case _ => None
        }
        def rejected: D[R] = B.flatMap(tagged)(_.left.toOption)
        def counts: (Vector[Int], Int) = B.aggregate(tagged)(Routes.counting[R, T](lanes))
      }
    }
    def select[T, X](o: D[T])(f: T => Option[X]): D[X] = B.flatMap(o)(t => f(t))
    def done[X, Y](d: X)(f: X => Y): Y = f(d)
  }

  /** A STREAM, read once: every lane is a channel of its own, and the
   * counts are the PROGRAM that reads, tags and sends — run it first
   * (unbounded lanes, the default) or beside the readers
   * (`stream(capacity)`, then they must). Every channel is closed at the
   * end of the input, failed on its error. */
  def stream[A](capacity: Int)(implicit S: Scheduler): Aux[Source[A], A, Source, Later] = new Routable[Source[A]] {
    type Elem = A
    type Out[X] = Source[X]
    type Done[X] = X ! Async
    def fan[R, T](c: Source[A], lanes: Int)(tag: A => Either[R, (Int, T)]): Fanned[Source, Later, R, T] = {
      val chans = Vector.fill(lanes)(Channel[T](capacity))
      val rejects = Channel[R](capacity)
      val all: Vector[Channel[_]] = chans :+ rejects
      new Fanned[Source, Later, R, T] {
        def lane(i: Int): Source[T] = chans(i).drained
        def rejected: Source[R] = rejects.drained
        def counts: (Vector[Int], Int) ! Async = {
          val per = Array.fill(lanes)(0)
          var rj = 0
          val one: A => Unit ! Async = a => tag(a) match {
            case Right((i, t)) => per(i) += 1; chans(i).send(t).map(_ => ())
            case Left(r) => rj += 1; rejects.send(r).map(_ => ())
          }
          Async.attempt(c.runForeach(one)).flatMap {
            case Right(()) =>
              all.foreach(_.close())
              pure[Async, (Vector[Int], Int)]((per.toVector, rj))
            case Left(e) =>
              all.foreach(_.fail(e))
              Async.await[(Vector[Int], Int)] { k => k(Left(e)); () => () }
          }
        }
      }
    }
    def select[T, X](o: Source[T])(f: T => Option[X]): Source[X] = Writer.expand[T, X, Unit, Async](o)(t => f(t).toVector)
    def done[X, Y](d: X ! Async)(f: X => Y): Y ! Async = d.map(f)
  }

  /** a stream with unbounded lanes: the driver never waits for a reader */
  implicit def source[A](implicit S: Scheduler): Aux[Source[A], A, Source, Later] = stream[A](Int.MaxValue)
}
