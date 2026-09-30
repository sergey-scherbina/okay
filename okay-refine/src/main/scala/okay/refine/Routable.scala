package okay.refine

import okay.{!, Async, Bulk, Channel, Chunks, Scheduler, Source, Writer, drained, effect, pure, runForeach}
import okay.Bulk.{aggregate, cache, flatMap, map}

/**
 * WHAT A ROUTING TABLE CAN BE RUN OVER (specs/refine.md,
 * refine-routable): a carrier `C` of inputs — a `Vector`, any `Bulk`
 * collection (Chunks in one JVM, `SparkBulk`'s rows on a cluster), a
 * `Source` stream — and what routing needs from it, which is exactly
 * three things: every input TAGGED ONCE (its lane, or its rejection),
 * each lane's values handed out as the carrier's own kind of
 * collection, and the COUNTS. So one table, `Routes`, and one call,
 * `Kinds.split(c)`, route every carrier; a new carrier (a Kafka topic, a
 * Flink stream) is one more instance, and no table changes.
 *
 * The typeclass is on the carrier VALUE (`Routable[Source[A]]`), not on
 * a type constructor, so an alias (`Source`, `Chunks`) or an opaque type
 * (`SparkBulk.Rows`) is found by its written type rather than by
 * higher-kinded unification, which aliases defeat.
 */
trait Routable[C]:
  /** one input */
  type Elem
  /** one lane's values: the carrier's own kind (a Vector, a D, a Source) */
  type Out[X]
  /** what the counts come back in: now (`Id`), or as a program (`! Async`) */
  type Done[X]

  /** tag every input once, and hand out each lane's share */
  def fan[R, T](c: C, lanes: Int)(tag: Elem => Either[R, (Int, T)]): Routable.Fanned[Out, Done, R, T]
  /** a lane's values, as the lane types them */
  def select[T, X](o: Out[T])(f: T => Option[X]): Out[X]
  /** the counts' shape changed where they are */
  def done[X, Y](d: Done[X])(f: X => Y): Done[Y]

object Routable:
  type Id[X] = X
  type Of[C, A] = Routable[C] { type Elem = A }
  /** a carrier with its lane and count shapes SAID, so `split` returns them */
  type Aux[C, A, O[_], D[_]] = Routable[C] { type Elem = A; type Out[X] = O[X]; type Done[X] = D[X] }

  /** what `fan` answers: lane `i`'s values, the rejects, the counts (per lane, rejected) */
  trait Fanned[Out[_], Done[_], R, T]:
    def lane(i: Int): Out[T]
    def rejected: Out[R]
    def counts: Done[(Vector[Int], Int)]

  private def count[R, T](n: Int, tags: IterableOnce[Either[R, (Int, T)]]): (Vector[Int], Int) =
    val per = Array.fill(n)(0)
    var rj = 0
    tags.iterator.foreach {
      case Right((i, _)) => per(i) += 1
      case Left(_) => rj += 1
    }
    (per.toVector, rj)

  /** a Vector, in this JVM, now */
  given vector[A]: Aux[Vector[A], A, Vector, Id] = new Routable[Vector[A]]:
    type Elem = A
    type Out[X] = Vector[X]
    type Done[X] = X
    def fan[R, T](c: Vector[A], lanes: Int)(tag: A => Either[R, (Int, T)]): Fanned[Vector, Id, R, T] =
      val tagged = c.map(tag)
      new Fanned[Vector, Id, R, T]:
        def lane(i: Int): Vector[T] = tagged.collect { case Right((j, t)) if j == i => t }
        def rejected: Vector[R] = tagged.collect { case Left(r) => r }
        def counts: (Vector[Int], Int) = count(lanes, tagged)
    def select[T, X](o: Vector[T])(f: T => Option[X]): Vector[X] = o.flatMap(f)
    def done[X, Y](d: X)(f: X => Y): Y = f(d)

  /** any `Bulk` collection — `SparkBulk`'s rows included: tagged once
   * and CACHED, a lane a filter over that, the counts one aggregate */
  given bulk[D[_], A](using B: Bulk[D]): Aux[D[A], A, D, Id] = new Routable[D[A]]:
    type Elem = A
    type Out[X] = D[X]
    type Done[X] = X
    def fan[R, T](c: D[A], lanes: Int)(tag: A => Either[R, (Int, T)]): Fanned[D, Id, R, T] =
      val tagged = c.map(tag).cache
      new Fanned[D, Id, R, T]:
        def lane(i: Int): D[T] = tagged.flatMap {
          case Right((j, t)) if j == i => Some(t)
          case _ => None
        }
        def rejected: D[R] = tagged.flatMap(_.left.toOption)
        def counts: (Vector[Int], Int) = tagged.aggregate(Routes.counting[R, T](lanes))
    def select[T, X](o: D[T])(f: T => Option[X]): D[X] = o.flatMap(f)
    def done[X, Y](d: X)(f: X => Y): Y = f(d)

  /** `Chunks` by name: an alias the generic `D[A]` cannot see through */
  given chunks[A](using B: Bulk[Chunks]): Aux[Chunks[A], A, Chunks, Id] = bulk[Chunks, A]

  /**
   * A STREAM, read once: every lane is a channel of its own, and the
   * counts are the PROGRAM that reads the source, tags each input and
   * sends it to its lane — so run `counts` beside the lanes' readers
   * (`Async.par`), or first when the channels are unbounded (the
   * default: nothing waits, memory holds what no reader has taken yet).
   * `Routable.stream(capacity)` bounds them: then the slowest reader
   * paces the source, and the readers MUST run with the driver.
   * Every channel is closed at the end of the input, failed on its error.
   */
  def stream[A](capacity: Int)(using S: Scheduler): Aux[Source[A], A, Source, [X] =>> X ! Async] = new Routable[Source[A]]:
    type Elem = A
    type Out[X] = Source[X]
    type Done[X] = X ! Async
    def fan[R, T](c: Source[A], lanes: Int)(tag: A => Either[R, (Int, T)]): Fanned[Source, [X] =>> X ! Async, R, T] =
      val chans = Vector.fill(lanes)(Channel[T](capacity))
      val rejects = Channel[R](capacity)
      val all: Vector[Channel[?]] = chans :+ rejects
      new Fanned[Source, [X] =>> X ! Async, R, T]:
        def lane(i: Int): Source[T] = chans(i).drained
        def rejected: Source[R] = rejects.drained
        def counts: (Vector[Int], Int) ! Async =
          val per = Array.fill(lanes)(0)
          var rj = 0
          val one: A => Unit ! Async = a => tag(a) match
            case Right((i, t)) => per(i) += 1; chans(i).send(t).map(_ => ())
            case Left(r) => rj += 1; rejects.send(r).map(_ => ())
          Async.attempt(c.runForeach(one)).flatMap {
            case Right(()) =>
              all.foreach(_.close())
              pure((per.toVector, rj))
            case Left(e) =>
              all.foreach(_.fail(e))
              effect(Async.Await[(Vector[Int], Int)](k => { k(Left(e)); () => () }))
          }
    def select[T, X](o: Source[T])(f: T => Option[X]): Source[X] = Writer.expand[T, X, Unit, Async](o)(t => f(t).toVector)
    def done[X, Y](d: X ! Async)(f: X => Y): Y ! Async = d.map(f)

  /** a stream with unbounded lanes: the driver never waits for a reader */
  given source[A](using Scheduler): Aux[Source[A], A, Source, [X] =>> X ! Async] = stream[A](Int.MaxValue)
