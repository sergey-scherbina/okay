package okay.scala2

import okay.{!, %, +, Take}
import okay.Row.plus
import okay.given
import Rows.coerce

/**
 * A stream for Scala 2.13 (specs/scala2-facade.md, stage 4).
 *
 * The core's `okay.Source[A]` is `Unit ! (Writer % A + Async)`: a
 * program that TELLS its elements and may perform Async between them.
 * That is exactly `Eff[Writer[A] with Async, Unit]`, so a source can be
 * written as an ordinary for-comprehension of `Writer.tell` and
 * `Async.delay` and turned into a `Source` by `fromEff`. This class
 * gives it the stream vocabulary, each word one call into the core:
 * `map` is `Writer.map`, `filter` is `Writer.expand`, `take`/`drop`
 * are stages driven by `through`. A stage that has finished stops
 * pulling from its source, so `take` on an infinite source ends.
 */
final class Source[A] private (private val body: SourceBody[A]) {

  private[scala2] def core: okay.Source[A] = body.s

  def map[B](f: A => B): Source[B] = Source.of(okay.Writer.map[A, B, Unit, okay.Async](core)(f))

  def filter(p: A => Boolean): Source[A] =
    Source.of(okay.Writer.expand[A, A, Unit, okay.Async](core)(a => if (p(a)) Vector(a) else Vector.empty))

  /** each element becomes any number of elements, in order */
  def mapConcat[B](f: A => Iterable[B]): Source[B] =
    Source.of(okay.Writer.expand[A, B, Unit, okay.Async](core)(a => f(a).toIndexedSeq))

  def take(n: Int): Source[A] = Source.through(core)(Source.taking[A](n))

  def takeWhile(p: A => Boolean): Source[A] = Source.through(core)(Source.takingWhile[A](p))

  def drop(n: Int): Source[A] = Source.through(core)(Source.dropping[A](n))

  def zipWithIndex: Source[(A, Long)] =
    Source.through(core)(Source.indexing[A](0L))

  /** this source, then `that` */
  def ++(that: => Source[A]): Source[A] = Source.of(core.flatMap(_ => that.core))

  /** both at once: a fiber per source feeding one channel, elements in
   * the order they ARRIVE (okay's `Channel.merge`) */
  def merge(that: Source[A]): Source[A] =
    Source.of(okay.drained(okay.Channel.merge[A, okay.Source, okay.Async, okay.Source, okay.Async](core, that.core)))

  /** every element, in order */
  def runCollect: Eff[Async, Vector[A]] = Eff.of(coerce(okay.runCollect(core)))

  def runForeach(f: A => Eff[Async, Unit]): Eff[Async, Unit] =
    Eff.of(coerce(okay.runForeach(core)(a => coerce[Rows.Top, okay.Async, Unit](f(a).program))))

  def runFold[S](z: S)(f: (S, A) => S): Eff[Async, S] =
    Eff.of(coerce(okay.Writer.loopWith[A, S, Unit, S, okay.Async](core)(z)(f)((s, _) => s)))

  /** the program this source is */
  def toEff: Eff[Writer[A] & Async, Unit] = Eff.of(coerce(core))
}

/** held out of `Source`'s constructor, as `ProgBody` is out of `Prog`'s */
private[scala2] final class SourceBody[A](val s: okay.Source[A]) extends AnyVal

object Source {

  private[scala2] def of[A](s: okay.Source[A]): Source[A] = new Source(new SourceBody(s))

  def apply[A](as: A*): Source[A] = of(okay.Source(as*))

  def fromIterable[A](as: Iterable[A]): Source[A] = of(okay.Source.of(as.toList))

  def empty[A]: Source[A] = of(okay.pure(()))

  /** the half-open range */
  def range(from: Long, until: Long): Source[Long] = of(okay.Source.range(from, until))

  /** peel elements off `s` until `f` answers None */
  def unfold[S, A](s: S)(f: S => Option[(A, S)]): Source[A] = of(okay.Source.unfold(s)(f))

  /** a source written as a program that tells */
  def fromEff[A](e: Eff[Writer[A] & Async, Unit]): Source[A] = of(coerce(e.program))

  // ---- stages: the stage's row carries Async too, so `through` can
  // forward the source's own Async operations past it

  private type St[I, O] = Take % I + (okay.Writer % O + okay.Async)

  private def through[I, O](s: okay.Source[I])(st: Unit ! St[I, O]): Source[O] =
    of(okay.through[I, O, okay.Async, Unit, Unit](s)(st))

  private def await[I, O]: Option[I] ! St[I, O] = okay.Stage.await[I, O].plus[okay.Async]
  private def tell[I, O](o: O): Unit ! St[I, O] = okay.Stage.tell[I, O](o).plus[okay.Async]
  private def done[I, O]: Unit ! St[I, O] = okay.pure(())

  private def passAll[A]: Unit ! St[A, A] =
    await[A, A].flatMap {
      case Some(a) => tell[A, A](a).flatMap(_ => passAll[A])
      case None => done[A, A]
    }

  private def taking[A](n: Int): Unit ! St[A, A] =
    if (n <= 0) done[A, A]
    else await[A, A].flatMap {
      case Some(a) => tell[A, A](a).flatMap(_ => taking[A](n - 1))
      case None => done[A, A]
    }

  private def takingWhile[A](p: A => Boolean): Unit ! St[A, A] =
    await[A, A].flatMap {
      case Some(a) if p(a) => tell[A, A](a).flatMap(_ => takingWhile[A](p))
      case _ => done[A, A]
    }

  private def dropping[A](n: Int): Unit ! St[A, A] =
    if (n <= 0) passAll[A]
    else await[A, A].flatMap {
      case Some(_) => dropping[A](n - 1)
      case None => done[A, A]
    }

  private def indexing[A](i: Long): Unit ! St[A, (A, Long)] =
    await[A, (A, Long)].flatMap {
      case Some(a) => tell[A, (A, Long)]((a, i)).flatMap(_ => indexing[A](i + 1))
      case None => done[A, (A, Long)]
    }
}
