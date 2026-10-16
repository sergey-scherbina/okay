package okay.std

import okay.{!, +, Effect, Handler, Join, Pure, Run, Within, effect, pure}
import okay.core.{Eff, Paths, Row, Split}

/**
 * A VALUE THAT MAY NOT BE THERE, as an effect of its own
 * (specs/core-gaps.md, 2026-09-26).
 *
 *     def age(name: String): Int ! Maybe = ages.get(name).maybe
 *
 * `Some(x).maybe` answers `x`; `None.maybe` stops the program, and
 * `Maybe.run` answers `None` for it — the `Option` moves out of every
 * signature along the way and into the handler at the end.
 *
 * WHY NOT `Abort`. `Abort` is `Throws % Unit`: a failure with nothing
 * to say. A program that can both find nothing and fail with a reason
 * was `Abort + Throws % E`, two of one class, which okay-freer's union
 * refused (`Distinct`); on the kernel's rows they would be two places,
 * but "nothing there" and "it broke" are still two MEANINGS, and a
 * signature should say one: `Maybe + Throws % E`, and "was it not
 * there, or did it break?" has two answers, from two handlers.
 *
 * WHY NOT `Option` ITSELF as the signature. It could be, the way a
 * `List` is nondeterminism (Choice.scala) — and Choice.scala gives the
 * reason not to: a row wants a signature that means ONE thing. The
 * box costs one allocation per operation, `Choose`'s price.
 *
 * An operation goes to its handler by its path among the program's
 * effects (`Split`), not by a test of its class.
 */
final case class Maybe[+A](value: Option[A]) extends Final derives Effect:
  /** nothing there: no handler resumes it */
  override def isFinal: Boolean = value.isEmpty

extension [A](o: Option[A])
  /** the value, or stop: `None` ends the program at `Maybe.run` */
  inline def maybe: A ! Maybe = effect[Maybe, A](Maybe(o))

object Maybe:
  /** stop: nothing is there */
  inline def none[A]: A ! Maybe = effect[Maybe, A](Maybe(None))

  /** the handler as a value: `p.handle(Maybe.option)` answers `Option[A]` — `Some` resumes the rest with its
   * value, `None` drops it (the kernel's `Deep`) */
  def option: Handler[Maybe, Option] = Handler.deep[Maybe, Option]([A] => (a: A) => Some(a))(
    [A, X] => (m: Maybe[X], k: X => Option[A] ! Pure) => m.value match
      case Some(x) => k(x)
      case None => pure[Option[A], Pure](None))

  /** handle Maybe into Option, wherever it is among the program's effects, the rest of them forwarded. `Some` is
   * answered with the rest of the program; `None` drops it. */
  def run[A, Es[_]](p: A ! Es)(using s: Split[Es, Maybe]): Option[A] ! s.Rest = p.handle(option)

  /**
   * SKIP INSTEAD OF STOP. `run` answers a whole program's absence;
   * these answer it per ELEMENT or per BRANCH, and that is what makes
   * `Maybe` more than `Abort`: an absence is decided by the handler's
   * SCOPE, and a narrow scope drops one element and goes on.
   *
   * `collect` runs `f` once per element, each under its own `run`, and
   * keeps the elements that were there — Haskell's `mapMaybe`, Scala's
   * `collect`, with an effectful `f`. Effects of `f` other than Maybe
   * are forwarded in element order, including those of an element that
   * then turned out empty.
   */
  def collect[X, B, Es[_]](xs: Iterable[X])(f: X => B ! Es)(using s: Split[Es, Maybe]): Vector[B] ! s.Rest =
    foldM[X, Vector[B], s.Rest](xs)(Vector.empty[B])((v, x) =>
      run[B, Es](f(x))(using s).map(o => if o.isEmpty then v else v :+ o.get))

  /**
   * An absence as a PRUNED BRANCH: every `Maybe` becomes a `Choose` —
   * `Some(x)` one alternative, `None` none — so under `runChoice` a
   * branch that found nothing dies and the others go on, and the
   * answers are exactly the branches where everything was there.
   *
   * The row being pruned usually carries `Choose` ALREADY (that is why
   * it is pruned): then the new alternatives join the ones already
   * there, at that `Choose`'s place — the rest's effects as they are
   * (`Join`: `Choose` found among them); else `Choose` is added. Each
   * `Maybe` is TRANSLATED to its `Choose` where it is performed (the
   * kernel's `Translating`).
   */
  def prune[A, Es[_]](p: A ! Es)(using s: Split[Es, Maybe], j: Join[Choose, s.Rest]): A ! j.Out =
    val q: A ! Choose + s.Rest =
      p.translate[Maybe, Choose]([X] => (m: Maybe[X]) => effect[Choose, X](Choose(m.value.toList)))
    // the same program: the effects joined are the compiler's
    Run.of[A, j.Out](Run.kernel(q))

  extension [A, Es[_]](p: A ! Es)
    /** where p found nothing, try q — a handler installed over p alone,
     * so the effects come out unchanged (Throws' `orElse`, for absence) */
    def orElse(using s: Split[Es, Maybe], w: Within[s.Rest, Es])(q: => A ! Es): A ! Es =
      run[A, Es](p)(using s).at[Es].flatMap:
        case Some(a) => pure[A, Es](a)
        case None => q

    /** the value, or the default where p found nothing */
    def getOrElse(a: => A)(using s: Split[Es, Maybe]): A ! s.Rest = run[A, Es](p)(using s).map(_.getOrElse(a))
