package okay.kont

import okay.Freer
import okay.Freer.{Bind, Delay, Diag, Inject, Return}
import scala.annotation.tailrec

/**
 * THE MACHINE'S CONTINUATION STACK AS A FREER (specs/freer-kont.md,
 * the probe of `freer-kont-frames-probe`, 2026-09-30).
 *
 * A type-aligned list of frames that is ITSELF a continuation: a
 * `Frames[G, A, S, T, Z]` is an `A => Freer[G, S, T, Z]`, and its
 * indexes are joined exactly as `Bind` joins them — `Frame`'s `f`
 * consumes `T` and produces `S2`, `rest` goes on from `S2` to `S` —
 * because the stack IS the `Bind` spine with a hole at its leftmost
 * leaf, read bottom-up; `End` is `Return`'s diagonal. That is what
 * lets the machine hand the stack out as the continuation of a head
 * form `Bind(Inject(e), fs)` with no allocation, and take it back by
 * SPLICING when it meets `Bind(a, ks: Frames)`: `apply` builds that
 * node and nothing else, so resuming a captured continuation is lazy
 * and runs in the machine's own loop — 100 000 clauses each calling
 * `k` nest no JVM frame (TestKont, the generator).
 *
 * The idiom is `Freer.Mapped`'s: a function that knows what it is,
 * sitting in a `Bind` as an ordinary `A => Freer`, callable as one,
 * and taken apart by the one loop that knows the class.
 */
enum Frames[G[_, _, +_], A, S, T, Z] extends (A => Freer[G, S, T, Z]):
  /** the empty continuation: the identity, on the diagonal like `Return` */
  case End[G[_, _, +_], A, S]() extends Frames[G, A, S, S, A]

  /** a frame and the rest: `f` consumes `T`, produces `S2`; the rest
   * goes from `S2` to `S` — `Bind`'s join, on the stack */
  case Frame[G[_, _, +_], A, S, S2, T, Y, Z](f: A => Freer[G, S2, T, Y],
                                              rest: Frames[G, Y, S, S2, Z]) extends Frames[G, A, S, T, Z]

  /** a lazy node, never a run: the machine splices it (see `Machine.run`) */
  def apply(a: A): Freer[G, S, T, Z] = this match
    case End() => Return(a)
    case _ => Bind(Return(a), this)

object Frames:
  /**
   * THE ONE CLASS TEST of the stack, and the claim it makes: a `Frames`
   * sitting as a `Bind`'s continuation is typed by that `Bind` — the
   * function type `A => Freer[G, S, T, Z]` IS its type, so the test on
   * the class is the whole test (`Free.Bind`'s constant claim, in the
   * same spirit). `null` when it is any other function.
   */
  def as[G[_, _, +_], A, S, T, Z](f: A => Freer[G, S, T, Z]): Frames[G, A, S, T, Z] = f match
    case fs: Frames[?, ?, ?, ?, ?] => fs.asInstanceOf[Frames[G, A, S, T, Z]]
    case _ => null

/**
 * THE DELIMITER IS A FRAME. `$` (Materzok & Biernacki, APLAS 2012) is
 * `ret` on the stack, so a body that returns runs `ret` by the
 * ordinary pop — the `($v)` rule needs no case in the loop — and a
 * `shift0` cuts the stack at the first frame holding one of these, so
 * its `k` carries `ret` — the `($/S0)` rule. Answer-type modification
 * is `Bind`'s: body `Freer[G, T, R, A]`, `ret: A => Freer[G, S, T, Y]`,
 * the delimited program `Freer[G, S, R, Y]` — today's `Cont.run(c)(k)`
 * with a lazy `ret`.
 */
final class Dollar[G[_, _, +_], S, Y, A, T](val prompt: Prompt[S, Y], val ret: A => Freer[G, S, T, Y])
  extends (A => Freer[G, S, T, Y]):
  def apply(a: A): Freer[G, S, T, Y] = ret(a)

object Dollar:
  /** the same one test as `Frames.as`: a `Dollar` in a frame typed
   * `A => Freer[G, S, T, Y]` was built by `dollar` at exactly those types */
  def as[G[_, _, +_], S, Y, A, T](f: A => Freer[G, S, T, Y]): Dollar[G, S, Y, A, T] = f match
    case d: Dollar[?, ?, ?, ?, ?] => d.asInstanceOf[Dollar[G, S, Y, A, T]]
    case _ => null

/**
 * A delimiter's identity and its two types: `S`, the index the frames
 * under it expect, and `Y`, what it answers. Identity is the object:
 * `same` answers the two equalities by `eq`, the one place a prompt's
 * types are claimed — a prompt is built once and never copied, so two
 * references that are `eq` were made by the same `apply` at the same
 * types (Delim's `===` on its `Prompt` makes the same claim).
 */
final class Prompt[S, Y](val label: String):
  def same[S2, Y2](q: Prompt[S2, Y2]): Option[(S =:= S2, Y =:= Y2)] =
    if this eq q then Some((summon[S =:= S].asInstanceOf[S =:= S2], summon[Y =:= Y].asInstanceOf[Y =:= Y2])) else None
  override def toString: String = label

/**
 * THE ONE OPERATION. `f` takes the stack up to and including the
 * delimiter named `p` — a `Frames[Row[F], X, S, T, Y]`, from the
 * operation's value `X` to the delimiter's answer `Y` — and answers a
 * program that stands in the delimiter's place: `Freer[Row[F], S, R,
 * Y]`. `(T, R)` are the leaf's own indexes (what `k` consumes, what
 * the body answers); `(S, Y)` are the prompt's.
 */
enum Cont0[F[_, _, +_], T, R, +X]:
  case Shift0[F[_, _, +_], S, Y, T, R, X](p: Prompt[S, Y],
                                          f: Frames[Row[F], X, S, T, Y] => Freer[Row[F], S, R, Y],
                                          at: String) extends Cont0[F, T, R, X]

/** the row: `Cont0` beside any indexed signature `F`; the effect tree's
 * signature is `Freer.Lift[Fx]` here */
type Row[F[_, _, +_]] = [T, R, X] =>> Cont0[F, T, R, X] | F[T, R, X]

/** a capture named a prompt that is not on the stack */
final class NoDollar(val at: String, val wanted: String, val installed: List[String])
  extends RuntimeException(
    s"$at: shift0 to '$wanted', which is not on the stack; installed, innermost first: ${installed.mkString("[", ", ", "]")}")

object Cont0:
  /** `ret $ body`: the body under the delimiter, `ret` as its frame */
  def dollar[F[_, _, +_], S, Y, A, T, R](p: Prompt[S, Y])(ret: A => Freer[Row[F], S, T, Y])(body: Freer[Row[F], T, R, A]): Freer[Row[F], S, R, Y] =
    Bind(body, new Dollar[Row[F], S, Y, A, T](p, ret))

  /** `$` with `ret = pure`: reset, whose `S = A` requirement is `Return`'s diagonal */
  def reset[F[_, _, +_], S, R, A](p: Prompt[S, A])(body: Freer[Row[F], S, R, A]): Freer[Row[F], S, R, A] =
    dollar[F, S, A, A, S, R](p)(Return[Row[F], S, A](_))(body)

  def shift0[F[_, _, +_], S, Y, T, R, X](p: Prompt[S, Y])(f: Frames[Row[F], X, S, T, Y] => Freer[Row[F], S, R, Y])(using at: okay.At): Freer[Row[F], T, R, X] =
    Inject[Row[F], T, R, X](Cont0.Shift0[F, S, Y, T, R, X](p, f, at.where))

  /** `shift`: the body under a fresh plain delimiter; `k` still carries `ret` */
  def shift[F[_, _, +_], S, Y, T, R, X](p: Prompt[S, Y])(f: Frames[Row[F], X, S, T, Y] => Freer[Row[F], S, R, Y])(using okay.At): Freer[Row[F], T, R, X] =
    shift0[F, S, Y, T, R, X](p)(k => reset[F, S, R, Y](p)(f(k)))

  def prompt[S, Y](using at: okay.At): Prompt[S, Y] = new Prompt[S, Y](s"prompt @ ${at.where}")

/**
 * The reversed stack: the same type-aligned discipline, outermost
 * first. A cut walks the stack down to the delimiter building one of
 * these and links it onto `End`; a splice reverses a segment and links
 * it onto the current stack. Both walks are `@tailrec`; both are
 * O(|segment|) and amortised free — every frame copied is about to run.
 */
private enum Rev[G[_, _, +_], A, T, S2, Y]:
  case Nil[G[_, _, +_], A, T]() extends Rev[G, A, T, T, A]
  case Snoc[G[_, _, +_], A, T, S3, S2, Y0, Y](prev: Rev[G, A, T, S3, Y0], f: Y0 => Freer[G, S2, S3, Y]) extends Rev[G, A, T, S2, Y]

private object Rev:
  @tailrec def link[G[_, _, +_], A, S, T, S2, Y, Z](rev: Rev[G, A, T, S2, Y], fs: Frames[G, Y, S, S2, Z]): Frames[G, A, S, T, Z] = rev match
    case Nil() => fs
    case Snoc(prev, f) => link(prev, Frames.Frame(f, fs))

  @tailrec def reverse[G[_, _, +_], A, S2, T0, T, X, Y](ks: Frames[G, X, S2, T, Y], acc: Rev[G, A, T0, T, X]): Rev[G, A, T0, S2, Y] = ks match
    case Frames.End() => acc
    case Frames.Frame(f, rest) => reverse(rest, Snoc(acc, f))

  /** `ks ++ fs`: the segment on top of the stack */
  def splice[G[_, _, +_], A, S, T, S2, Y, Z](ks: Frames[G, A, S2, T, Y], fs: Frames[G, Y, S, S2, Z]): Frames[G, A, S, T, Z] = fs match
    case Frames.End() => ks
    case _ => link(reverse(ks, Nil[G, A, T]()), fs)

object Machine:
  /**
   * The one loop: run `p` to a head form — `Return(x)`, or `Bind(Inject(e), fs)`
   * for the first operation no delimiter on the stack answers, `fs` the
   * stack as the continuation. `S0`, `R`, `Z` are the run's; every arm
   * is typed by GADT refinement of the two registers.
   */
  def run[F[_, _, +_], S0, R, Z](p: Freer[Row[F], S0, R, Z]): Freer[Row[F], S0, R, Z] =
    type G = Row[F]

    final class Next[X, T](val focus: Freer[G, T, R, X], val fs: Frames[G, X, S0, T, Z])

    @tailrec def installed(fs: Frames[G, ?, S0, ?, Z], acc: List[String]): List[String] = fs match
      case fr: Frames.Frame[G, ?, S0, ?, ?, ?, Z] @unchecked => fr.f match
        case d: Dollar[?, ?, ?, ?, ?] => installed(fr.rest, d.prompt.label :: acc)
        case _ => installed(fr.rest, acc)
      case _ => acc.reverse

    /** cut the stack at the frame holding `sh.p`'s `Dollar`: `k` is the
     * segment with it, the body takes the delimiter's place */
    @tailrec def cut[X, S, Y, T, T2, C](sh: Cont0.Shift0[F, S, Y, T, R, X], all: Frames[G, X, S0, T, Z], fs: Frames[G, C, S0, T2, Z], rev: Rev[G, X, T, T2, C]): Next[?, ?] = fs match
      case Frames.End() => throw NoDollar(sh.at, sh.p.label, installed(all, Nil))
      case fr: Frames.Frame[G, C, S0, s2, T2, y2, Z] => Dollar.as(fr.f) match
        case null => cut(sh, all, fr.rest, Rev.Snoc(rev, fr.f))
        case d => sh.p.same(d.prompt) match
          case Some((es, ey)) =>
            val seg: Frames[G, X, s2, T, y2] = Rev.link(Rev.Snoc(rev, d), Frames.End[G, y2, s2]())
            val k: Frames[G, X, S, T, Y] = ey.flip.liftCo[[y] =>> Frames[G, X, S, T, y]](es.flip.liftCo[[s] =>> Frames[G, X, s, T, y2]](seg))
            val body: Freer[G, s2, R, y2] = ey.liftCo[[y] =>> Freer[G, s2, R, y]](es.liftCo[[s] =>> Freer[G, s, R, Y]](sh.f(k)))
            Next(body, fr.rest)
          case None => cut(sh, all, fr.rest, Rev.Snoc(rev, fr.f))

    @tailrec def loop[X, T](focus: Freer[G, T, R, X], fs: Frames[G, X, S0, T, Z]): Freer[G, S0, R, Z] = focus match
      case b: Bind[G, T, t2, R, x0, X] => Frames.as(b.f) match
        // a stack as the continuation: a resumed `k`, or the head form
        // handed out and fed back; spliced, never wrapped
        case null => fs match
          // the head form already — an operation nobody here answers,
          // one frame, empty stack: hand it back as it is, no push
          case _: Frames.End[G, X, S0] => b.a match
            case Inject(e) if !e.isInstanceOf[Cont0.Shift0[?, ?, ?, ?, ?, ?]] => focus
            case a => loop[x0, t2](a, Frames.Frame(b.f, fs))
          case _ => loop[x0, t2](b.a, Frames.Frame(b.f, fs))
        case ks => loop[x0, t2](b.a, Rev.splice(ks, fs))
      case r: Return[G, R, X] => fs match
        case _: Frames.End[G, X, S0] => focus
        case fr: Frames.Frame[G, X, S0, s2, T, ?, Z] => loop(fr.f(r.a), fr.rest)
      case d: Delay[G, T, R, X] => loop(d.thunk(), fs)
      case Inject(e) => e match
        case sh: Cont0.Shift0[F, ?, ?, T, R, X] @unchecked => cut(sh, fs, fs, Rev.Nil[G, X, T]()) match
          case n: Next[x, t] => loop[x, t](n.focus, n.fs)
        case _ => Bind(focus, fs)
      case Diag(e) => e match
        case sh: Cont0.Shift0[F, ?, ?, T, R, X] @unchecked => cut(sh, fs, fs, Rev.Nil[G, X, T]()) match
          case n: Next[x, t] => loop[x, t](n.focus, n.fs)
        case _ => Bind(focus, fs)

    loop(p, Frames.End[G, Z, S0]())
