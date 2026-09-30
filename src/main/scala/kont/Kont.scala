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
 * SPLICING when it meets one — as a `Bind`'s continuation, or as the
 * `Resume` thunk of the `Delay` that `apply` answers — so resuming a
 * captured continuation is lazy and runs in the machine's own loop:
 * 100 000 clauses each calling `k` nest no JVM frame (TestKont, the
 * generator); forced by anyone else, that thunk runs the machine.
 *
 * The idiom is `Freer.Mapped`'s: a function that knows what it is,
 * sitting in a `Bind` as an ordinary `A => Freer`, callable as one,
 * and taken apart by the one loop that knows the class.
 */
enum Frames[F[_, _, +_], A, S, T, Z] extends (A => Freer[Row[F], S, T, Z]):
  /** the empty continuation: the identity, on the diagonal like `Return` */
  case End[F[_, _, +_], A, S]() extends Frames[F, A, S, S, A]

  /** a frame and the rest: `f` consumes `T`, produces `S2`; the rest
   * goes from `S2` to `S` — `Bind`'s join, on the stack */
  case Frame[F[_, _, +_], A, S, S2, T, Y, Z](f: A => Freer[Row[F], S2, T, Y],
                                              rest: Frames[F, Y, S, S2, Z]) extends Frames[F, A, S, T, Z]

  /**
   * THE DELIMITER IS A FRAME. `$` (Materzok & Biernacki, APLAS 2012) is
   * `ret` on the stack with the prompt beside it, so a body that
   * returns runs `ret` by the ordinary pop — the `($v)` rule needs no
   * case in the loop — and a `shift0` cuts the stack at the first
   * `Reset` naming its prompt, so its `k` carries `ret` — the `($/S0)`
   * rule. Answer-type modification is `Bind`'s: body `Freer[G, T, R,
   * A]`, `ret: A => Freer[G, S2, T, Y]`, the delimited program
   * `Freer[G, S2, R, Y]` — today's `Cont.run(c)(k)` with a lazy `ret`.
   * `reset` is this with `ret = pure`. A program asks for one with the
   * OPERATION `Cont0.Reset0`, never by building this frame: an
   * operation passes through a handler loop between the program and
   * the machine (the loop forwards what it does not own), a frame does
   * not — `Freer.resume`'s rotation folds a `Bind(body, Reset(..))`
   * into a closure, and a cut on the machine's stack would not see it.
   */
  case Reset[F[_, _, +_], A, S, S2, T, Y, Z](p: Prompt[S2, Y], ret: A => Freer[Row[F], S2, T, Y],
                                              rest: Frames[F, Y, S, S2, Z]) extends Frames[F, A, S, T, Z]

  /**
   * THE CONTINUATION CARRIES ITS OWN INTERPRETER (operator, 2026-09-30).
   * Applied as a plain function — by ANY interpreter of the tree,
   * `Freer.resume`'s rotation included — the stack answers a `Delay`
   * whose thunk runs the machine on itself with `a` at its top: forced
   * by an outer loop it runs the frames WITH the `Cont0` inside them
   * handled and hands back the next head form, one JVM call that
   * returns before the next. So `Freer` needs to know nothing of frames
   * and a loop outside never sees one. The machine of `Frames.run`
   * never forces it: meeting `Delay(r: Resume)` it SPLICES `r.fs` onto
   * its stack — the lazy node a clause's `k(x)` must be, so 100 000
   * resumptions from 100 000 clauses nest no frame (TestKont).
   */
  def apply(a: A): Freer[Row[F], S, T, Z] = this match
    case End() => Return(a)
    case _ => Delay(Frames.Resume(a, this))

object Frames:
  /** a resumption: the value and the stack it enters, as the thunk of a
   * `Delay` — run by whoever forces it, spliced by the machine */
  final class Resume[F[_, _, +_], A, S, T, Z](val a: A, val fs: Frames[F, A, S, T, Z]) extends (() => Freer[Row[F], S, T, Z]):
    def apply(): Freer[Row[F], S, T, Z] = Frames.run[F, S, T, Z](Bind(Return[Row[F], T, A](a), fs))

  /**
   * THE TWO CLASS TESTS of the stack, and the claim they make: a
   * `Frames` sitting as a `Bind`'s continuation, or a `Resume` as a
   * `Delay`'s thunk, is typed by that node — the function type IS its
   * type, so the test on the class is the whole test (`Free.Bind`'s
   * constant claim, in the same spirit). `null` for any other function.
   */
  def as[F[_, _, +_], A, S, T, Z](f: A => Freer[Row[F], S, T, Z]): Frames[F, A, S, T, Z] = f match
    case fs: Frames[?, ?, ?, ?, ?] => fs.asInstanceOf[Frames[F, A, S, T, Z]]
    case _ => null

  def resume[F[_, _, +_], S, T, Z](t: () => Freer[Row[F], S, T, Z]): Resume[F, ?, S, T, Z] = t match
    case r: Resume[?, ?, ?, ?, ?] => r.asInstanceOf[Resume[F, ?, S, T, Z]]
    case _ => null

  /**
   * The one loop: run `p` to a head form — `Return(x)`, or `Bind(Inject(e), k)`
   * for the first operation no delimiter on the stack answers, `k` the
   * stack itself (which re-enters this loop when applied). `S0`, `R`, `Z` are the run's; every arm
   * is typed by GADT refinement of the two registers.
   */
  def run[F[_, _, +_], S0, R, Z](p: Freer[Row[F], S0, R, Z]): Freer[Row[F], S0, R, Z] =
    type G = Row[F]

    final class Next[X, T](val focus: Freer[G, T, R, X], val fs: Frames[F, X, S0, T, Z])

    @tailrec def installed(fs: Frames[F, ?, ?, ?, ?], acc: List[String]): List[String] = fs match
      case Frames.Frame(_, rest) => installed(rest, acc)
      case Frames.Reset(p, _, rest) => installed(rest, p.label :: acc)
      case _ => acc.reverse

    /** cut the stack at the `Reset` naming `sh.p`: `k` is the
     * segment with it, the body takes the delimiter's place */
    @tailrec def cut[X, S, Y, T, T2, C](sh: Cont0.Shift0[F, S, Y, T, R, X], all: Frames[F, X, S0, T, Z], fs: Frames[F, C, S0, T2, Z], rev: Rev[F, X, T, T2, C]): Next[?, ?] = fs match
      case Frames.End() => throw NoReset(sh.at, sh.p.label, installed(all, Nil))
      case fr: Frames.Frame[F, C, S0, ?, T2, ?, Z] => cut(sh, all, fr.rest, Rev.Snoc(rev, fr.f))
      case d: Frames.Reset[F, C, S0, s2, T2, y2, Z] => sh.p.same(d.p) match
        case Some((es, ey)) =>
          // the segment WITH the delimiter: `k` carries `ret` (the $/S0 rule)
          val seg: Frames[F, X, s2, T, y2] = Rev.link(Rev.SnocReset(rev, d.p, d.ret), Frames.End[F, y2, s2]())
          val k: Frames[F, X, S, T, Y] = ey.flip.liftCo[[y] =>> Frames[F, X, S, T, y]](es.flip.liftCo[[s] =>> Frames[F, X, s, T, y2]](seg))
          val body: Freer[G, s2, R, y2] = ey.liftCo[[y] =>> Freer[G, s2, R, y]](es.liftCo[[s] =>> Freer[G, s, R, Y]](sh.f(k)))
          Next(body, d.rest)
        case None => cut(sh, all, d.rest, Rev.SnocReset(rev, d.p, d.ret))

    @tailrec def loop[X, T](focus: Freer[G, T, R, X], fs: Frames[F, X, S0, T, Z]): Freer[G, S0, R, Z] = focus match
      case b: Bind[G, T, t2, R, x0, X] => Frames.as(b.f) match
        // a stack as the continuation: a resumed `k`, or the head form
        // handed out and fed back; spliced, never wrapped
        case null => b.a match
          // a value under a Bind: apply, no frame — what `Freer.resume`'s
          // `Bind(Return(a), f) => f(a)` does, and a right-nested chain is
          // nothing else (measured: +24 B and 1.26x a step without this)
          case r: Return[G, R, x0] => loop[X, T](b.f(r.a), fs)
          case _ => fs match
          // the head form already — an operation nobody here answers,
          // one frame, empty stack: hand it back as it is, no push
            case _: Frames.End[F, X, S0] @unchecked => b.a match
              case Inject(e) if !e.isInstanceOf[Cont0[?, ?, ?, ?]] => focus
              case a => loop[x0, t2](a, Frames.Frame(b.f, fs))
            case _ => loop[x0, t2](b.a, Frames.Frame(b.f, fs))
        case ks => loop[x0, t2](b.a, Rev.splice(ks, fs))
      case r: Return[G, R, X] => fs match
        case _: Frames.End[F, X, S0] @unchecked => focus
        case fr: Frames.Frame[F, X, S0, s2, T, ?, Z] => loop(fr.f(r.a), fr.rest)
        // the ($v) rule: a delimiter is popped like any frame
        case d: Frames.Reset[F, X, S0, s2, T, ?, Z] => loop(d.ret(r.a), d.rest)
      case d: Delay[G, T, R, X] => Frames.resume[F, T, R, X](d.thunk) match
        // a resumption: the value at the top of its stack, spliced — never forced
        case null => loop(d.thunk(), fs)
        case r: Frames.Resume[F, a, T, R, X] => loop[a, R](Return(r.a), Rev.splice(r.fs, fs))
      case Inject(e) => e match
        // the delimiter, asked for as an operation, becomes a frame
        case rs: Cont0.Reset0[F, T, X, a, t, R] @unchecked => loop[a, t](rs.body, Frames.Reset(rs.p, rs.ret, fs))
        case sh: Cont0.Shift0[F, ?, ?, T, R, X] @unchecked => cut(sh, fs, fs, Rev.Nil[F, X, T]()) match
          case n: Next[x, t] => loop[x, t](n.focus, n.fs)
        case _ => Bind(focus, fs)
      case Diag(e) => e match
        case rs: Cont0.Reset0[F, T, X, a, t, R] @unchecked => loop[a, t](rs.body, Frames.Reset(rs.p, rs.ret, fs))
        case sh: Cont0.Shift0[F, ?, ?, T, R, X] @unchecked => cut(sh, fs, fs, Rev.Nil[F, X, T]()) match
          case n: Next[x, t] => loop[x, t](n.focus, n.fs)
        case _ => Bind(focus, fs)

    loop(p, Frames.End[F, Z, S0]())

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
 * THE TWO OPERATIONS: the delimiter and the capture. `Shift0`'s `f`
 * takes the stack up to and including the
 * delimiter named `p` — a `Frames[F, X, S, T, Y]`, from the
 * operation's value `X` through the `Reset` to its answer `Y` — and answers a
 * program that stands in the delimiter's place: `Freer[Row[F], S, R,
 * Y]`. `(T, R)` are the leaf's own indexes (what `k` consumes, what
 * the body answers); `(S, Y)` are the prompt's.
 */
enum Cont0[F[_, _, +_], T, R, +X]:
  /** `ret $ body`: the body at `T`, `ret` modifies `T` to `S`, the
   * delimiter answers `Y`; the machine turns it into a `Frames.Reset`
   * on ITS stack — an operation reaches the machine through any
   * handler loop in between, which is why the delimiter is asked for
   * this way and not built as a frame */
  case Reset0[F[_, _, +_], S, Y, A, T, R](p: Prompt[S, Y],
                                          ret: A => Freer[Row[F], S, T, Y],
                                          body: Freer[Row[F], T, R, A]) extends Cont0[F, S, R, Y]
  case Shift0[F[_, _, +_], S, Y, T, R, X](p: Prompt[S, Y],
                                          f: Frames[F, X, S, T, Y] => Freer[Row[F], S, R, Y],
                                          at: String) extends Cont0[F, T, R, X]

/** the row: `Cont0` beside any indexed signature `F`; the effect tree's
 * signature is `Freer.Lift[Fx]` here */
type Row[F[_, _, +_]] = [T, R, X] =>> Cont0[F, T, R, X] | F[T, R, X]

/** a capture named a prompt that is not on the stack */
final class NoReset(val at: String, val wanted: String, val installed: List[String])
  extends RuntimeException(
    s"$at: shift0 to '$wanted', which is not on the stack; installed, innermost first: ${installed.mkString("[", ", ", "]")}")

object Cont0:
  /** `ret $ body`: the body under the delimiter — an operation, so it
   * reaches the machine through any handler loop between them */
  def dollar[F[_, _, +_], S, Y, A, T, R](p: Prompt[S, Y])(ret: A => Freer[Row[F], S, T, Y])(body: Freer[Row[F], T, R, A]): Freer[Row[F], S, R, Y] =
    Inject[Row[F], S, R, Y](Cont0.Reset0[F, S, Y, A, T, R](p, ret, body))

  /** `$` with `ret = pure`: reset, whose `S = A` requirement is `Return`'s diagonal */
  def reset[F[_, _, +_], S, R, A](p: Prompt[S, A])(body: Freer[Row[F], S, R, A]): Freer[Row[F], S, R, A] =
    dollar[F, S, A, A, S, R](p)(Return[Row[F], S, A](_))(body)

  def shift0[F[_, _, +_], S, Y, T, R, X](p: Prompt[S, Y])(f: Frames[F, X, S, T, Y] => Freer[Row[F], S, R, Y])(using at: okay.At): Freer[Row[F], T, R, X] =
    Inject[Row[F], T, R, X](Cont0.Shift0[F, S, Y, T, R, X](p, f, at.where))

  /** `shift`: the body under a fresh plain delimiter; `k` still carries `ret` */
  def shift[F[_, _, +_], S, Y, T, R, X](p: Prompt[S, Y])(f: Frames[F, X, S, T, Y] => Freer[Row[F], S, R, Y])(using okay.At): Freer[Row[F], T, R, X] =
    shift0[F, S, Y, T, R, X](p)(k => reset[F, S, R, Y](p)(f(k)))

  def prompt[S, Y](using at: okay.At): Prompt[S, Y] = new Prompt[S, Y](s"prompt @ ${at.where}")

/**
 * The reversed stack: the same type-aligned discipline, outermost
 * first. A cut walks the stack down to the delimiter building one of
 * these and links it onto `End`; a splice reverses a segment and links
 * it onto the current stack. Both walks are `@tailrec`; both are
 * O(|segment|) and amortised free — every frame copied is about to run.
 */
private enum Rev[F[_, _, +_], A, T, S2, Y]:
  case Nil[F[_, _, +_], A, T]() extends Rev[F, A, T, T, A]
  case Snoc[F[_, _, +_], A, T, S3, S2, Y0, Y](prev: Rev[F, A, T, S3, Y0], f: Y0 => Freer[Row[F], S2, S3, Y]) extends Rev[F, A, T, S2, Y]
  case SnocReset[F[_, _, +_], A, T, S3, S2, Y0, Y](prev: Rev[F, A, T, S3, Y0], p: Prompt[S2, Y], ret: Y0 => Freer[Row[F], S2, S3, Y]) extends Rev[F, A, T, S2, Y]

private object Rev:
  @tailrec def link[F[_, _, +_], A, S, T, S2, Y, Z](rev: Rev[F, A, T, S2, Y], fs: Frames[F, Y, S, S2, Z]): Frames[F, A, S, T, Z] = rev match
    case Nil() => fs
    case Snoc(prev, f) => link(prev, Frames.Frame(f, fs))
    case SnocReset(prev, p, ret) => link(prev, Frames.Reset(p, ret, fs))

  @tailrec def reverse[F[_, _, +_], A, S2, T0, T, X, Y](ks: Frames[F, X, S2, T, Y], acc: Rev[F, A, T0, T, X]): Rev[F, A, T0, S2, Y] = ks match
    case Frames.End() => acc
    case Frames.Frame(f, rest) => reverse(rest, Snoc(acc, f))
    case Frames.Reset(p, ret, rest) => reverse(rest, SnocReset(acc, p, ret))

  /** `ks ++ fs`: the segment on top of the stack */
  def splice[F[_, _, +_], A, S, T, S2, Y, Z](ks: Frames[F, A, S2, T, Y], fs: Frames[F, Y, S, S2, Z]): Frames[F, A, S, T, Z] = fs match
    case Frames.End() => ks
    case _ => link(reverse(ks, Nil[F, A, T]()), fs)
