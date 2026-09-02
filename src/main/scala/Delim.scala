package okay

import okay.!.*
import scala.annotation.tailrec

/**
 * Delimited control as an EFFECT — multi-prompt, in the shape of
 * Dybvig, Peyton Jones and Sabry's "A monadic framework for delimited
 * continuations" (2007): a prompt is a first-class tag carrying the
 * delimiter's answer type, `push` installs one, and `shift` captures
 * up to a NAMED prompt rather than to the nearest one.
 *
 * Two design points worth stating, because both were arrived at the
 * hard way.
 *
 * FIRST: `push` (reset) is an operation, not a handler application.
 * The reason is not symmetry with shift — it is that capturing across
 * an intervening delimiter is the whole point of multi-prompt, and
 * nested handlers cannot do it: an inner handler forwarding a shift
 * it does not own would forward it OPAQUELY, leaving its own frames
 * out of the captured continuation. One machine has to own the whole
 * prompt stack, so both push and shift must reach it as operations.
 *
 * SECOND: tags are what make several answer types coexist in ONE
 * effect row. A signature parameterised by its answer (`Control % R`)
 * would give two prompts of different answer types the same runtime
 * class, and union splitting here is by class — the two would be
 * indistinguishable. With the answer type riding inside the prompt,
 * there is a single `Delim` signature and the tags keep them apart.
 *
 * The price, stated plainly: the operations' payloads are programs in
 * the same row, which a single-parameter signature cannot express in
 * types, so they are erased here and re-typed inside the machine. The
 * smart constructors below are the only way to build these
 * operations, which makes the casts sealed module invariants — the
 * same discipline as Writer's phantom equation, and documented in the
 * same spirit rather than hidden.
 */

/** a delimiter's identity AND its answer type; identity is the tag */
final class Prompt[R]

enum Delim[+A]:
  /** install a delimiter and run the body under it (reset) */
  case Push[R](prompt: Prompt[R], body: Any) extends Delim[R]

  /**
   * Capture the continuation up to THIS prompt. The whole classic
   * family is two independent bits, so it is one operation with two
   * flags rather than four cases:
   *
   *   underPrompt — does f's body run with the delimiter still
   *                 installed? (shift, control: yes; the 0-variants
   *                 consume it)
   *   delimitK    — does invoking the captured continuation
   *                 re-install the delimiter? (shift, shift0: yes;
   *                 the control-variants hand back a bare segment)
   *
   *   reset(E[shift    f]) = reset (f (x => reset E[x]))
   *   reset(E[control  f]) = reset (f (x =>       E[x]))
   *   reset(E[shift0   f]) =        f (x => reset E[x])
   *   reset(E[control0 f]) =        f (x =>       E[x])
   */
  case Capture[R, A](prompt: Prompt[R], f: Any,
                     underPrompt: Boolean, delimitK: Boolean) extends Delim[A]

/** a shift naming a prompt that is not installed */
final class NoPrompt extends RuntimeException(
  "shift to a prompt that is not on the stack")

object Delim {

  /** a fresh delimiter tag */
  def prompt[R]: Prompt[R] = new Prompt[R]

  /** run the body under the delimiter — reset, as an operation */
  def push[R, F[+_]](p: Prompt[R])(body: R ! (Delim + F)): R ! (Delim + F) =
    effect(Push(p, body))

  /**
   * Capture the continuation up to `p` and hand it to `f`. The
   * continuation is a PROGRAM-valued function, so `f` may perform
   * effects around it, invoke it many times, or drop it entirely
   * (which is an early exit).
   *
   * `shift`: the body runs under the delimiter and the continuation
   * re-installs it — the variant most people mean, and the one that
   * lets a captured continuation shift again.
   */
  def shift[R, A, F[+_]](p: Prompt[R])
                        (f: (A => R ! (Delim + F)) => R ! (Delim + F)): A ! (Delim + F) =
    effect(Capture(p, f, underPrompt = true, delimitK = true))

  /** the body CONSUMES the delimiter (a further shift to `p` escapes
   * outward), the continuation still re-installs it */
  def shift0[R, A, F[+_]](p: Prompt[R])
                         (f: (A => R ! (Delim + F)) => R ! (Delim + F)): A ! (Delim + F) =
    effect(Capture(p, f, underPrompt = false, delimitK = true))

  /** the body runs under the delimiter, the continuation does NOT
   * re-install it — a bare segment, spliced where it is invoked */
  def control[R, A, F[+_]](p: Prompt[R])
                          (f: (A => R ! (Delim + F)) => R ! (Delim + F)): A ! (Delim + F) =
    effect(Capture(p, f, underPrompt = true, delimitK = false))

  /** neither: the delimiter is consumed and the continuation is bare */
  def control0[R, A, F[+_]](p: Prompt[R])
                           (f: (A => R ! (Delim + F)) => R ! (Delim + F)): A ! (Delim + F) =
    effect(Capture(p, f, underPrompt = false, delimitK = false))

  /** the common shape: a fresh prompt, a block under it, run */
  def reset[R, F[+_]](body: Prompt[R] => R ! (Delim + F)): R ! F =
    val p = prompt[R]
    run(push(p)(body(p)))

  /** a prompt is its own typed token: the same prompt has the same
   * answer type — the witness the machine uses to split its stack */
  given Same[Prompt] = Same.byIdentity

  /**
   * The machine's continuation, TYPED: a chain from the current
   * answer A to the run's answer Z. `K` is one Bind's continuation
   * (types chain through it), `Mark` a delimiter carrying its
   * prompt — and so the answer type of the program under it.
   */
  private enum Segs[F[+_], A, Z]:
    case Done[F[+_], Z]() extends Segs[F, Z, Z]
    case K[F[+_], X, Y, Z](f: X => Y ! (Delim + F), rest: Segs[F, Y, Z]) extends Segs[F, X, Z]
    case Mark[F[+_], X, Z](p: Prompt[X], rest: Segs[F, X, Z]) extends Segs[F, X, Z]

  /** the stack cut at a prompt: what was captured (a chain ending
   * where the prompt's mark was) and what lies outside it */
  private final case class Cut[F[+_], A, P, Z](captured: Segs[F, A, P], outer: Segs[F, P, Z])

  /** the machine's state between steps: a program and the stack it
   * continues into */
  private final case class Next[F[+_], A, Z](prog: A ! (Delim + F), kont: Segs[F, A, Z])

  /**
   * The machine. Our Bind nodes already reify continuations as plain
   * functions, so the freer tree IS the control stack — the machine
   * only has to keep the segment chain and the prompt markers in it.
   * A captured segment is turned back INTO A PROGRAM (reify below),
   * which is why the continuation is an ordinary value: multi-shot
   * comes for free, and nothing is a closure over interpreter state.
   *
   * Two claims and no other cast (cast-free-delim): a Push's body
   * and a Capture's f are programs in this machine's row, erased at
   * the operation because the row's other half F is not the
   * operation's to name — re-typed here, at their two lines, where F
   * is known. Everything else the chain's types carry.
   */
  def run[R, F[+_]](prog: R ! (Delim + F)): R ! F = {
    type Row = Delim + F
    type Prog[A] = A ! Row

    /** frames back into a program: binds become flatMaps, markers
     * become pushes — the continuation re-installs its delimiter */
    def reify[A, P](segs: Segs[F, A, P], start: Prog[A]): Prog[P] = segs match
      case Segs.Done() => start
      case Segs.K(f, rest) => reify(rest, start.flatMap(f))
      case Segs.Mark(p, rest) => reify(rest, effect[Row, A](Push(p, start)))

    /** cut the chain at the mark of p: the mark's prompt IS p by
     * identity, and Same's witness makes the mark's type P's */
    def split[A, P, Z](kont: Segs[F, A, Z], p: Prompt[P]): Option[Cut[F, A, P, Z]] = kont match
      case Segs.Done() => None
      case Segs.Mark(q, rest) =>
        (q === p) match
          case Some(ev) =>
            Some(Cut(ev.liftCo[[t] =>> Segs[F, A, t]](Segs.Done()), ev.liftCo[[t] =>> Segs[F, t, Z]](rest)))
          case None => split(rest, p).map(c => Cut(Segs.Mark(q, c.captured), c.outer))
      case Segs.K(f, rest) => split(rest, p).map(c => Cut(Segs.K(f, c.captured), c.outer))

    // ONE tail-recursive loop: an earlier version split it into
    // loop/onOp, and mutual recursion is not tail-optimised, so every
    // operation cost frames and a thousand nested captures blew the
    // stack. Merged, only a FOREIGN operation suspends (under a
    // flatMap closure, as State.handle does) and the Delim ops
    // themselves are flat.
    @tailrec def loop(state: Next[F, ?, R]): R ! F = state match
      case n: Next[F, a, R] => (n.prog.resume: @unchecked) match
        case Pure(x) => n.kont match
          case Segs.Done() => okay.pure(x)
          case Segs.K(f, rest) => loop(Next(f(x), rest))
          // the delimited block finished normally: drop its marker
          case Segs.Mark(_, rest) => loop(Next(okay.pure(x), rest))

        case Effect(e) => step(e, n.kont) match
          case Left(answer) => answer
          case Right(next) => loop(next)

        case Bind(Effect(e), k) =>
          step(e, Segs.K(k, n.kont)) match
            case Left(answer) => answer
            case Right(next) => loop(next)

    /** one operation: either the machine is done (Left) or it
     * continues with a new program and stack (Right) */
    def step[X](e: Row[X], kont: Segs[F, X, R]): Either[R ! F, Next[F, ?, R]] =
      <|>[Delim, F](e) match
        case Left(c) => c match
          case pu: Push[r] =>
            // claim 1: the pushed body answers the prompt's r in this
            // row; r is an X (the op's answer), which K carries up
            val body = pu.body.asInstanceOf[Prog[r]]
            Right(Next(body, Segs.Mark(pu.prompt, Segs.K((a: r) => okay.pure[Row, X](a), kont))))

          case cap: Capture[p, a] =>
            split(kont, cap.prompt) match
              case Some(cut) =>
                val k = (v: a) => {
                  val seg = reify(cut.captured, okay.pure[Row, X](v))
                  if cap.delimitK then effect[Row, p](Push(cap.prompt, seg)) else seg
                }
                // claim 2: f takes a continuation into the prompt's
                // answer and gives back a program at it, in this row
                val body = cap.f.asInstanceOf[(a => Prog[p]) => Prog[p]](k)
                // shift/control put the body back under the delimiter;
                // the 0-variants have consumed it
                if cap.underPrompt then Right(Next(effect[Row, p](Push(cap.prompt, body)), cut.outer))
                else Right(Next(body, cut.outer))
              case None => throw NoPrompt()

        // a foreign operation suspends the machine: the residual
        // program performs it and resumes with the same stack
        case Right(g) => Left(Effect(g).flatMap(x => loop(Next(okay.pure(x), kont))))

    loop(Next(prog, Segs.Done()))
  }

  /** abort to a prompt with a value: a shift that drops the
   * continuation (the 0-variant, so the delimiter goes with it) */
  def abort[R, A, F[+_]](p: Prompt[R])(value: R): A ! (Delim + F) =
    shift0[R, A, F](p)(_ => okay.pure(value))
}

/** The class IS the whole identity: Delim has no parameter but its
 * (erased) answer type, so splitting a row on it is a TOTAL test —
 * said once here, rather than as a "cannot be checked at runtime"
 * warning at every use site of a test that is in fact complete. */
given TypeableK[Delim] = typeableK(classOf[Delim[?]])
