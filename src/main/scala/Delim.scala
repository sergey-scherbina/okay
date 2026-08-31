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

  /** one frame of the machine's continuation */
  private enum Seg:
    case K(f: Any => Any)
    case Delimiter(p: Prompt[?])

  /**
   * The machine. Our Bind nodes already reify continuations as plain
   * functions, so the freer tree IS the control stack — the machine
   * only has to keep the segment list and the prompt markers in it.
   * A captured segment is turned back INTO A PROGRAM (reify below),
   * which is why the continuation is an ordinary value: multi-shot
   * comes for free, and nothing is a closure over interpreter state.
   */
  def run[R, F[+_]](prog: R ! (Delim + F)): R ! F = {
    type Prog = Any ! (Delim + F)

    /** frames back into a program: binds become flatMaps, markers
     * become pushes — the continuation re-installs its delimiter */
    def reify(segs: List[Seg], start: Prog): Prog =
      segs.foldLeft(start) { (acc, seg) =>
        seg match
          case Seg.K(f) => acc.flatMap(a => f(a).asInstanceOf[Prog])
          case Seg.Delimiter(p) =>
            effect[Delim + F, Any](Push(p.asInstanceOf[Prompt[Any]], acc))
      }

    // ONE tail-recursive loop: an earlier version split it into
    // loop/onOp, and mutual recursion is not tail-optimised, so every
    // operation cost frames and a thousand nested captures blew the
    // stack. Merged, only a FOREIGN operation suspends (under a
    // flatMap closure, as State.handle does) and the Delim ops
    // themselves are flat.
    @tailrec def loop(cur: Prog, kont: List[Seg]): R ! F =
      cur.resume match
        case Pure(a) => kont match
          case Nil => okay.pure(a.asInstanceOf[R])
          case Seg.K(f) :: rest => loop(f(a).asInstanceOf[Prog], rest)
          // the delimited block finished normally: drop its marker
          case Seg.Delimiter(_) :: rest => loop(okay.pure(a), rest)

        case Effect(e) => step(e, kont) match
          case Left(answer) => answer
          case Right((next, k2)) => loop(next, k2)

        case Bind(Effect(e), k) =>
          step(e, Seg.K(k.asInstanceOf[Any => Any]) :: kont) match
            case Left(answer) => answer
            case Right((next, k2)) => loop(next, k2)

    /** one operation: either the machine is done (Left) or it
     * continues with a new program and stack (Right) */
    def step(e: (Delim + F)[Any], kont: List[Seg]): Either[R ! F, (Prog, List[Seg])] =
      <|>[Delim, F](e) match
        case Left(c) => c match
          case Push(p, body) =>
            Right((body.asInstanceOf[Prog], Seg.Delimiter(p) :: kont))

          case Capture(p, f, underPrompt, delimitK) =>
            // everything up to the named prompt is the captured part
            val (captured, rest) = kont.span {
              case Seg.Delimiter(q) => !(q eq p)
              case _ => true
            }
            rest match
              case Seg.Delimiter(_) :: outer =>
                val tag = p.asInstanceOf[Prompt[Any]]
                val k = (a: Any) =>
                  val seg = reify(captured, okay.pure(a))
                  if delimitK then effect[Delim + F, Any](Push(tag, seg)) else seg
                val body = f.asInstanceOf[Any => Any](k).asInstanceOf[Prog]
                // shift/control put the body back under the delimiter;
                // the 0-variants have consumed it
                if underPrompt then
                  Right((effect[Delim + F, Any](Push(tag, body)), outer))
                else Right((body, outer))
              case _ => throw NoPrompt()

        // a foreign operation suspends the machine: the residual
        // program performs it and resumes with the same stack
        case Right(g) => Left(Effect(g).flatMap(x => loop(okay.pure(x), kont)))

    loop(prog.asInstanceOf[Prog], Nil)
  }

  /** abort to a prompt with a value: a shift that drops the
   * continuation (the 0-variant, so the delimiter goes with it) */
  def abort[R, A, F[+_]](p: Prompt[R])(value: R): A ! (Delim + F) =
    shift0[R, A, F](p)(_ => okay.pure(value))
}
