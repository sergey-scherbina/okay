package okay.freer



import okay.freer.Shift.{Abort, Dollar, Push, Resume, Resumption, Shift0}

/**
 * SHIFT ON THE MACHINE (shift-file-split): what `Delimited` does with Shift's operations — `Steps`, which
 * installs, cuts and puts back prompts and handler frames and answers a throw; the one place Shift's claims
 * about a `Free` row are made (`claim`); the barrier; the nested run. The operations themselves, and the API,
 * are `object Shift`'s (Shift.scala), which imports these; `Shift.Pending` is this `Pending`.
 */
object ShiftMachine {

  /** the prompt an operation names */
  private[okay] def promptOf(x: Any): Prompt[?] | Null = x match
    case d: Dollar[?, ?, ?, ?] => d.p
    case d: Push[?, ?, ?] => d.p
    case s: Shift0[?, ?, ?, ?] => s.p
    case a: Abort[?, ?, ?] => a.p
    case r: Resume[?, ?, ?, ?] => r.p
    case _ => null

  /**
   * THE CLAIMS of a `Free` row on the machine, made here and nowhere else. The machine is cast-free; what its
   * types cannot say about a row of `Free` is said once here: every node of a `Free` program is at `Unit`, so
   * every segment and stack of its run is; an operation, or a nested run, was built in the row of the program it
   * stands in; and the place a prompt's boundary stands answers that prompt's type (Dybvig, Peyton Jones & Sabry's
   * `eqPrompt`).
   */
  private[okay] def claim[X](x: Any): X = x.asInstanceOf[X]

  /** `Steps.held`'s bits */
  private final val Framed = 1
  private final val Catches = 2
  /** a FINALIZING catch frame (a `Resource` scope): only then may an abort's dropped piece need discontinuing */
  private final val Finalizes = 4

  /** a nested run's value boundary: no capture crosses it (a handler's operation and a throw do) */
  private object Barrier extends Delimited.Mark

  /**
   * A RUN AS A VALUE: a `Delay`'s thunk, forced by whoever holds it, STEPPED INTO by a machine for the row already
   * running (`Steps.enter`) — so a run nested in a run, a reset in a reset or a handler in a handler, is one loop and
   * not a host frame. `Nested` is Shift's own; a handler's run (`HandleFrames.Run`) is the other.
   */
  abstract class Pending[R, F[+_]] extends (() => R ! F), Freer.Suspended:
    def program: R ! Shift % ? + F

  /** a run of a `Shift % ? + F` program: `nested` — no barrier, a capture with no delimiter here goes out */
  private[okay] final class Nested[R, F[+_]](val program: R ! Shift % ? + F, val nested: Boolean) extends Pending[R, F]:
    def apply(): R ! F =
      val s = Steps[F](nested)
      Delimited.over[Unary[Shift % ? + F], F, Unary[F]](s, s).value(program)

  /**
   * what each operation does, for a row `Shift % ? + F`, and which leave the machine. A prompt in force is two
   * value boundaries, its opening and its closing, with `ret` between them; a HANDLER FRAME is one too
   * (`HandleFrames.Handling`), and an operation of `F` it takes is a capture to it whose body is its clause — `k`
   * putting the frame back (a deep handler). A CATCH FRAME (`HandleFrames.Catching`) answers a throw in its place.
   */
  private[okay] final class Steps[F[+_]](nested: Boolean)
    extends Delimited.Step[Unary[Shift % ? + F], Unary[Shift % ? + F]], Delimited.Outer[Unary[Shift % ? + F], F]:
    private type L[S, R, A] = Unary[Shift % ? + F][S, R, A]
    private type U = Unit

    /**
     * what kinds of frame this run has installed: `Framed` (a handler's — until then an operation of `F` leaves
     * without a look), `Catches` (a catch frame's — until then user code runs with no `try`) and `Finalizes` (a
     * finalizing one's — until then an abort drops its piece with no look for a scope to release). A captured `k`
     * carries the bits of the run it was captured in, and the run that resumes it — perhaps another, a dialogue
     * driven later — takes them on with the frames its piece puts back
     */
    private var held = 0
    private def framed: Boolean = (held & Framed) != 0
    private def hold(bits: Int, machine: Delimited[L]): Unit =
      if (bits & Catches) != 0 && (held & Catches) == 0 then machine.guarding()
      held |= bits
    /** the frame `apply` found for the operation it kept, for `step` to cut to */
    private var taker: HandleFrames.Handling[?] | Null = null

    // ---- Outer: which operations leave

    def apply[T, R, X, B, S, Z](op: L[T, R, X], m: Stack[L, B, S, R, Z], machine: Delimited[L]): F[X] | Null = (op: (Shift % ? + F)[X]) match
      // the usual one first, one type test: an operation of another effect (shift-generator-cost)
      case s: Shift[?, ?] => if nested then ownPrompt(s, op, m, machine) else null
      case _ =>
        if !framed then claim[F[X]](op)
        else machine.holds(m, takes(op)) match
          case h: HandleFrames.Handling[?] => taker = h; null
          case _ => claim[F[X]](op)

    /** in a run with no barrier, a capture to a prompt not installed here goes out; an installation and a
     * resumption are always this run's — they put a prompt on, not look for one */
    private def ownPrompt[T, R, X, B, S, Z](s: Shift[?, ?], op: L[T, R, X], m: Stack[L, B, S, R, Z], machine: Delimited[L]): F[X] | Null =
      s match
        case _: Dollar[?, ?, ?, ?] | _: Push[?, ?, ?] | _: Resume[?, ?, ?, ?] => null
        case _ =>
          val p = promptOf(s)
          if p != null && machine.holds(m, p.is) == null then claim[F[X]](op) else null

    private def takes(op: Any): Delimited.Mark => Boolean =
      case h: HandleFrames.Handling[?] => h.takes(op)
      case _ => false

    def diagonal[T, R, A](op: L[T, R, A]): T =:= R = claim[T =:= R](summon[T =:= T])

    override def enter[T, R, A](t: () => Freer[L, T, R, A]): Freer[L, T, R, A] | Null = t match
      case p: Pending[?, ?] => claim[Freer[L, T, R, A]](p.program)
      case _ => null

    override def barrier(t: () => Any): Delimited.Mark | Null = t match
      case n: Nested[?, ?] if !n.nested => Barrier
      case _ => null

    // ---- Step: what the machine does with them

    def step[A, B, S, T, R, Z](op: L[T, R, A], k: Frames[L, A, B, S, T], m: Stack[L, B, S, R, Z],
                               machine: Delimited[L]): Delimited.Next[L, Z] =
      at(op, claim[Frames[L, A, B, U, U]](k), claim[Stack[L, B, U, U, Z]](m), machine)

    private def at[A, B, Z](op: (Shift % ? + F)[A], k: Frames[L, A, B, U, U], m: Stack[L, B, U, U, Z],
                            machine: Delimited[L]): Delimited.Next[L, Z] = op match
      case d: Dollar[?, r0, r, ?] =>
        d.p match
          case _: HandleFrames.Handling[?] => hold(Framed, machine)
          case _ => ()
        d.p match
          case c: HandleFrames.Catching => hold(if c.finalizes then Catches | Finalizes else Catches, machine)
          case _ => ()
        val ret = claim[r0 => Freer[L, U, U, r]](d.ret)
        machine.next(claim[Freer[L, U, U, r0]](d.body), machine.end[r0, U], machine.delim(d.p, machine.frame(ret, claim[Frames[L, r, B, U, U]](k)), m))
      case d: Push[?, r, ?] =>
        machine.next(claim[Freer[L, U, U, r]](d.body), machine.end[r, U], machine.delim(d.p.whole, claim[Frames[L, r, B, U, U]](k), m))
      case s: Shift0[?, a, r, ?] =>
        val c = claim[Cut[a, r, Z]](cutOrFail(s.p, s.at, k, m, machine))
        val body = claim[Resumption[a, r, F] => Freer[L, U, U, r]](s.body)
        // `shift`: the body under a fresh reset of `p` — its boundary pushed in this same step (shift-generator-cost:
        // as `shift0` of a `push` it was an operation, a node and a step more a capture)
        val k2 = Resumption[a, r, F](s.p, c.piece, held)
        if s.under then machine.next(body(k2), machine.end[r, U], machine.delim(s.p.whole, c.out, c.rest))
        else machine.next(body(k2), c.out, c.rest)
      case ab: Abort[?, a, r] =>
        val c = claim[Cut[a, r, Z]](cutOrFail(ab.p, ab.at, k, m, machine))
        // the piece an abort drops is DISCONTINUED when a scope in it must release (resource-abort-releases): thrown
        // into, its finalizers run inner first, and the value answered after them — else just the value
        if (held & Finalizes) == 0 || !finalizerIn(c.piece) then machine.next(Freer.Return[L, U, r](ab.value), c.out, c.rest)
        else
          val dropped = Resumption[a, r, F](ab.p, c.piece, held).discontinue.map(_ => ab.value)
          machine.next(claim[Freer[L, U, U, r]](dropped), c.out, c.rest)
      case rs: Resume[?, a, r, ?] =>
        hold(rs.held, machine)
        machine.reinstall(claim[Delimited.Piece[L, a, U, r, U]](rs.k), claim[Freer[L, U, U, a]](rs.body), claim[Frames[L, r, B, U, U]](k), m)
      // an operation of `F` a frame takes (`apply` kept it): a capture to the frame, the clause its body
      case _ =>
        val h = taker.nn
        taker = null
        handled(h, op, cut(k, m, _ eq h, never, machine).nn, machine)

    /** the clause of frame `h` for `op` in the frame's place, `k` resuming the piece cut to it */
    private def handled[A, Y, Z](h: HandleFrames.Handling[Y], op: Any, c: Cut[A, Any, Z], machine: Delimited[L]): Delimited.Next[L, Z] =
      val piece = claim[Delimited.Piece[L, Any, U, Y, U]](c.piece)
      machine.next(claim[Freer[L, U, U, Any]](h.clause(op, Resumption[Any, Y, F](h, piece, held))), c.out, c.rest)

    /** a throw: the nearest catch frame answers it in its place, the frames above it dropped; one that declines
     * (null) or throws passes it, or what it threw, to the frames below; none takes it — thrown on */
    override def thrown[A, B, S, T, R, Z](t: Throwable, k: Frames[L, A, B, S, T], m: Stack[L, B, S, R, Z],
                                          machine: Delimited[L]): Delimited.Next[L, Z] | Null =
      catchFrom(t, t, claim[Frames[L, A, B, U, U]](k), claim[Stack[L, B, U, U, Z]](m), machine)

    /** `first` the throw the walk began with: none taking `t` — null when it is that one (the machine throws it on),
     * else `t` thrown here, a handler's own throw, so it is not lost for the one it replaced */
    @scala.annotation.tailrec
    private def catchFrom[A, B, Z](first: Throwable, t: Throwable, k: Frames[L, A, B, U, U], m: Stack[L, B, U, U, Z],
                                   machine: Delimited[L]): Delimited.Next[L, Z] | Null =
      cut(k, m, catches, never, machine) match
        case null => if t eq first then null else throw t
        case c => answerOf(c.tag, t) match
          case null => catchFrom(first, t, c.out, c.rest, machine)
          case again: Delimited.Thrown => catchFrom(first, again.t, c.out, c.rest, machine)
          case p => machine.next(claim[Freer[L, U, U, Any]](p), c.out, c.rest)

    /** the catch frame's answer for `t`: a program, null (not its throw), or what its handler threw */
    private def answerOf(tag: Delimited.Mark, t: Throwable): Any = tag match
      case h: HandleFrames.Catching => try h.caught(t) catch case t2: Throwable => Delimited.Thrown(t2)
      case _ => null

    /** a finalizing catch frame among the boundaries of `piece` */
    @scala.annotation.tailrec
    private def finalizerIn[A0, T0, A, T](piece: Delimited.Piece[L, A0, T0, A, T]): Boolean = piece match
      case Delimited.Piece.One(_, tag) => finalizer(tag)
      case Delimited.Piece.Snoc(prev, _, tag) => finalizer(tag) || finalizerIn(prev)

    private def finalizer(tag: Delimited.Mark | Null): Boolean = tag match
      case c: HandleFrames.Catching => c.finalizes
      case _ => false

    private val catches: Delimited.Mark => Boolean = _.isInstanceOf[HandleFrames.Catching]
    private val never: Delimited.Mark => Boolean = _ => false
    private val isBarrier: Delimited.Mark => Boolean = _ eq Barrier

    /** a capture to a prompt: the piece up to and including its boundary and `ret` (taken along), and what lies
     * under them */
    private final class Cut[A, R, Z](val tag: Delimited.Mark, val piece: Delimited.Piece[L, A, U, R, U],
                                     val out: Frames[L, R, Any, U, U], val rest: Stack[L, Any, U, U, Z])

    private def cutOrFail[A, B, Z](p: Prompt[?], from: String, k: Frames[L, A, B, U, U], m: Stack[L, B, U, U, Z],
                                   machine: Delimited[L]): Cut[A, Any, Z] =
      cut(k, m, p.is, isBarrier, machine) match
        case null => throw NoPrompt(from, p.label, installed(m))
        case c => c

    /** to the nearest prompt `is` holds for, `stop` before a barrier it holds for; null when none */
    private def cut[A, B, Z](k: Frames[L, A, B, U, U], m: Stack[L, B, U, U, Z], is: Delimited.Mark => Boolean,
                             stop: Delimited.Mark => Boolean, machine: Delimited[L]): Cut[A, Any, Z] | Null =
      machine.cut(k, m, is, stop) match
        case null => null
        case open => (open.tag, open.rest) match
          // a reset's one boundary: the piece is up to and including it
          case (w: Prompt.Whole, _) =>
            Cut(w.p, claim[Delimited.Piece[L, A, U, Any, U]](open.piece), claim[Frames[L, Any, Any, U, U]](open.out),
              claim[Stack[L, Any, U, U, Z]](open.rest))
          // a dollar's: its `ret`, the first frame above it, goes into `k` (λ$'s `S0 k.e`), the body answers below
          // it — so `k`'s last segment is `ret` alone, over an unmarked boundary where the caller's continuation joins
          case (p: Prompt[?], _) => open.out match
            case Frames.Frame(ret, out) =>
              val piece = Delimited.Piece.Snoc(open.piece, Frames.Frame(ret, Frames.end), null)
              Cut(p, claim[Delimited.Piece[L, A, U, Any, U]](piece), claim[Frames[L, Any, Any, U, U]](out),
                claim[Stack[L, Any, U, U, Z]](open.rest))
            case _ => throw IllegalStateException(s"prompt ${open.tag} has no ret above it")
          case _ => throw IllegalStateException(s"a capture found ${open.tag}, which is no prompt")

    /** the prompts on the machine's stack, innermost first, for `NoPrompt` */
    private def installed[B, Z](m: Stack[L, B, U, U, Z]): List[String] =
      var seen = List.empty[String]
      labels(m, p => seen = p.label :: seen)
      seen.reverse

    @scala.annotation.tailrec
    private def labels[B, S, R, Z](m: Stack[L, B, S, R, Z], see: Prompt[?] => Unit): Unit = m match
      case Stack.Delim(tag, _, rest) =>
        tag match
          case p: Prompt[?] => see(p)
          case w: Prompt.Whole => see(w.p)
          case _ => ()
        if !(tag eq Barrier) then labels(rest, see)
      case _ => ()

}
