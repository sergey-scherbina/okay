package okay

import okay.Freer.Return

/**
 * HANDLERS AS FRAMES OF THE ONE MACHINE (handle-frames, specs/handle-frames.md).
 *
 * A handler form runs as its own fast loop until that loop meets a run nested in it (`Frames.Pending`: a
 * machine run, another handler). Then it UPGRADES: it describes itself — its clause and its state now — as a
 * frame over the rest of its program and hands the whole to the machine, which runs it, the nested run and every
 * handler it meets after as frames on one stack. The host stack no longer grows with the program's handler
 * nesting.
 *
 * A frame is `ret $ body` with a `Cont0.Handling` for its delimiter; the machine makes an operation the frame
 * takes a `shift0` to it whose body is the clause (Delimited.scala). A handler with a state is parameter
 * passing: the frame answers `S => program`, the clause applies the continuation's answer to the next state,
 * and the frame's answer is applied to the state it started from.
 *
 * THE CLAIM this file makes, once per form: a program of the handled row `F + G` is a program of `G` on the
 * machine, at the same erasure — every operation of `F` in it is taken by the frame before it could leave.
 */
// public: the inline handler forms expand into every caller's code and reach it from there
object HandleFrames:

  /**
   * THE STATE FRAME, for every loop that threads a state (handle-frames-loops): from state `s0` over `x`,
   * `step(s, op, resume)` answers the operation — `resume(s2, v)` goes on with the next state and the
   * operation's answer, and a clause that does not call it stops there (a fold that is done); `ret(s, a)` is the
   * answer at the end. Parameter-passing: the frame answers `S => program`.
   */
  def stateful[F[+_], S, A, R, G[+_]](t: TypeableK[F], ret: (S, A) => R ! G)
                                     (step: (S, Any, (S, Any) => R ! G) => R ! G)(s0: S, x: A ! F + G): Shift.U[G, R] =
    statefulOver[F, S, A, R, G, F + G](t, ret)(step)(s0, x)

  /** `stateful` over a program whose row is not `F + G` itself — a walker that RE-TELLS at another type (`Writer.map`:
   * `Writer % W` in, `Writer % V` out): what the frame does not take is still the rest of the row, at erasure */
  def statefulOver[F[+_], S, A, R, G[+_], H[+_]](t: TypeableK[F], ret: (S, A) => R ! G)
                                                (step: (S, Any, (S, Any) => R ! G) => R ! G)(s0: S, x: A ! H): Shift.U[G, R] =
    type Ans = S => Shift.U[G, R]
    val frame = new Cont0.Handling[Ans]("state"):
      def takes(op: Any): Boolean = t.test(op)
      def clause(op: Any, k: Any => Any): Any =
        Return[Shift.Ro[G], Unit, Ans]((s: S) =>
          HandleFrames.clauseAt[S, R, G](step, s, op, k))
    val back: A => Shift.U[G, Ans] = a => Return[Shift.Ro[G], Unit, Ans]((s: S) => ret(s, a).asInstanceOf[Shift.U[G, R]])
    Freer.Inject[Shift.Ro[G], Unit, Unit, Ans](Cont0.Dollar0[Freer.Lift[G], Ans, A, Unit, Unit](
      Cont0.delimiter[Ans, Unit](frame), back, x.asInstanceOf[Shift.U[G, A]]))
      .flatMap(g => g(s0))

  /** the clause with `resume` made of the frame's continuation: `k(v)` answers `S => program`, applied to `s2` */
  private def clauseAt[S, R, G[+_]](step: (S, Any, (S, Any) => R ! G) => R ! G, s: S, op: Any, k: Any => Any): Shift.U[G, R] =
    step(s, op, (s2, v) => k(v).asInstanceOf[Shift.U[G, S => Shift.U[G, R]]].flatMap(g => g(s2)).asInstanceOf[R ! G])
      .asInstanceOf[Shift.U[G, R]]

  /** the state form (`Handler.stateOf`) as a frame from state `s0` over `x` */
  def state[F[+_], S, A, G[+_]](f: [X] => (S, F[X]) => (S, X), t: TypeableK[F])(s0: S, x: A ! F + G): Shift.U[G, (S, A)] =
    stateful[F, S, A, (S, A), G](t, (s, a) => pure((s, a)))((s, op, resume) =>
      val (s2, v) = f(s, op.asInstanceOf[F[Any]])
      resume(s2, v))(s0, x)

  /** a loop's run as a value, its two faces written in place (one object, as `Run` says) */
  @scala.annotation.nowarn("msg=New anonymous class definition will be duplicated")
  inline def run[B, G[+_]](inline fast: B ! G, inline frame: Shift.U[G, B]): B ! G =
    Free.delay(new Run[B, G]:
      def apply(): B ! G = fast
      def program: Shift.U[G, B] = frame)

  /** the control form (`Effects[Free].handle`, `Handler.control`) as a frame over `x`: the clause gets `k` */
  def control[F[+_], A, B, G[+_]](ret: A => Free[G, B], h: F !> Free[G, B], t: TypeableK[F])(x: Free[F + G, A]): Shift.U[G, B] =
    val frame = new Cont0.Handling[B]("handle"):
      def takes(op: Any): Boolean = t.test(op)
      def clause(op: Any, k: Any => Any): Any = h(op.asInstanceOf[F[Any]]) / k.asInstanceOf[Any => Free[G, B]]
    Freer.Inject[Shift.Ro[G], Unit, Unit, B](Cont0.Dollar0[Freer.Lift[G], B, A, Unit, Unit](
      Cont0.delimiter[B, Unit](frame), ret.asInstanceOf[A => Shift.U[G, B]], x.asInstanceOf[Shift.U[G, A]]))

  /** a frame program as a value: stepped into by a running machine, else run on a machine of its own */
  def pending[B, G[+_]](program: Shift.U[G, B]): B ! G =
    Free.delay(Delimited.machine[Freer.Lift[G]].owned[Unit, Unit, B, B ! G](program)(Shift.Stacked.residual[B, G]))

  /**
   * a handler's run as ONE object (stateSmall: a holder of two closures was four allocations a run,
   * 1.55x on 100 small runs): a form subclasses it where its loop is, `apply` the loop, `program` the frame
   */
  abstract class Run[B, G[+_]] extends Frames.Pending[Freer.Lift[G], Unit, Unit, B, B ! G]
