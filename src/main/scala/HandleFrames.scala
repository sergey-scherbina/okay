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

  /** the state form (`Handler.stateOf`) as a frame from state `s0` over `x` */
  def state[F[+_], S, A, G[+_]](f: [X] => (S, F[X]) => (S, X), t: TypeableK[F])(s0: S, x: A ! F + G): Shift.U[G, (S, A)] =
    type Ans = S => Shift.U[G, (S, A)]
    val frame = new Cont0.Handling[Ans]("state"):
      def takes(op: Any): Boolean = t.test(op)
      def clause(op: Any, k: Any => Any): Any =
        Return[Shift.Ro[G], Unit, Ans]((s: S) =>
          val (s2, v) = f(s, op.asInstanceOf[F[Any]])
          k(v).asInstanceOf[Shift.U[G, Ans]].flatMap(g => g(s2)))
    val ret: A => Shift.U[G, Ans] = a => Return[Shift.Ro[G], Unit, Ans]((s: S) => Return[Shift.Ro[G], Unit, (S, A)]((s, a)))
    Freer.Inject[Shift.Ro[G], Unit, Unit, Ans](Cont0.Dollar0[Freer.Lift[G], Ans, A, Unit, Unit](
      Cont0.delimiter[Ans, Unit](frame), ret, x.asInstanceOf[Shift.U[G, A]]))
      .flatMap(g => g(s0))

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
