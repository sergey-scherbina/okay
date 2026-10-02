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

  /** a frame program as a value: stepped into by a running machine, else run on a machine of its own */
  def pending[B, G[+_]](program: Shift.U[G, B]): B ! G =
    Free.delay(new Frames.Own[Freer.Lift[G], Unit, Unit, B, B ! G](program, Shift.Stacked.residual[B, G]))

  /** a handler's run as a value: forced, its fast loop; stepped into by a machine, its frame */
  def handled[B, G[+_]](fast: () => B ! G, frame: () => Shift.U[G, B]): B ! G =
    Free.delay(new Frames.Handled[Freer.Lift[G], Unit, Unit, B, B ! G](fast, frame))
