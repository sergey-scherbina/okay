package okay

import okay.Freer.Return

/**
 * HANDLERS AS FRAMES OF THE ONE MACHINE (handle-frames, specs/handle-frames.md).
 *
 * A handler's run is a VALUE (`Run`, a `Frames.Pending` in a `Delay`) with two faces: its fast fold, and its
 * FRAME. Forced by anything but a machine, it is the fold; a fold that meets a run nested in it forces that one
 * as a fold too, until `Limit` folds deep, and there as its frame on a machine — inside which every handler is a
 * frame, so nothing nests past it. A running machine steps into a run's frame in its own loop. The host stack is
 * bounded by `Limit`, whatever the program's handler nesting; a composition of handlers stays on the folds.
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
    statefulAll[S, A, R, G, H](t.test, ret, null)(step)(s0, x)

  /**
   * the state frame in full: `takes` says which operations are the frame's (a frame that must SEE a forwarded
   * operation — Resource's release before a final one — takes them all and performs them again below itself);
   * `onThrow`, if not null, makes it a catch frame too, answering a throw from inside with the state in force
   * (handle-frames-catch): the frame's answer is `S => program`, applied to that state by the frame below it
   */
  def statefulAll[S, A, R, G[+_], H[+_]](takes0: Any => Boolean, ret: (S, A) => R ! G, onThrow: ((S, Throwable) => R ! G) | Null)
                                        (step: (S, Any, (S, Any) => R ! G) => R ! G)(s0: S, x: A ! H): Shift.U[G, R] =
    type Ans = S => Shift.U[G, R]
    val frame: Cont0.Handling[Ans] =
      if onThrow == null then new Cont0.Handling[Ans]("state"):
        def takes(op: Any): Boolean = takes0(op)
        def clause(op: Any, k: Any => Any): Any =
          Return[Shift.Ro[G], Unit, Ans]((s: S) => HandleFrames.clauseAt[S, R, G](step, s, op, k))
      else
        val thrown = onThrow
        new Cont0.Handling[Ans]("state") with Cont0.Catching:
          def takes(op: Any): Boolean = takes0(op)
          def clause(op: Any, k: Any => Any): Any =
            Return[Shift.Ro[G], Unit, Ans]((s: S) => HandleFrames.clauseAt[S, R, G](step, s, op, k))
          def caught(t: Throwable): Any =
            Return[Shift.Ro[G], Unit, Ans]((s: S) => thrown(s, t).asInstanceOf[Shift.U[G, R]])
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

  /** a `try` as a frame (handle-frames-catch): `h` answers a throw from anything `x` runs, `x` built under it too */
  def catching[A, G[+_]](h: Throwable => A ! G)(x: => A ! G): Shift.U[G, A] =
    val frame = new Prompt[A]("try", "catch") with Cont0.Catching:
      def caught(t: Throwable): Any = h(t)
    Freer.Inject[Shift.Ro[G], Unit, Unit, A](Cont0.Dollar0[Freer.Lift[G], A, A, Unit, Unit](
      Cont0.delimiter[A, Unit](frame), (a: A) => Return[Shift.Ro[G], Unit, A](a), Free.delay(() => x).asInstanceOf[Shift.U[G, A]]))

  /** the control form (`Effects[Free].handle`, `Handler.control`) as a frame over `x`: the clause gets `k` */
  def control[F[+_], A, B, G[+_]](ret: A => Free[G, B], h: F !> Free[G, B], t: TypeableK[F])(x: Free[F + G, A]): Shift.U[G, B] =
    val frame = new Cont0.Handling[B]("handle"):
      def takes(op: Any): Boolean = t.test(op)
      def clause(op: Any, k: Any => Any): Any = h(op.asInstanceOf[F[Any]]) / k.asInstanceOf[Any => Free[G, B]]
    Freer.Inject[Shift.Ro[G], Unit, Unit, B](Cont0.Dollar0[Freer.Lift[G], B, A, Unit, Unit](
      Cont0.delimiter[B, Unit](frame), ret.asInstanceOf[A => Shift.U[G, B]], x.asInstanceOf[Shift.U[G, A]]))

  /** a frame program as a value: stepped into by a running machine, else run on a machine of its own */
  def pending[B, G[+_]](program: Shift.U[G, B]): B ! G =
    Free.delay(Delimited.machine[Freer.Lift[G]].owned[Unit, Unit, B, B ! G](program)(Shift.residual[B, G]))

  /**
   * HOW DEEP FOLDS NEST (handle-frames-loops, measured): a fold that meets a nested run FORCES it — the nested
   * fold runs on this one's stack, the old behaviour and the fast one — until it is `Limit` folds deep; there
   * the nested run runs as its FRAME on a machine of its own, and inside a machine every handler is a frame, so
   * nothing nests past it. Handing the outer fold to the machine at the first nested run instead cost a
   * composition of two handlers 7.4x (SplitBenchmark.mixedList, `State.run(Writer.run(p))`): on the machine every
   * tail-resumptive operation is a capture. The bound is the stack's: 32 folds of a few frames each, well inside a
   * 128 KB thread and Scala.js.
   */
  final val Limit = 32

  /**
   * a handler's run as ONE object (stateSmall: a holder of two closures was four allocations a run,
   * 1.55x on 100 small runs): `at(depth)` its fold, entered `depth` folds deep; `program` its frame
   */
  abstract class Run[B, G[+_]] extends Frames.Pending[Freer.Lift[G], Unit, Unit, B, B ! G]:
    def at(depth: Int): B ! G
    final def apply(): B ! G = at(0)

  /** a loop's run as a value, its two faces written in place (one object, as `Run` says) */
  @scala.annotation.nowarn("msg=New anonymous class definition will be duplicated")
  inline def run[B, G[+_]](inline fast: Int => B ! G, inline frame: Shift.U[G, B]): B ! G =
    Free.delay(new Run[B, G]:
      def at(depth: Int): B ! G = fast(depth)
      def program: Shift.U[G, B] = frame)

  /**
   * the nested run at the head of `y` (a `Delay`, alone or under its `Bind`) FORCED, for a fold `depth` deep: as
   * its fold below the `Limit`, as its frame on a machine at it. A machine run (`Own`) is forced as it is.
   */
  def shallow[A, H[+_]](y: A ! H, depth: Int): A ! H = y match
    case Freer.Delay(t) => forced[A, H](t, depth)
    case Freer.Bind(Freer.Delay(t), g) => forced[Any, H](t, depth).flatMap(g.asInstanceOf[Any => A ! H])
    case other => other

  /** THE ONE CLAIM: the thunk of a `Delay` in a program of row H answers a program of row H */
  private def forced[A, H[+_]](t: () => Any, depth: Int): A ! H = t match
    case r: Run[?, ?] =>
      if depth < Limit then r.at(depth + 1).asInstanceOf[A ! H]
      else Delimited.machine[Freer.Lift[H]].owned[Unit, Unit, A, A ! H](r.program.asInstanceOf[Shift.U[H, A]])(
        Shift.residual[A, H])()
    case o => o().asInstanceOf[A ! H]
