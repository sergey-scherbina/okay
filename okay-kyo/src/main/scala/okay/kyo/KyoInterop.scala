package okay.kyo

import okay.{!, Async, Pure, async}
import okay.given
import _root_.kyo.{<, Abort, AllowUnsafe, Duration, Flat, KyoApp}

/**
 * Interop with kyo (specs/interop.md): value and Async bridges. A
 * pure kyo computation evaluates into a pure okay program; a kyo
 * async computation runs to completion inside one okay Async
 * operation (blocking a virtual thread, Loom-style); an okay Async
 * program becomes a kyo value the blunt way — kyo's effect rows are
 * ArrowEffects internally, so a structural embedding is out of scope
 * here (the shared-subset row mapping is future work).
 */
object KyoInterop {

  /** a pure kyo computation as a pure okay program */
  def fromKyo[A: Flat](k: => A < Any): A ! Pure = okay.pure(k.eval)

  /** run a kyo async computation inside one okay Async operation */
  def fromKyoAsync[A: Flat](k: => A < (Abort[Nothing] & _root_.kyo.Async)): A ! Async =
    async {
      import AllowUnsafe.embrace.danger
      KyoApp.Unsafe.runAndBlock(Duration.Infinity)(k).getOrThrow
    }

  /** an okay Async program as a kyo IO suspension */
  def toKyo[A](p: => A ! Async): A < _root_.kyo.IO =
    _root_.kyo.IO(p.runWith)

  // ------------------------------------------------------------------
  // The structural row mapping, operation for operation. kyo's effects
  // are ArrowEffects — an operation is an arrow Input[C] ~> Output[C]
  // suspended in <, which is exactly our F[A]-with-answer-A shape:
  // Choice is ArrowEffect[Seq, Id] (our Choose verbatim), Emit[V] is
  // ArrowEffect[Const[V], Const[Unit]] (our Writer with a Unit answer),
  // Abort is Const[Error[E]] ~> nothing. Outbound walks our tree;
  // inbound uses their handleFirst, which hands us the raw input AND
  // the continuation — repacked as our operation. Env is a
  // ContextEffect (reader-family): outbound walks, inbound asks once
  // and runs theirs with the constant environment — semantically exact.

  import okay.{%, Reader, Writer, Throws, Choose, effect}
  import okay.!.*
  import _root_.kyo.{Tag, Frame, Env, Emit, Choice, Abort}
  import _root_.kyo.kernel.ArrowEffect

  /** Reader → Env, ask for ask */
  def toKyoEnv[R, A](p: A ! Reader % R)(using Tag[R], Frame): A < Env[R] =
    p.resume match
      case Pure(a) => a
      case Effect(Reader.Ask()) => Env.get[R]
      case Bind(Effect(Reader.Ask()), k) =>
        Env.get[R].flatMap((r: R) => toKyoEnv(k(r)))

  /** Env → Reader: one ask, then their computation runs with it */
  def fromKyoEnv[R, A: Flat](v: A < Env[R])(using Tag[R], Frame): A ! Reader % R =
    effect[Reader % R, R](Reader.Ask()).flatMap(r => okay.pure(Env.run(r)(v).eval))

  /** Writer → Emit, tell for tell */
  def toKyoEmit[W, A](p: A ! Writer % W)(using Tag[Emit[W]], Frame): A < Emit[W] =
    p.resume match
      case Pure(a) => a
      case Effect(e) => Emit.valueWith(e.asInstanceOf[W])(e.asInstanceOf[A])
      case Bind(Effect(e), k) =>
        val w = e.asInstanceOf[W]
        Emit.valueWith(w)(toKyoEmit(k(w.asInstanceOf)))

  /** Emit → Writer: their continuation, repacked as our operation */
  def fromKyoEmit[W, A: Flat](v: A < Emit[W])(using Tag[W], Tag[Emit[W]], Frame): A ! Writer % W =
    ArrowEffect.handleFirst(Tag[Emit[W]], v)(
      handle = [C] => (w, cont) =>
        effect[Writer % W, W](Writer(w)).flatMap(_ => fromKyoEmit(cont(()).asInstanceOf[A < Emit[W]])),
      done = a => (okay.pure(a): A ! Writer % W)
    ).eval

  /** Throws → Abort (the continuation after a raise is dead) */
  def toKyoAbort[E, A](p: A ! Throws % E)(using Tag[Abort[E]], Frame): A < Abort[E] =
    p.resume match
      case Pure(a) => a
      case Effect(t: Throws[?, ?]) => Abort.fail(t.e.asInstanceOf[E])
      case Bind(Effect(t: Throws[?, ?]), _) => Abort.fail(t.e.asInstanceOf[E])

  /** Abort → Throws */
  def fromKyoAbort[E, A: Flat](v: A < Abort[E])(using _root_.kyo.SafeClassTag[E], Tag[E], Frame): A ! Throws % E =
    import _root_.kyo.Result
    Abort.run[E](v).eval.foldFailureOrThrow(e => okay.raise(e))(a => okay.pure(a))

  /** Choose → Choice — the same arrow, Seq ~> Id, on both sides */
  def toKyoChoice[A](p: A ! Choose)(using Frame): A < Choice =
    p.resume match
      case Pure(a) => a
      case Effect(Choose(as)) => Choice.get(as)
      case Bind(Effect(Choose(as)), k) =>
        Choice.get(as).flatMap(x => toKyoChoice(k(x)))

  /** Choice → Choose: their continuation as our multi-shot operation */
  def fromKyoChoice[A: Flat](v: A < Choice)(using Frame): A ! Choose =
    ArrowEffect.handleFirst(Tag[Choice], v)(
      handle = [C] => (as, cont) =>
        effect[Choose, C](Choose(as)).flatMap(c => fromKyoChoice(cont(c).asInstanceOf[A < Choice])),
      done = a => (okay.pure(a): A ! Choose)
    ).eval
}
