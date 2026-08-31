package okay

import scala.annotation.tailrec
import okay.!.*

/**
 * The consumer side of a pipeline: await the next element of type V.
 * A consumer is a program with the Take effect — the exact dual of a
 * writer (tell pushes an element out, await pulls one in), and pipe
 * below is the classic coroutine pairing of the two by delimited
 * continuations: no channel, no buffer, no materialized list — each
 * await transfers control to the producer for exactly one element.
 */
enum Take[V, +A]:
  /** the next element, or None — the producer has ended */
  case Await[V]() extends Take[V, Option[V]]

object Take:
  /** the next element, or None at the end of the input */
  inline def await[V]: Option[V] ! Take % V = effect(Await())

/**
 * Connect a producer to a consumer: each await meets the next told
 * value, control bouncing between the two programs one element at a
 * time. The consumer drives — a finite consumer ends an INFINITE
 * producer (only the asked elements are ever computed); when the
 * producer ends first, every further await answers None. The
 * consumer's answer is the result; the producer's surplus is dropped,
 * its answer ignored. Fully typed by the Take/Writer GADTs — no casts.
 */
def pipe[W, A, B](p: A ! Writer % W)(c: B ! Take % W): B = {
  @tailrec def loop(p: A ! Writer % W, c: B ! Take % W): B = (c.resume: @unchecked) match
    case Pure(b) => b
    case Effect(Take.Await()) => Writer.uncons(p).toOption.map(_._1)
    case Bind(Effect(Take.Await()), k) => Writer.uncons(p) match
      case Right((w, rest)) => loop(rest, k(Some(w)))
      case Left(_) => loop(p, k(None))

  loop(p, c)
}

/**
 * A pipeline stage (specs/stage-pipeline.md): a transducer as a
 * program — it awaits I and tells O, state is just its recursion
 * parameters. Tokenizers, parsers, codec dialects, any stream
 * rewriter share this one shape; composition (through) is
 * demand-driven coroutine pairing, so every stage is incremental,
 * resumable and lazy by construction.
 */
type Stage[I, O, A] = A ! (Take % I + Writer % O)

object Stage {

  /** the next input, or None — upstream ended */
  inline def await[I, O]: Option[I] ! (Take % I + Writer % O) =
    effect(Take.Await())

  /** emit one output */
  inline def tell[I, O](o: O): O ! (Take % I + Writer % O) =
    effect(Writer(o))

  /** the identity stage: every input becomes an output */
  def id[T]: Stage[T, T, Unit] =
    await[T, T].flatMap {
      case Some(t) => tell[T, T](t).flatMap(_ => id[T])
      case None => pure(())
    }

  /** batch inputs into chunks of the given size (the tail flushes on
   * end of input — a stage may still tell after seeing None) */
  def chunked[T](size: Int): Stage[T, Chunk[T], Unit] =
    def go(buf: Vector[T]): Stage[T, Chunk[T], Unit] =
      await[T, Chunk[T]].flatMap {
        case Some(t) =>
          val b = buf :+ t
          if b.length >= size then tell[T, Chunk[T]](ChunkBuf.ofSpecialized(b))
            .flatMap(_ => go(Vector.empty))
          else go(b)
        case None =>
          if buf.isEmpty then pure(())
          else tell[T, Chunk[T]](ChunkBuf.ofSpecialized(buf)).map(_ => ())
      }

    go(Vector.empty)

  /** flatten chunks back into elements */
  def unchunk[T]: Stage[Chunk[T], T, Unit] =
    await[Chunk[T], T].flatMap {
      case Some(c) =>
        def emit(i: Int): Stage[Chunk[T], T, Unit] =
          if i >= c.length then unchunk[T]
          else tell[Chunk[T], T](c(i)).flatMap(_ => emit(i + 1))
        emit(0)
      case None => pure(())
    }
}

/**
 * Compose two stages, demand-driven: the downstream's awaits are fed
 * by the upstream's tells; the upstream's awaits become the composed
 * stage's awaits; when the upstream finishes, further downstream
 * awaits answer None (the upstream may have flushed first). The
 * downstream's answer is the result. Nothing runs until the final
 * consumer pulls.
 */
def through[I, M, O, A, B](up: Stage[I, M, A])(down: Stage[M, O, B]): Stage[I, O, B] = {
  type Res = Take % I + Writer % O

  // drive the upstream until it tells (Some(m) + rest) or ends (None);
  // its own awaits surface as OUR awaits, in CPS to stay a program
  def pull(u: Stage[I, M, A])(cont: (Option[M], Stage[I, M, A]) => B ! Res): B ! Res =
    (u.resume: @unchecked) match
      case Pure(_) => cont(None, u)
      case Effect(e) => <|>[Take % I, Writer % M](e) match
        case Left(Take.Await()) =>
          // a final await tells nothing more: the upstream is done
          cont(None, u)
        case Right(w) => cont(Some(okay.out(w)), Free.Pure(null.asInstanceOf[A]))
      case Bind(Effect(e), k) => <|>[Take % I, Writer % M](e) match
        case Left(Take.Await()) =>
          effect[Res, Option[I]](Take.Await()).flatMap(oi => pull(k(oi.asInstanceOf))(cont))
        case Right(w) => cont(Some(okay.out(w)), k(w.asInstanceOf))

  def loop(u: Stage[I, M, A], d: Stage[M, O, B]): B ! Res =
    (d.resume: @unchecked) match
      case Pure(b) => pure(b)
      case Effect(e) => <|>[Take % M, Writer % O](e) match
        case Left(Take.Await()) => pull(u)((om, _) => pure(om.asInstanceOf[B]))
        case Right(o) => effect[Res, B](o.asInstanceOf[Res[B]])
      case Bind(Effect(e), k) => <|>[Take % M, Writer % O](e) match
        case Left(Take.Await()) => pull(u)((om, u2) => loop(u2, k(om.asInstanceOf)))
        case Right(o) =>
          effect[Res, Any](o.asInstanceOf[Res[Any]]).flatMap(x => loop(u, k(x.asInstanceOf)))

  loop(up, down)
}

/** run a plain producer through a stage: its tells feed the stage's
 * awaits, the stage's tells are the result stream */
@scala.annotation.targetName("throughProducer")
def through[W, M, A, B](p: A ! Writer % W)(s: Stage[W, M, B]): B ! Writer % M = {
  def loop(rest: A ! Writer % W, d: Stage[W, M, B]): B ! Writer % M =
    (d.resume: @unchecked) match
      case Pure(b) => pure(b)
      case Effect(e) => <|>[Take % W, Writer % M](e) match
        case Left(Take.Await()) => pure(Writer.uncons(rest).toOption.map(_._1).asInstanceOf[B])
        case Right(m) => effect[Writer % M, B](m.asInstanceOf[(Writer % M)[B]])
      case Bind(Effect(e), k) => <|>[Take % W, Writer % M](e) match
        case Left(Take.Await()) => Writer.uncons(rest) match
          case Right((w, r)) => loop(r, k(Some(w).asInstanceOf))
          case Left(_) => loop(rest, k(None.asInstanceOf))
        case Right(m) =>
          effect[Writer % M, Any](m.asInstanceOf[(Writer % M)[Any]]).flatMap(x => loop(rest, k(x.asInstanceOf)))

  loop(p, s)
}

/**
 * Compose two EFFECTFUL stages: both sides may perform arbitrary
 * effects G (Async above all) between their awaits and tells; the G
 * operations of either side forward into the composed row in the
 * order they are reached. Same demand-driven pairing as the pure
 * through — nothing runs until the final consumer pulls, and a G op
 * runs only when the pull actually crosses it.
 */
@scala.annotation.targetName("throughG")
def through[I, M, O, G[+_] : TypeableK, A, B](up: A ! (Take % I + (Writer % M + G)))
                                             (down: B ! (Take % M + (Writer % O + G)))
                                             : B ! (Take % I + (Writer % O + G)) = {
  type Up = Take % I + (Writer % M + G)
  type Res = Take % I + (Writer % O + G)

  // drive the upstream to its next tell, re-emitting its awaits as
  // OUR awaits and forwarding its G ops on the way
  def pull(u: A ! Up)(cont: (Option[M], A ! Up) => B ! Res): B ! Res =
    (u.resume: @unchecked) match
      case Pure(_) => cont(None, u)
      case Effect(e) => <|>[Take % I, Writer % M + G](e) match
        case Left(Take.Await()) => cont(None, u)
        case Right(rest) => <|>[G, Writer % M](rest) match
          case Left(g) => effect[Res, Any](g.asInstanceOf[Res[Any]])
            .flatMap(_ => cont(None, Free.Pure(null.asInstanceOf[A])))
          case Right(w) => cont(Some(okay.out(w)), Free.Pure(null.asInstanceOf[A]))
      case Bind(Effect(e), k) => <|>[Take % I, Writer % M + G](e) match
        case Left(Take.Await()) =>
          effect[Res, Option[I]](Take.Await()).flatMap(oi => pull(k(oi.asInstanceOf))(cont))
        case Right(rest) => <|>[G, Writer % M](rest) match
          case Left(g) => effect[Res, Any](g.asInstanceOf[Res[Any]])
            .flatMap(x => pull(k(x.asInstanceOf))(cont))
          case Right(w) => cont(Some(okay.out(w)), k(w.asInstanceOf))

  def loop(u: A ! Up, d: B ! (Take % M + (Writer % O + G))): B ! Res =
    (d.resume: @unchecked) match
      case Pure(b) => pure(b)
      case Effect(e) => <|>[Take % M, Writer % O + G](e) match
        case Left(Take.Await()) => pull(u)((om, _) => pure(om.asInstanceOf[B]))
        case Right(o) => effect[Res, B](o.asInstanceOf[Res[B]])
      case Bind(Effect(e), k) => <|>[Take % M, Writer % O + G](e) match
        case Left(Take.Await()) => pull(u)((om, u2) => loop(u2, k(om.asInstanceOf)))
        case Right(o) =>
          effect[Res, Any](o.asInstanceOf[Res[Any]]).flatMap(x => loop(u, k(x.asInstanceOf)))

  loop(up, down)
}

/**
 * An effectful producer through an effectful stage: the producer's
 * tells feed the stage's awaits, everyone's G ops forward, the
 * stage's tells are the result stream — the generalization the LLM
 * client walks by hand (SSE lines ! Async through the event stage).
 */
@scala.annotation.targetName("throughProducerG")
def through[W, M, G[+_] : TypeableK, A, B](p: A ! (Writer % W + G))
                                          (s: B ! (Take % W + (Writer % M + G)))
                                          : B ! (Writer % M + G) = {
  type Src = Writer % W + G
  type Res = Writer % M + G

  def pull(rest: A ! Src)(cont: (Option[W], A ! Src) => B ! Res): B ! Res =
    (rest.resume: @unchecked) match
      case Pure(_) => cont(None, rest)
      case Effect(e) => <|>[G, Writer % W](e) match
        case Left(g) => effect[Res, Any](g.asInstanceOf[Res[Any]])
          .flatMap(_ => cont(None, Free.Pure(null.asInstanceOf[A])))
        case Right(w) => cont(Some(w.asInstanceOf[W]), Free.Pure(null.asInstanceOf[A]))
      case Bind(Effect(e), k) => <|>[G, Writer % W](e) match
        case Left(g) => effect[Res, Any](g.asInstanceOf[Res[Any]])
          .flatMap(x => pull(k(x.asInstanceOf))(cont))
        case Right(w) => cont(Some(w.asInstanceOf[W]), k(w.asInstanceOf))

  def loop(rest: A ! Src, d: B ! (Take % W + (Writer % M + G))): B ! Res =
    (d.resume: @unchecked) match
      case Pure(b) => pure(b)
      case Effect(e) => <|>[Take % W, Writer % M + G](e) match
        case Left(Take.Await()) => pull(rest)((ow, _) => pure(ow.asInstanceOf[B]))
        case Right(o) => effect[Res, B](o.asInstanceOf[Res[B]])
      case Bind(Effect(e), k) => <|>[Take % W, Writer % M + G](e) match
        case Left(Take.Await()) => pull(rest)((ow, r2) => loop(r2, k(ow.asInstanceOf)))
        case Right(o) =>
          effect[Res, Any](o.asInstanceOf[Res[Any]]).flatMap(x => loop(rest, k(x.asInstanceOf)))

  loop(p, s)
}

/**
 * The same pipe for a producer performing arbitrary effects G: the
 * consumer still drives, and the G-operations met between elements
 * are carried into the answer — the result is a program in G.
 * (Structured effects are handled over the producer first — handlers
 * are stream transformers; the Handler-able residue is what remains.)
 */
def pipe[W, A, B, G[+_] : TypeableK](p: A ! Writer % W + G)(c: B ! Take % W): B ! G = {
  def loop(p: A ! Writer % W + G, c: B ! Take % W): B ! G = (c.resume: @unchecked) match
    case Pure(b) => pure(b)
    case Effect(Take.Await()) => Writer.uncons(p).map(_.toOption.map(_._1))
    case Bind(Effect(Take.Await()), k) => Writer.uncons(p).flatMap:
      case Right((w, rest)) => loop(rest, k(Some(w)))
      case Left(_) => loop(p, k(None))

  loop(p, c)
}

/** by class only: `Await()` carries no trace of V, so a row may hold
 * ONE Take — see typeableKByClass */
given takeK[V]: TypeableK[Take % V] = typeableKByClass(classOf[Take[?, ?]])
