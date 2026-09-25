package okay2.stream

import scala.annotation.tailrec
import okay2._
import okay2.Free.{Return, Inject, Bind}

/**
 * The consumer side of a pipeline: await the next element of type V.
 * A consumer is a program with the Take effect — the exact dual of a
 * writer (tell pushes an element out, await pulls one in), and `pipe`
 * is the classic coroutine pairing of the two: no channel, no buffer,
 * no materialized list — each await transfers control to the producer
 * for exactly one element. The test is by CLASS only: a row may hold
 * ONE Take.
 */
sealed trait Take[V] extends Row { type Op[+A] = Take.Op[V, A] }

object Take {
  sealed trait Op[V, +A]
  /** the next element, or None — the producer has ended */
  final case class Await[V]() extends Op[V, Option[V]]

  implicit def effect[V]: Effect[Take[V]] = Effect.of[Take[V]]

  /** the next element, or None at the end of the input */
  def await[V]: Option[V] ! Take[V] = Free.inject[Take[V], Option[V]](Await())

  /** the input as a SOURCE: each step is one await, and the rest is this again */
  def each[V]: Pull[V, Take[V]] = new Pull[V, Take[V]] {
    def step: Option[(V, Pull[V, Take[V]])] ! Take[V] = await[V].map(_.map(v => (v, this)))
  }

  /** the iteratee a FoldUntil is: a consumer that asks for an element
   * only while its state has not seen enough */
  def foldUntil[W, S, R](fo: FoldUntil[W, S, R]): R ! Take[W] =
    Effects.loop[S, R, Take[W]](fo.init) { s =>
      if (fo.done(s)) pure(Right(fo.end(s)))
      else await[W].map {
        case Some(w) => Left(fo.add(s, w))
        case None => Right(fo.end(s))
      }
    }
}

/**
 * The coroutine pairings. The consumer drives — a finite consumer ends
 * an INFINITE producer; when the producer ends first, every further
 * await answers None. The consumer's answer is the result.
 *
 * NAMES, where the Scala 3 core overloads `through` and `pipe` by
 * `targetName`: Scala 2 cannot overload four methods whose parameters
 * all erase to `Free`, so each pairing has its own word — `pipe` and
 * `pipeIn` (producer into a consumer, pure and in G), `through` and
 * `throughIn` (stage after stage), `into` and `intoIn` (producer into
 * a stage).
 *
 * How deep the producer/stage handshake may recurse before it unwinds
 * itself: a stage that ACCUMULATES takes the drive path once per
 * element without emitting, so the chain is as deep as the run between
 * two emissions. Past the budget the loop answers with a deferred
 * program and the Scala stack unwinds.
 */
object Pipe {

  private val PullBudget = 256

  /** connect a producer to a consumer, one element per await */
  def pipe[W, A, B](p: Free[Writer[W], A])(c: Free[Take[W], B]): B = {
    @tailrec def loop(p: Free[Writer[W], A], c: Free[Take[W], B]): B = Free.resume(c) match {
      case Return(b) => b
      case Inject(Take.Await()) => Writer.uncons(p).toOption.map(_._1).asInstanceOf[B]
      case Bind(Inject(Take.Await()), k) => Writer.uncons(p) match {
        case Right((w, rest)) => loop(rest, k(Some(w)))
        case Left(_) => loop(p, k(None))
      }
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
    loop(p, c)
  }

  /** the same for a producer performing G between its tells: the
   * G-operations met between elements are carried into the answer */
  def pipeIn[W, A, B, G <: Row](p: Free[Writer[W] with G, A])(c: Free[Take[W], B]): B ! G = {
    def loop(p: Free[Writer[W] with G, A], c: Free[Take[W], B]): B ! G = Free.resume(c) match {
      case Return(b) => pure(b)
      case Inject(Take.Await()) => Writer.unconsIn[W, A, G](p).map(e => e.toOption.map(_._1).asInstanceOf[B])
      case Bind(Inject(Take.Await()), k) => Writer.unconsIn[W, A, G](p).flatMap {
        case Right((w, rest)) => loop(rest, k(Some(w)))
        case Left(_) => loop(p, k(None))
      }
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
    Free.delay(() => loop(p, c))
  }

  /**
   * Compose two stages, demand-driven: the downstream's awaits are fed
   * by the upstream's tells; the upstream's awaits become the composed
   * stage's awaits; when the upstream finishes, further downstream
   * awaits answer None. Nothing runs until the final consumer pulls —
   * the drive starts when the program RUNS (`Free.delay`), so a built
   * program is a value and each run starts the stages from their
   * first node.
   */
  def through[I, M, O, A, B](up: Stage[I, M, A])(down: Stage[M, O, B]): Stage[I, O, B] = {
    type Res = Take[I] + Writer[O]
    // the splits as patterns, made once per composition
    // (okay2-split-at-rest); a stage's row is exactly Take + Writer, so
    // what is not an await IS a tell (`Split.only`)
    val UpAwaits = Split.at[Take[I]]
    val DownAwaits = Split.at[Take[M]]

    // drive the upstream until it tells or ends; its own awaits
    // surface as OUR awaits, in CPS to stay a program
    def pull(u: Stage[I, M, A])(cont: (Option[M], Stage[I, M, A]) => B ! Res): B ! Res =
      Free.resume(u) match {
        case Return(_) => cont(None, u)
        case Inject(UpAwaits(_)) => cont(None, u)
        case Inject(e) => Split.only[Writer[M], Any](e) match {
          case Writer.Say(w) => cont(Some(w), Return(Writer.loneAnswer[A]))
        }
        case Bind(Inject(UpAwaits(_)), k) => Take.await[I].at[Res].flatMap(oi => pull(k(oi))(cont))
        case Bind(Inject(e), k) => Split.only[Writer[M], Any](e) match {
          case Writer.Say(w) => cont(Some(w), k(()))
        }
        case other => throw new IllegalStateException("resume left a non-head form: " + other)
      }

    def loop(u: Stage[I, M, A], d: Stage[M, O, B], depth: Int): B ! Res =
      Free.resume(d) match {
        case Return(b) => pure(b)
        case Inject(DownAwaits(_)) => pull(u)((om, _) => pure(om.asInstanceOf[B]))
        case Inject(o) => Free.Inject[Writer[O], B](o).at[Res]
        case Bind(Inject(DownAwaits(_)), k) =>
          if (depth >= PullBudget) pull(u)((om, u2) => pure[Res, Unit](()).flatMap(_ => loop(u2, k(om), 0)))
          else pull(u)((om, u2) => loop(u2, k(om), depth + 1))
        case Bind(Inject(o), k) => Free.Inject[Writer[O], Any](o).at[Res].flatMap(x => loop(u, k(x), 0))
        case other => throw new IllegalStateException("resume left a non-head form: " + other)
      }

    Free.delay(() => loop(up, down, 0))
  }

  /** run a plain producer through a stage: its tells feed the stage's
   * awaits, the stage's tells are the result stream */
  def into[W, M, A, B](p: Free[Writer[W], A])(s: Stage[W, M, B]): B ! Writer[M] = {
    val Awaits = Split.at[Take[W]]
    // a call from inside flatMap (or a by-name `++`) cannot be a jump; `again`
    // takes it, so the walk itself stays a checked loop (specs/stack-safety.md)
    def again(rest: Free[Writer[W], A], d: Stage[W, M, B]): B ! Writer[M] = loop(rest, d, 0)
    @tailrec def loop(rest: Free[Writer[W], A], d: Stage[W, M, B], depth: Int): B ! Writer[M] =
      Free.resume(d) match {
        case Return(b) => pure(b)
        case Inject(Awaits(_)) => pure(Writer.uncons(rest).toOption.map(_._1).asInstanceOf[B])
        case Inject(m) => Free.Inject[Writer[M], B](m)
        case Bind(Inject(Awaits(_)), k) =>
          if (depth >= PullBudget)
            pure[Writer[M], Unit](()).flatMap { _ =>
              Writer.uncons(rest) match {
                case Right((w, r)) => again(r, k(Some(w)))
                case Left(_) => again(rest, k(None))
              }
            }
          else Writer.uncons(rest) match {
            case Right((w, r)) => loop(r, k(Some(w)), depth + 1)
            case Left(_) => loop(rest, k(None), depth + 1)
          }
        case Bind(Inject(m), k) => Free.Inject[Writer[M], Any](m).flatMap(x => again(rest, k(x)))
        case other => throw new IllegalStateException("resume left a non-head form: " + other)
      }

    Free.delay(() => loop(p, s, 0))
  }

  /**
   * Compose two EFFECTFUL stages: both sides may perform G between
   * their awaits and tells; the G operations of either side forward
   * into the composed row in the order they are reached.
   */
  def throughIn[I, M, O, G <: Row, A, B](up: A ! (Take[I] + (Writer[M] + G)))
                                        (down: B ! (Take[M] + (Writer[O] + G))): B ! (Take[I] + (Writer[O] + G)) = {
    type Up = Take[I] + (Writer[M] + G)
    type Res = Take[I] + (Writer[O] + G)
    val UpAwaits = Split.at[Take[I]]
    val UpTells = Split.at[Writer[M]]
    val DownAwaits = Split.at[Take[M]]

    def pull(u: Free[Up, A])(cont: (Option[M], A ! Up) => B ! Res): B ! Res =
      Free.resume(u) match {
        case Return(_) => cont(None, u)
        case Inject(UpAwaits(_)) => cont(None, u)
        case Inject(UpTells(Writer.Say(w))) => cont(Some(w), Return(Writer.loneAnswer[A]))
        case Inject(g) => Free.Inject[G, A](g).at[Res].flatMap(_ => cont(None, Return(unreachable[A])))
        case Bind(Inject(UpAwaits(_)), k) => Take.await[I].at[Res].flatMap(oi => pull(k(oi))(cont))
        case Bind(Inject(UpTells(Writer.Say(w))), k) => cont(Some(w), k(()))
        case Bind(Inject(g), k) => Free.Inject[G, Any](g).at[Res].flatMap(x => pull(k(x))(cont))
        case other => throw new IllegalStateException("resume left a non-head form: " + other)
      }

    def loop(u: Free[Up, A], d: B ! (Take[M] + (Writer[O] + G)), depth: Int): B ! Res =
      Free.resume(d) match {
        case Return(b) => pure(b)
        case Inject(DownAwaits(_)) => pull(u)((om, _) => pure(om.asInstanceOf[B]))
        case Inject(o) => Free.Inject[Writer[O] + G, B](o).at[Res]
        case Bind(Inject(DownAwaits(_)), k) =>
          if (depth >= PullBudget) pull(u)((om, u2) => pure[Res, Unit](()).flatMap(_ => loop(u2, k(om), 0)))
          else pull(u)((om, u2) => loop(u2, k(om), depth + 1))
        case Bind(Inject(o), k) => Free.Inject[Writer[O] + G, Any](o).at[Res].flatMap(x => loop(u, k(x), 0))
        case other => throw new IllegalStateException("resume left a non-head form: " + other)
      }

    Free.delay(() => loop(up, down, 0))
  }

  /** an effectful producer through an effectful stage: the producer's
   * tells feed the stage's awaits, everyone's G ops forward, the
   * stage's tells are the result stream */
  def intoIn[W, M, G <: Row, A, B](p: Free[Writer[W] with G, A])
                                  (s: B ! (Take[W] + (Writer[M] + G))): B ! (Writer[M] + G) = {
    type Src = Writer[W] + G
    type Res = Writer[M] + G
    val Tells = Split.at[Writer[W]]
    val Awaits = Split.at[Take[W]]

    def pull(rest: Free[Src, A])(cont: (Option[W], A ! Src) => B ! Res): B ! Res =
      Free.resume(rest) match {
        case Return(_) => cont(None, rest)
        case Inject(Tells(Writer.Say(w))) => cont(Some(w), Return(Writer.loneAnswer[A]))
        case Inject(g) => Free.Inject[G, A](g).at[Res].flatMap(_ => cont(None, Return(unreachable[A])))
        case Bind(Inject(Tells(Writer.Say(w))), k) => cont(Some(w), k(()))
        case Bind(Inject(g), k) => Free.Inject[G, Any](g).at[Res].flatMap(x => pull(k(x))(cont))
        case other => throw new IllegalStateException("resume left a non-head form: " + other)
      }

    def loop(rest: Free[Src, A], d: B ! (Take[W] + (Writer[M] + G)), depth: Int): B ! Res =
      Free.resume(d) match {
        case Return(b) => pure(b)
        case Inject(Awaits(_)) => pull(rest)((ow, _) => pure(ow.asInstanceOf[B]))
        case Inject(o) => Free.Inject[Writer[M] + G, B](o)
        case Bind(Inject(Awaits(_)), k) =>
          if (depth >= PullBudget) pull(rest)((ow, r2) => pure[Res, Unit](()).flatMap(_ => loop(r2, k(ow), 0)))
          else pull(rest)((ow, r2) => loop(r2, k(ow), depth + 1))
        case Bind(Inject(o), k) => Free.Inject[Writer[M] + G, Any](o).flatMap(x => loop(rest, k(x), 0))
        case other => throw new IllegalStateException("resume left a non-head form: " + other)
      }

    Free.delay(() => loop(p, s, 0))
  }

  /** a position that exists only to have a type: the upstream has
   * ended after a lone G-operation, and reading it would be the
   * machine's bug — so it throws, named */
  private def unreachable[A]: A = throw new IllegalStateException("unreachable: the upstream has ended")
}
