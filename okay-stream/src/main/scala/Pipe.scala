package okay

import okay.std.*
import okay.freer.FoldUntil

/**
 * The consumer side of a pipeline: await the next element of type V.
 * A consumer is a program with the Take effect — the exact dual of a
 * writer (tell pushes an element out, await pulls one in), and pipe
 * below is the classic coroutine pairing of the two by delimited
 * continuations: no channel, no buffer, no materialized list — each
 * await transfers control to the producer for exactly one element.
 */
/**
 * PARAMETERISED, so the derived test is by CLASS only: the operations
 * carry no runtime trace of V, and a row may therefore hold ONE
 * Take. Two — `Take % Int + Take % String` — misroute, loudly
 * (TestRowIdentity): the first handler answers both asks and the
 * second continuation gets a ClassCastException, rather than a
 * plausible wrong answer.
 */
enum Take[V, +A] derives Effect:
  /** the next element, or None — the producer has ended */
  case Await[V]() extends Take[V, Option[V]]

object Take:
  /** the next element, or None at the end of the input */
  inline def await[V]: Option[V] ! Take % V = effect(Await())

  /** the input as a SOURCE (specs/direct-loops.md v3): `for i <-
   * Take.each[I] do …` in a direct block is the iteratee written as
   * a loop — each step is one `await`, and the rest is this again */
  def each[V]: Pull[V, Take % V] = new:
    def step: Option[(V, Pull[V, Take % V])] ! Take % V = await[V].map(_.map(v => (v, this)))

  /**
   * The iteratee a `FoldUntil` is (specs/fold-until.md, stage 3;
   * theory ch. 7): a consumer program that asks for an element only
   * while its state has not seen enough, and answers `end(s)` when it
   * has or when the input ends. Written over `!.loop` — the state is
   * the loop's, the `await` is the only effect — so `pipe(p)(Take.
   * foldUntil)` is `Writer.foldUntil(p)` by the coroutine road.
   */
  def foldUntil[W, S, R](using fo: FoldUntil[W, S, R]): R ! Take % W =
    // trampolined: each step re-enters through the program's flatMap
    def go(s: S): R ! Take % W =
      if fo.done(s) then pure(fo.end(s))
      else await[W].flatMap {
        case Some(w) => go(fo.add(s, w))
        case None => pure(fo.end(s))
      }
    defer(go(fo.init))

/**
 * Connect a producer to a consumer: each await meets the next told
 * value, control bouncing between the two programs one element at a
 * time. The consumer drives — a finite consumer ends an INFINITE
 * producer (only the asked elements are ever computed); when the
 * producer ends first, every further await answers None. The
 * consumer's answer is the result; the producer's surplus is dropped,
 * its answer ignored. Fully typed by the Take/Writer GADTs — no casts.
 */
def pipe[W, A, B](p: A ! Writer % W)(c: B ! Take % W): B =
  val u = Pulling.PureUp[W, A](p)
  c.handle(Handler.answering[Take % W]([X] => (op: Take[W, X]) => op match
    case Take.Await() => u.next())).run

/**
 * A pipeline stage (specs/stage-pipeline.md): a transducer as a
 * program — it awaits I and tells O, state is just its recursion
 * parameters. Tokenizers, parsers, codec dialects, any stream
 * rewriter share this one shape; composition (through) is
 * demand-driven coroutine pairing, so every stage is incremental,
 * resumable and lazy by construction.
 */
type Stage[I, O, A] = A ! Take % I + Writer % O

object Stage {

  /** the next input, or None — upstream ended */
  inline def await[I, O]: Option[I] ! Take % I + Writer % O =
    effect[Take % I, Option[I]](Take.Await()).at[Take % I + Writer % O]

  /** emit one output — it answers nothing, like every tell */
  inline def tell[I, O](o: O): Unit ! Take % I + Writer % O =
    Writer.tell(o).at[Take % I + Writer % O]

  /** the identity stage: every input becomes an output */
  def id[T]: Stage[T, T, Unit] =
    await[T, T].flatMap {
      case Some(t) => tell[T, T](t).flatMap(_ => id[T])
      case None => pure(())
    }

  /**
   * The transducer skeleton, named: carry a state, step it with each
   * input (telling whatever that input is worth), and flush at the end
   * of the input.
   *
   * Every stage in this library was writing this by hand — the
   * lexer's scanner, SSE framing, `chunked` below, the demo's stream
   * join — and each hand-written copy is a chance to forget the
   * recursion or the flush. The step ANSWERS the new state and is
   * itself a stage, so it may tell nothing, one, or many outputs
   * (which is the shape all four actually have); nothing is allocated
   * per element to say how many, and a step that needs another input
   * can simply await one.
   *
   * fs2's `mapAccumulate` is the 1:1 special case, spelled below.
   * The reason it is the special case and not the primitive is that
   * of the five stages written here, ZERO are one-output-per-input.
   *
   * `step` and `end` share ONE parameter list, and that is inference
   * rather than taste: in a third list (`(z)(step)(end)`) the
   * compiler solves the type variables after each list, so `I` — a
   * lambda PARAMETER type, which nothing before it constrains — is
   * committed to `Any` before the step's body is ever typed, and
   * every call site has to spell `[I, O, S]` out. In one list the
   * expected result type reaches them and every call below infers.
   * A default for `end` breaks it again for the same reason (the
   * default is elaborated at `Stage[Nothing, Nothing, S]`), so the
   * no-flush case passes `pure` explicitly — which reads as what it
   * is: nothing to flush.
   */
  def transduce[I, O, S](z: S)(step: (S, I) => Stage[I, O, S],
                               end: S => Stage[I, O, S]): Stage[I, O, S] =
    def go(s: S): Stage[I, O, S] = await[I, O].flatMap {
      case Some(i) => step(s, i).flatMap(go)
      case None => end(s)
    }

    go(z)

  /**
   * `transduce` whose STEP may end the stage (specs/fold-until.md,
   * stage 3): `Left(s)` carries on, `Right(r)` answers and the stage
   * ends there, so `through` pulls nothing more from upstream — a
   * prefix parser, a `takeWhile` with state, "the first n matches".
   * `end` is the answer when the input ends first, from the last
   * `Left` state. `transduce(z)(step, end)` is this with a step that
   * never answers `Right`; the two share one parameter list for the
   * inference reason `transduce` states.
   */
  def transduceUntil[I, O, S, R](z: S)(step: (S, I) => Stage[I, O, Either[S, R]],
                                       end: S => R): Stage[I, O, R] =
    def go(s: S): Stage[I, O, R] = await[I, O].flatMap {
      case Some(i) => step(s, i).flatMap {
        case Left(next) => go(next)
        case Right(r) => pure(r)
      }
      case None => pure(end(s))
    }

    go(z)

  /**
   * The stateful 1:1 map — fs2's `mapAccumulate`, for those arriving
   * with it in hand. It is `transduce` with the two degrees of
   * freedom that combinator has spent: exactly one output per input,
   * and nothing to flush.
   *
   * Where the emission is CONDITIONAL — the join in okay-demo, which
   * emits only on one of its two input shapes — this is the wrong
   * tool and fs2 shows why: its own version of that join has to emit
   * `Option[Output]` per element and filter downstream. `transduce`
   * emits nothing instead of emitting a None.
   */
  def mapAccumulate[I, O, S](z: S)(f: (S, I) => (S, O)): Stage[I, O, S] =
    transduce(z)((s, i) => {
      val (s2, o) = f(s, i)
      tell[I, O](o).map(_ => s2)
    }, pure)

  /**
   * The PHASED transducer (specs/stage-pipeline.md, stage-phased):
   * a stream with phases — a header before rows, a preamble before
   * frames — forces `transduce` to encode the phase as a sum in S,
   * and every step then carries branches for states illegal in its
   * phase. Here the accumulator CHANGES TYPE at the switch instead:
   * `head` runs at S1 and either stays (Left) or switches (Right)
   * carrying the S2 the body starts from; `body` runs at S2 and
   * cannot mention S1 — not by discipline, by type. Atkey's
   * parameterised composition applied to the pipeline.
   *
   * The per-input transition is EXECUTED through `PState` — the
   * type-changing state the theory chapter exhibits
   * (docs/theory/03), here doing work: one Cps program whose state
   * type goes S1 -> Either[S1, S2], `run` at every head input.
   *
   * Ends are honest both ways: input may end DURING the head, and
   * the answer says which phase the stream died in.
   */
  def phased[I, O, S1, S2](z: S1)(
      head: (S1, I) => Either[(S1, Vector[O]), (S2, Vector[O])],
      body: (S2, I) => (S2, Vector[O]),
      endHead: S1 => Vector[O],
      endBody: S2 => Vector[O]): Stage[I, O, Either[S1, S2]] =

    def tellAll(os: Vector[O]): Stage[I, O, Unit] =
      Pulling.tellAll[I, O](os, 0)

    // the switch, run through PState: the Atkey instance executed
    type R = (Either[S1, S2], Vector[O])
    def switch(s1: S1, i: I): R =
      PState.run[S1, Either[S1, S2], Vector[O]](s1):
        PState.get[S1, R].flatMap { s =>
          head(s, i) match
            case Left((ns, os)) =>
              PState.set[S1, Either[S1, S2], R](Left(ns)).map(_ => os)
            case Right((s2, os)) =>
              PState.set[S1, Either[S1, S2], R](Right(s2)).map(_ => os)
        }

    def inHead(s1: S1): Stage[I, O, Either[S1, S2]] = await[I, O].flatMap {
      case None => tellAll(endHead(s1)).map(_ => Left(s1))
      case Some(i) =>
        val (next, os) = switch(s1, i)
        tellAll(os).flatMap { _ =>
          next match
            case Left(ns) => inHead(ns)
            case Right(s2) => inBody(s2)
        }
    }

    def inBody(s2: S2): Stage[I, O, Either[S1, S2]] = await[I, O].flatMap {
      case None => tellAll(endBody(s2)).map(_ => Right(s2))
      case Some(i) =>
        val (ns, os) = body(s2, i)
        tellAll(os).flatMap(_ => inBody(ns))
    }

    inHead(z)

  /**
   * The three-phase sibling (specs/stage-pipeline.md, stage-phased3)
   * — one more arity, not a family: the http message shape
   * (request-line -> headers -> body) needs exactly three, and
   * chaining two `phased` cannot express it — the middle phase's end
   * is the third's TYPED start, and steps are functions, not stages.
   * Same guarantees at both seams: no phase enum, illegal states
   * unrepresentable, ends honest in all three phases (the answer
   * names the dying phase), each switch run through PState.
   */
  def phased3[I, O, S1, S2, S3](z: S1)(
      first: (S1, I) => Either[(S1, Vector[O]), (S2, Vector[O])],
      second: (S2, I) => Either[(S2, Vector[O]), (S3, Vector[O])],
      third: (S3, I) => (S3, Vector[O]),
      endFirst: S1 => Vector[O],
      endSecond: S2 => Vector[O],
      endThird: S3 => Vector[O]): Stage[I, O, Either[S1, Either[S2, S3]]] =

    def tellAll(os: Vector[O]): Stage[I, O, Unit] =
      Pulling.tellAll[I, O](os, 0)

    // each seam's switch runs through PState — the same executed
    // Atkey step as `phased`, at both type changes
    def switch[SA, SB](sa: SA, i: I,
                       step: (SA, I) => Either[(SA, Vector[O]), (SB, Vector[O])])
    : (Either[SA, SB], Vector[O]) =
      type R = (Either[SA, SB], Vector[O])
      PState.run[SA, Either[SA, SB], Vector[O]](sa):
        PState.get[SA, R].flatMap { s =>
          step(s, i) match
            case Left((ns, os)) => PState.set[SA, Either[SA, SB], R](Left(ns)).map(_ => os)
            case Right((sb, os)) => PState.set[SA, Either[SA, SB], R](Right(sb)).map(_ => os)
        }

    def inFirst(s1: S1): Stage[I, O, Either[S1, Either[S2, S3]]] = await[I, O].flatMap {
      case None => tellAll(endFirst(s1)).map(_ => Left(s1))
      case Some(i) =>
        val (next, os) = switch(s1, i, first)
        tellAll(os).flatMap { _ =>
          next match
            case Left(ns) => inFirst(ns)
            case Right(s2) => inSecond(s2)
        }
    }

    def inSecond(s2: S2): Stage[I, O, Either[S1, Either[S2, S3]]] = await[I, O].flatMap {
      case None => tellAll(endSecond(s2)).map(_ => Right(Left(s2)))
      case Some(i) =>
        val (next, os) = switch(s2, i, second)
        tellAll(os).flatMap { _ =>
          next match
            case Left(ns) => inSecond(ns)
            case Right(s3) => inThird(s3)
        }
    }

    def inThird(s3: S3): Stage[I, O, Either[S1, Either[S2, S3]]] = await[I, O].flatMap {
      case None => tellAll(endThird(s3)).map(_ => Right(Right(s3)))
      case Some(i) =>
        val (ns, os) = third(s3, i)
        tellAll(os).flatMap(_ => inThird(ns))
    }

    inFirst(z)

  /** batch inputs into chunks of the given size (the tail flushes on
   * end of input — a stage may still tell after seeing None) */
  /**
   * `inline` for one reason, and it is measured: `ChunkBuf` allocates
   * an UNBOXED array when the element type is concrete at the point
   * of expansion (its `summonFrom` finds a `ClassTag`), and a plain
   * `def` hides that type behind an abstract `T` so every element is
   * boxed on the way into the chunk. Profiling the chunked merge
   * found `boxToLong` among its named frames; inlining is what lets
   * `s.chunked(16)` on a `Source[Long]` fill an `Array[Long]`
   * (chunked-profile, 2026-09-03). A caller whose element type is
   * still abstract loses nothing — the fallback is the boxed array it
   * would have had anyway.
   *
   * The state starts EMPTY rather than holding a fresh buffer,
   * because a Stage is a VALUE and driving the same value twice must
   * not share one array between the runs; allocating on the first
   * element keeps each run's buffer its own.
   */
  inline def chunked[T](size: Int): Stage[T, Chunk[T], Unit] =
    val batched: Stage[T, Chunk[T], (ChunkBuf[T] | Null, Int)] =
      transduce[T, Chunk[T], (ChunkBuf[T] | Null, Int)]((null, 0))((acc, t) => {
        val (held, n) = acc
        val buf = if held == null then ChunkBuf[T](size) else held.nn
        buf.update(n, t)
        if n + 1 < size then pure((buf, n + 1))
        // the chunk leaves owning its array; the next one starts fresh
        else tell[T, Chunk[T]](buf.chunk).map(_ => (null, 0))
      }, acc => {
        val (held, n) = acc
        if n == 0 then pure(acc) else tell[T, Chunk[T]](held.nn.take(n)).map(_ => acc)
      })

    batched.map(_ => ())

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
 *
 * On the kernel (specs/freer-on-core.md): the downstream's `Take` is
 * TRANSLATED — each await a program that steps the upstream to its next
 * tell (`Writer.uncons`, the machine's head), its own awaits and other
 * effects performed where they are met. The upstream's rest is the
 * run's own state, made when the program RUNS (`defer`), so a built
 * program run twice starts its stages afresh
 * (windows-stage-rerun-loses-pane). The machine's stack is on the heap:
 * no `PullBudget` any more.
 */
private[okay] object Pulling:
  /** a pure upstream, stepped one tell a pull; ended, every pull answers None */
  final class PureUp[W, A](p: A ! Writer % W):
    private var rest = p
    private var ended = false
    def next(): Option[W] =
      if ended then None
      else Writer.uncons(rest) match
        case Right((w, r)) => rest = r; Some(w)
        case Left(_) => ended = true; None

  /** an effectful upstream: a pull steps it to its next tell, its other operations performed on the way */
  final class Up[W, A, G[_]](p: A ! Writer % W + G):
    private var rest = p
    private var ended = false
    def next: Option[W] ! G =
      if ended then pure(None)
      else Writer.uncons[W, A, G](rest).map:
        case Right((w, r)) => rest = r; Some(w)
        case Left(_) => ended = true; None

  /** tell each of `os`, in order — trampolined through flatMap */
  def tellAll[I, O](os: Vector[O], i: Int): Stage[I, O, Unit] =
    if i >= os.length then pure(())
    else Stage.tell[I, O](os(i)).flatMap(_ => tellAll[I, O](os, i + 1))

def through[I, M, O, A, B](up: Stage[I, M, A])(down: Stage[M, O, B]): Stage[I, O, B] =
  defer:
    val u = Pulling.Up[M, A, Take % I](up.at[Writer % M + Take % I])
    down.translate[Take % M, Take % I]([X] => (op: Take[M, X]) => op match
      case Take.Await() => u.next)

/** run a plain producer through a stage: its tells feed the stage's
 * awaits, the stage's tells are the result stream */
@scala.annotation.targetName("throughProducer")
def through[W, M, A, B](p: A ! Writer % W)(s: Stage[W, M, B]): B ! Writer % M =
  defer:
    val u = Pulling.PureUp[W, A](p)
    s.handle(Handler.answering[Take % W]([X] => (op: Take[W, X]) => op match
      case Take.Await() => u.next()))

/**
 * Compose two EFFECTFUL stages: both sides may perform arbitrary
 * effects G (Async above all) between their awaits and tells; the G
 * operations of either side are performed in the order they are
 * reached. Same demand-driven pairing as the pure through — nothing
 * runs until the final consumer pulls, and a G op runs only when the
 * pull actually crosses it.
 */
@scala.annotation.targetName("throughG")
def through[I, M, O, G[_], A, B](up: A ! (Take % I + (Writer % M + G)))
                                (down: B ! (Take % M + (Writer % O + G)))
                                : B ! (Take % I + (Writer % O + G)) =
  defer:
    val u = Pulling.Up[M, A, Take % I + G](up.at[Writer % M + (Take % I + G)])
    down.translate[Take % M, Take % I + G]([X] => (op: Take[M, X]) => op match
      case Take.Await() => u.next).at[Take % I + (Writer % O + G)]

/**
 * An effectful producer through an effectful stage: the producer's
 * tells feed the stage's awaits, everyone's G ops are performed, the
 * stage's tells are the result stream — the generalization the LLM
 * client walks by hand (SSE lines ! Async through the event stage).
 */
@scala.annotation.targetName("throughProducerG")
def through[W, M, G[_], A, B](p: A ! Writer % W + G)
                             (s: B ! (Take % W + (Writer % M + G)))
                             : B ! Writer % M + G =
  defer:
    val u = Pulling.Up[W, A, G](p)
    s.translate[Take % W, G]([X] => (op: Take[W, X]) => op match
      case Take.Await() => u.next).at[Writer % M + G]

/**
 * The same pipe for a producer performing arbitrary effects G: the
 * consumer still drives, and the G-operations met between elements
 * are carried into the answer — the result is a program in G.
 */
def pipe[W, A, B, G[_]](p: A ! Writer % W + G)(c: B ! Take % W): B ! G =
  defer:
    val u = Pulling.Up[W, A, G](p)
    c.translate[Take % W, G]([X] => (op: Take[W, X]) => op match
      case Take.Await() => u.next).at[G]
