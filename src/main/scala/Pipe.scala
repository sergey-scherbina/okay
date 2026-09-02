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

  /** emit one output — it answers nothing, like every tell */
  inline def tell[I, O](o: O): Unit ! (Take % I + Writer % O) =
    effect(Writer(o))

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
   * (docs/theory/03), here doing work: one Cont program whose state
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
      os.foldLeft(pure(()): Stage[I, O, Unit])((p, o) => p.flatMap(_ => tell[I, O](o)))

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
      os.foldLeft(pure(()): Stage[I, O, Unit])((p, o) => p.flatMap(_ => tell[I, O](o)))

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
  def chunked[T](size: Int): Stage[T, Chunk[T], Unit] =
    // named, not inlined into the `.map` below: as the RECEIVER of a
    // call the transduce gets no expected type, and then `I` has
    // nothing to be inferred from
    val batched: Stage[T, Chunk[T], Vector[T]] =
      transduce(Vector.empty[T])((buf, t) => {
        val b = buf :+ t
        if b.length < size then pure(b)
        else tell[T, Chunk[T]](ChunkBuf.ofSpecialized(b)).map(_ => Vector.empty[T])
      }, buf =>
        if buf.isEmpty then pure(buf)
        else tell[T, Chunk[T]](ChunkBuf.ofSpecialized(buf)).map(_ => buf))

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
 */
/**
 * The claims this file makes that the type system cannot check, named
 * once each rather than asserted thirty-three times.
 *
 * All of them are about what a `Bind` forgets. Under
 * `Bind(Effect(e), k)` an operation's answer type IS the bind's
 * intermediate, and that is existential — so `k` wants a value at a
 * type only the row's own invariant knows. `case Effect(e)` needs
 * none of this: GADT refinement gives the type back, which is why
 * these shapes appear only under a bind.
 */
private object Erased {
  /**
   * The argument a bind's continuation expects.
   *
   * Every remaining use is on the WRITER side, and that is the whole
   * explanation. `Take` is a GADT — `Await() extends Take[V,
   * Option[V]]` — so matching it refines the bind's intermediate and
   * sixteen of these disappeared without replacement when that was
   * checked. `Writer` has no constructor to match: it is
   * `opaque type Writer[W, +A] = W`, an operation IS its element, and
   * there is no case whose pattern could carry the equation.
   *
   * A type-level existential package does not help either, and the
   * reason is sharper than "it would allocate" — the path-dependent
   * encoding (`type Kind[K[_]] = { type A; type T = K[A] }`) is
   * purely type-level and allocates nothing at all.
   *
   * It does not help because `Bind` is ALREADY that package.
   * `Bind[F, X, A](op: F[X], k: X => Free[F, A])` relates the
   * operation and its continuation through one coherent `X`; nothing
   * needs relating. What is missing is the other thing, and reduced
   * to two lines the compiler says it exactly:
   *
   *     Found:    W
   *     Required: X
   *
   * That is `W =:= X`, which the identity signature's only injector
   * (`Writer(w): Writer[W, W]`) makes true and no pattern can
   * witness, because the operation HAS no constructor to match. A
   * Both of the article's encodings were tried here — the
   * path-dependent `type Kind[K[_]] = { type A; type T = K[A] }` and
   * the newtype `type Type[+F[_]] <: (Any { type T })` with its
   * `wrap`/`unwrap` — and both give exactly those two lines, because
   * they encode the same thing: an existential package NAMES an
   * unknown type so two things sharing it line up, and never reveals
   * it. That is what it is for.
   *
   * The positive half was checked too, so this is not a dismissal:
   * given a `Pair[F, G, A]` packed, `unwrap` hands both sides back at
   * one `v.T` and a natural transformation goes under it — `mapK`
   * works, exactly as the article says.
   *
   * It does not help HERE because `Free.Bind[F, X, A](op: F[X], k: X
   * => Free[F, A])` is that package already: two things sharing one
   * unknown `X`, related by an ordinary type parameter, and the
   * compiler does give a coherent `X`. Nothing needs packing. What is
   * missing is the opposite operation — revealing that the unknown
   * equals a known one — which an existential encoding exists
   * precisely NOT to do. A constructor to match supplies it, and an
   * identity signature has none.
   */
  def resumeWith[X](a: Any): X = a.asInstanceOf[X]

  /** an operation re-injected into a row at the answer type the
   * caller needs — sound because the identity signatures
   * (`Take % I`, `Writer % O`) fix that type by their only injector */
  def reinject[E](e: Any): E = e.asInstanceOf[E]

  /** a position that exists only to have a type: the upstream has
   * ended, and reading it would be the machine's bug — so it throws,
   * named, rather than handing out a null dressed as an A */
  def unreachable[A]: A = throw IllegalStateException("unreachable: the upstream has ended")
}

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
        case Right(Writer.Say(w)) => cont(Some(w), Free.Pure(()))
      case Bind(Effect(e), k) => <|>[Take % I, Writer % M](e) match
        case Left(Take.Await()) =>
          effect[Res, Option[I]](Take.Await()).flatMap(oi => pull(k(oi))(cont))
        case Right(Writer.Say(w)) => cont(Some(w), k(()))

  def loop(u: Stage[I, M, A], d: Stage[M, O, B]): B ! Res =
    (d.resume: @unchecked) match
      case Pure(b) => pure(b)
      case Effect(e) => <|>[Take % M, Writer % O](e) match
        case Left(Take.Await()) => pull(u)((om, _) => pure(om))
        case Right(o) => effect[Res, B](Erased.reinject[Res[B]](o))
      case Bind(Effect(e), k) => <|>[Take % M, Writer % O](e) match
        case Left(Take.Await()) => pull(u)((om, u2) => loop(u2, k(om)))
        case Right(o) =>
          effect[Res, Any](Erased.reinject[Res[Any]](o)).flatMap(x => loop(u, k(Erased.resumeWith(x))))

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
        case Left(Take.Await()) => pure(Erased.resumeWith[B](Writer.uncons(rest).toOption.map(_._1)))
        case Right(m) => effect[Writer % M, B](Erased.reinject[(Writer % M)[B]](m))
      case Bind(Effect(e), k) => <|>[Take % W, Writer % M](e) match
        case Left(Take.Await()) => Writer.uncons(rest) match
          case Right((w, r)) => loop(r, k(Erased.resumeWith(Some(w))))
          case Left(_) => loop(rest, k(None))
        case Right(m) =>
          effect[Writer % M, Any](Erased.reinject[(Writer % M)[Any]](m)).flatMap(x => loop(rest, k(Erased.resumeWith(x))))

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
          case Left(g) => effect[Res, Any](Erased.reinject[Res[Any]](g))
            .flatMap(_ => cont(None, Free.Pure(Erased.unreachable[A])))
          case Right(Writer.Say(w)) => cont(Some(w), Free.Pure(()))
      case Bind(Effect(e), k) => <|>[Take % I, Writer % M + G](e) match
        case Left(Take.Await()) =>
          effect[Res, Option[I]](Take.Await()).flatMap(oi => pull(k(oi))(cont))
        case Right(rest) => <|>[G, Writer % M](rest) match
          case Left(g) => effect[Res, Any](Erased.reinject[Res[Any]](g))
            .flatMap(x => pull(k(Erased.resumeWith(x)))(cont))
          case Right(Writer.Say(w)) => cont(Some(w), k(()))

  def loop(u: A ! Up, d: B ! (Take % M + (Writer % O + G))): B ! Res =
    (d.resume: @unchecked) match
      case Pure(b) => pure(b)
      case Effect(e) => <|>[Take % M, Writer % O + G](e) match
        case Left(Take.Await()) => pull(u)((om, _) => pure(om))
        case Right(o) => effect[Res, B](Erased.reinject[Res[B]](o))
      case Bind(Effect(e), k) => <|>[Take % M, Writer % O + G](e) match
        case Left(Take.Await()) => pull(u)((om, u2) => loop(u2, k(om)))
        case Right(o) =>
          effect[Res, Any](Erased.reinject[Res[Any]](o)).flatMap(x => loop(u, k(Erased.resumeWith(x))))

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
        case Left(g) => effect[Res, Any](Erased.reinject[Res[Any]](g))
          .flatMap(_ => cont(None, Free.Pure(Erased.unreachable[A])))
        case Right(Writer.Say(w)) => cont(Some(w), Free.Pure(()))
      case Bind(Effect(e), k) => <|>[G, Writer % W](e) match
        case Left(g) => effect[Res, Any](Erased.reinject[Res[Any]](g))
          .flatMap(x => pull(k(Erased.resumeWith(x)))(cont))
        case Right(Writer.Say(w)) => cont(Some(w), k(()))

  def loop(rest: A ! Src, d: B ! (Take % W + (Writer % M + G))): B ! Res =
    (d.resume: @unchecked) match
      case Pure(b) => pure(b)
      case Effect(e) => <|>[Take % W, Writer % M + G](e) match
        case Left(Take.Await()) => pull(rest)((ow, _) => pure(ow))
        case Right(o) => effect[Res, B](Erased.reinject[Res[B]](o))
      case Bind(Effect(e), k) => <|>[Take % W, Writer % M + G](e) match
        case Left(Take.Await()) => pull(rest)((ow, r2) => loop(r2, k(ow)))
        case Right(o) =>
          effect[Res, Any](Erased.reinject[Res[Any]](o)).flatMap(x => loop(rest, k(Erased.resumeWith(x))))

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
