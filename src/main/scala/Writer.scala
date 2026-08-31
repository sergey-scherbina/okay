package okay

/**
 * The Writer effect IS a stream: like Produce, the identity signature
 * — telling w is emitting w itself, no wrapper node, zero allocation —
 * but with the element type W kept SEPARATE from the program's answer:
 * A ! Writer % W reads "computes A, telling W". (Producer is the
 * diagonal cousin: it emits its own answers, one type for both.)
 * There is no Tell wrapper and no reinterpretation pass — a writer
 * program is already the stream; run is a fold over it, and
 * toLazyList consumes it lazily.
 *
 * The answer type of an operation is phantom, and the type is OPAQUE
 * so that the phantom is disciplined by the compiler: the only public
 * constructor is the diagonal Writer(w): Writer[W, W] — an operation
 * with answer = W is the only one that can exist, which is exactly
 * what the casts inside this file reassert (they cannot be proven
 * in place: the equation links an op to its continuation's erased
 * domain, so no runtime Typeable test could witness it — only a GADT
 * wrapper would, at an allocation per tell).
 *
 * The price of the identity representation: at run time a told String
 * is just a String, so when forwarding, the union is split by the
 * runtime class of W — forward only effects whose operations are
 * class-distinct from W, or handle them first.
 */
opaque type Writer[W, +A] = A match 
    case Nothing => 
    case _ => W & A

/**
 * An operation IS its element.
 *
 * Inside this file the opaque type is transparent, so this needs no
 * cast at all — and stating it here means the callers that take a
 * `Writer % W` operation out of a row (`Pipe.through`, `Pipe.into`)
 * do not each have to assert it with an `asInstanceOf`. The equation
 * belongs to the encoding, so it is published by the encoding.
 */
def out[W, A](w: Writer[W, A]): W = w

/**
 * THE THEOREM: a writer operation's answer is its element, `A = W`.
 *
 * `Writer(w): Writer[W, W]` is the only injector, so every operation
 * that can EXIST has `A = W`. The type system does not record it —
 * the alias makes `A` phantom, and that is the point, since a phantom
 * answer is what lets `tell` allocate nothing. So the equation is true
 * by construction and unprovable afterwards, and this is the single
 * place in the library that asserts it.
 *
 * Stating it as EVIDENCE rather than as a cast is what makes the
 * difference. An `asInstanceOf` at a use site says "this type is that
 * type" and offers no way to ask why; an `A =:= W` names the theorem,
 * so each site applies it by name and the compiler checks the
 * application. The assertion happens once, here, next to the argument
 * for it. `=:=` erases to the identity, so a single `refl` is minted
 * once and re-typed per use — the evidence costs nothing at run time.
 *
 * Why the alias stays `= W` and not `W & A`: an intersection would
 * make `answer` a subtyping step instead of a cast, which looks like
 * a strict improvement and is not. `A` is inferred, and it is
 * inferred as `Nothing` wherever the answer is unused — the compiler
 * then believes the value IS a `Nothing` and emits a checkcast to it,
 * which fails on the String actually there. Measured: 22 tests, and
 * publishing the bound as `<: A` fails the same way. It is the
 * `ChunkBuf` lesson again — `Array[?]` beat `Array[A]` because a type
 * that claims NOTHING about the parameter cannot be wrong about it.
 */
private val refl: Any =:= Any = summon[Any =:= Any]

def told[W, A]: A =:= W = refl.asInstanceOf[A =:= W]

/** the theorem, applied: an operation's answer is the element in it */
def answer[W, A](w: Writer[W, A]): A = told[W, A].flip(out(w))

/** an op is its element; the phantom answer is W: Writer(w) is the
 * only injector, and it fixes answer = W (the module invariant
 * behind this cast) */
given [W]: Handler[Writer % W] = new:
  def handle[A](e: Writer[W, A]): A = answer(e)

object Writer {

  /** the operation: telling w, answered by w — the ONLY constructor,
   * diagonal by its type, which is what seals the phantom discipline */
  inline def apply[W](w: W): Writer[W, W] = w

  /** tell w: emit it as an operation (whose answer is w itself) */
  inline def tell[W](w: W): W ! Writer % W = effect(Writer(w))

  /** ops of Writer % W are recognized by the runtime class of W
   * (outside this file the opaque type has no Typeable of its own) */
  given [W](using t: scala.reflect.Typeable[W]): TypeableK[Writer % W] = new:
    def unapply[A](x: Any): Option[x.type & Writer[W, A]] = t.unapply(x)

  import scala.annotation.tailrec
  import !.*

  /**
   * Fold everything told into any Fold algebra, forwarding the
   * effects F. Like State.handle, a bespoke tail-recursive loop — the
   * accumulator has to be threaded through the loop itself, which an
   * answer-polymorphic relay handler cannot hold.
   */
  def fold[W, S, A, F[+_]](a: A ! Writer % W + F)
                          (using TypeableK[Writer % W], Fold[W, S]): (S, A) ! F = {
    val K = summon[Fold[W, S]]

    def _loop(s: S)(x: A ! Writer % W + F): (S, A) ! F = loop(s)(x)

    @tailrec def loop(s: S)(x: A ! Writer % W + F): (S, A) ! F = (x.resume: @unchecked) match
      case Pure(a) => Pure((s, a))
      case Effect(e) => <|>[Writer % W, F](e) match
        case Left(w) => Pure((K.add(s, w), answer(w)))
        case Right(e) => Effect(e).map((s, _))
      case Bind(Effect(e), k) => <|>[Writer % W, F](e) match
        case Left(w) => loop(K.add(s, w))(k(answer(w)))
        case Right(e) => Effect(e).flatMap(x => _loop(s)(k(x)))

    loop(K.init)(a)
  }

  /** collect everything told, in order, forwarding the effects F */
  def run[W, A, F[+_]](a: A ! Writer % W + F)
                      (using TypeableK[Writer % W]): (Seq[W], A) ! F =
    fold[W, Seq[W], A, F](a)

  /**
   * The observation of the writer as codata: the same shape as
   * Stream.uncons but with a richer functor — Either[A, (W, rest)]
   * instead of Option[(W, rest)]. The told values come out one by one
   * through Right, on demand; when they end, Left carries the
   * program's own answer (nothing is lost — by then every told value
   * has already been observed). run/fold are this loop with a Fold
   * accumulating the Rights.
   */
  def uncons[W, A](a: A ! Writer % W): Either[A, (W, A ! Writer % W)] = (a.resume: @unchecked) match
    case Free.Pure(a) => Left(a)
    case Effect(e) => Right((e, Free.Pure(answer(e))))
    case Bind(Effect(e), k) => Right((e, k(answer(e))))

  /**
   * The same observation for a writer program performing ARBITRARY
   * effects G alongside its telling: the next told value arrives
   * inside G — the G-operations met on the way are carried into the
   * answer (deferred, not run). Any structured effect handler (State,
   * Reader, Throws, ...) forwards the telling, so it can be run over
   * the program FIRST — handlers are stream transformers — and the
   * Handler-able residue (Async, say) is what the consumer pays at
   * each pull. G is split from the told values by its runtime class.
   */
  def uncons[W, A, G[+_] : TypeableK](a: A ! Writer % W + G)
  : Either[A, (W, A ! Writer % W + G)] ! G = (a.resume: @unchecked) match
    case Free.Pure(a) => okay.pure(Left(a))
    case Effect(e) => <|>[G, Writer % W](e) match
      case Left(g) => Effect(g).map(Left(_))
      case Right(w) => okay.pure(Right((w, Free.Pure(answer(w)))))
    case Bind(Effect(e), k) => <|>[G, Writer % W](e) match
      case Left(g) => Effect(g).flatMap(x => uncons[W, A, G](k(x)))
      case Right(w) => okay.pure(Right((w, k(answer(w)))))
}

/** the diagonal writer: it tells its own answers, like Producer but
 * with the element type visible in the signature */
type Teller[A] = A ! Writer % A

/**
 * The third corner of the triangle: generate materializes into the
 * diagonal writer too — one unfold, three carriers (LazyList by pure
 * laziness, Producer by identity operations, Teller by typed ones).
 * put is tell as a delimited-control operation: shift captures the
 * continuation and binds it after the emission.
 */
given Put[Teller] with
  final override inline def put[A](a: A): A /> Teller[A] =
    shift(Writer.tell(a).flatMap(_))

/**
 * A writer program is a stream of its told values: the same
 * observation as Writer.uncons with the answer forgotten (Left becomes
 * the end) — an infinite teller unfolds on demand like any stream.
 */
given [A]: Stream[[W] =>> A ! Writer % W, Pure] = new:
  def uncons[W](s: A ! Writer % W): Option[(W, A ! Writer % W)] ! Pure =
    pure(Writer.uncons(s).toOption)

/**
 * Writer is the one parameterised signature whose split is COMPLETE:
 * `opaque type Writer[W, +A] = W`, so an operation IS its element at
 * runtime and testing the erasure tests W's own class. A row may hold
 * two writers — `Writer % String + Writer % Int` — and they route
 * correctly, which `TestRowIdentity` asserts. Class-distinct W is the
 * condition, and it is the same condition the identity encoding
 * already carries.
 */
given writerK[W](using t: scala.reflect.Typeable[W]): TypeableK[Writer % W] = new:
  def unapply[A](x: Any): Option[x.type & Writer[W, A]] =
    t.unapply(x).asInstanceOf[Option[x.type & Writer[W, A]]]

