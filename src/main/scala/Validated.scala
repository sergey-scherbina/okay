package okay

/**
 * EVERY ERROR, NOT THE FIRST (specs/validated.md).
 *
 * `Throws` is monadic, so it stops at the first error. That is right
 * for a computation whose later steps need the earlier ones' answers,
 * and wrong for the two cases this library meets most often: a
 * configuration being read and a form being filled. A user who
 * mistyped four fields should be told about four fields.
 *
 * The answer is the rung below the monad (theory ch. 12): an
 * APPLICATIVE cannot bind one leaf's answer into another's body, so
 * it has no way to stop early, so it collects. That is not a
 * limitation worked around — it is the reason this type can do what
 * `Either` cannot.
 *
 *   traverse(fields)(check)        // at Either: the first problem
 *   traverse(fields)(check)        // at Validated: all of them
 *
 * The program is the same. Every combinator already written against
 * `Applicative` — `traverse`, `sequence`, `replicateA`, `*>`, `<*` —
 * works here the day the instance exists, which is the whole argument
 * for having written them generically.
 *
 * NOT THE ONLY ACCUMULATOR HERE. `okay-codec`'s `Validate` walks a
 * schema with the same rule written by hand on `Either`, because it
 * predates this type and its errors carry paths. Both stay, and
 * `Validate.validated` reads a walk as this type (the bridge,
 * two-accumulating-validators, 2026-09-23), so a schema walk is one
 * leaf under `app`/`traverse` like any other.
 *
 * THERE IS DELIBERATELY NO `Monad[Validated]`. The monad-applicative
 * consistency law says `app` must agree with the `flatMap`
 * derivation, and that derivation stops at the first error — so a
 * Monad instance would quietly turn every `traverse` back into the
 * behaviour this type exists to refuse. When a later step really does
 * need an earlier answer, `andThen` says so at the call site, and the
 * type keeps its promise everywhere else.
 */
enum Validated[+E, +A]:
  case Valid(a: A)
  case Invalid(e: E)

object Validated:

  /** the value road */
  inline def valid[E, A](a: A): Validated[E, A] = Valid(a)

  /** the error road */
  inline def invalid[E, A](e: E): Validated[E, A] = Invalid(e)

  /** an Either read as a validation — the door in */
  def fromEither[E, A](e: Either[E, A]): Validated[E, A] = e match
    case Right(a) => Valid(a)
    case Left(err) => Invalid(err)

  /**
   * The instance, and `app` is the whole point: two `Invalid`s
   * COMBINE where `Either` would have kept the first and dropped the
   * second.
   *
   * `E` is a `Semigroup` and not a fixed `Seq`, so the caller decides
   * what accumulation means: concatenation for a form, a count for a
   * sampler, a `Map[Field, Seq[Problem]]` for an API. Fixing a
   * sequence would allocate one per leaf and settle a question three
   * consumers answer differently.
   */
  /**
   * The rung above, and it is a REAL one rather than `selectA`.
   *
   * Mokhov, Lukyanov, Marlow and Dimino's `select` runs the scrutinee
   * and then AT MOST ONE handler: a `Right` is already the answer, so
   * the handler is skipped and its effects never happen. For a
   * validator that is the difference between "the address is wrong"
   * being reported on an order that was never going to be shipped and
   * not being reported at all.
   *
   * A FAILED scrutinee does not run the handler either. That is the
   * reference implementation (Haskell's `selective` package, the
   * `Validation` instance) and it is the honest reading: the branch
   * that was going to be taken is not known, so there is nothing to
   * check yet.
   */
  given selective[E](using S: Semigroup[E]): Selective[[A] =>> Validated[E, A]] with
    def pure[A](a: A): Validated[E, A] = Valid(a)

    override def fmap[A, B](v: Validated[E, A], f: A => B): Validated[E, B] = v match
      case Valid(a) => Valid(f(a))
      case Invalid(e) => Invalid(e)

    extension [A, B](f: Validated[E, A => B])
      def app(a: Validated[E, A]): Validated[E, B] = (f, a) match
        case (Valid(g), Valid(x)) => Valid(g(x))
        case (Invalid(e1), Invalid(e2)) => Invalid(S.combine(e1, e2))
        case (Invalid(e1), _) => Invalid(e1)
        case (_, Invalid(e2)) => Invalid(e2)

    extension [A, B](e: Validated[E, Either[A, B]])
      override def select(f: => Validated[E, A => B]): Validated[E, B] = e match
        case Valid(Right(b)) => Valid(b)              // the handler is SKIPPED
        case Valid(Left(a)) => f match
          case Valid(g) => Valid(g(a))
          case Invalid(err) => Invalid(err)
        case Invalid(err) => Invalid(err)

  extension [E, A](v: Validated[E, A])
    /** the Either road back, for a caller that wants to branch */
    def toEither: Either[E, A] = v match
      case Valid(a) => Right(a)
      case Invalid(e) => Left(e)

    def isValid: Boolean = v match
      case Valid(_) => true
      case Invalid(_) => false

    /** the value, or a stand-in */
    def getOrElse[B >: A](b: => B): B = v match
      case Valid(a) => a
      case Invalid(_) => b

    /**
     * THE SHORT-CIRCUITING STEP, named so the reader sees it.
     *
     * It is `flatMap` in everything but the name, and the name is the
     * design: written as `flatMap` it would invite a `Monad` instance,
     * and that instance would make every `traverse` stop at the first
     * error by law. A call site that needs one answer to compute the
     * next says `andThen` and keeps the choice local.
     */
    def andThen[B](f: A => Validated[E, B]): Validated[E, B] = v match
      case Valid(a) => f(a)
      case Invalid(e) => Invalid(e)
