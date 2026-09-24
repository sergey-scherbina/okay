/**
 * okay2 — the okay core, written a second time in Scala 2.13
 * (specs/okay2.md). Not the facade (`okay-scala2`, which wraps the
 * Scala 3 library for a 2.13 caller): a full implementation of the
 * same tree, the same rotation, the same handlers, with nothing of
 * Scala 3 on its classpath.
 *
 * Its own sbt build (`okay2/build.sbt`), gated with
 * `cd okay2 && ../scripts/gate.sh test`.
 *
 * What is the same: the freer tree (`Return | Inject | Bind | Delay`),
 * `resume`'s four rotation cases, the split by class, widening as one
 * cast, `Cont` as an opaque facade over `Free[Shift, *]` with one-step
 * absorption, tail-resumptive `relay`, `translate`, the Cont-valued
 * `handle` with abort and multi-shot.
 *
 * What had to change, and why (each measured, specs/okay2.md):
 * - A ROW is a type of kind `*` with a member `Op[+A]` (`Row.scala`).
 *   Scala 2 cannot give a type ALIAS the kind `* -> *` by partial
 *   application, so `F + G` cannot be `[A] =>> F[A] | G[A]`; it is a
 *   Row whose `Op` is left abstract, which erases to Object exactly as
 *   the union does.
 * - A union does not commute here: `Member` has a `right` rule, and a
 *   program written at `A + B` lands in `B + A` by `.at[B + A]`.
 * - A signature is a Row with its operations in the companion —
 *   `sealed trait Console extends Row { type Op[+A] = Console.Op[A] }`
 *   — where Scala 3 writes `enum Console[+A] derives Effect`.
 * - A LONE operation (`Inject(e)` with no continuation) is handled as
 *   a Bind with a pure continuation: scalac 2 refines the existential
 *   answer type under a `Bind` and cannot at a bare `Inject`.
 * - No `inline`: the hot paths are ordinary methods for the JIT.
 */
package object okay2 {

  /** a computation of A performing the operations of the row R: A ! R.
   * Scala 2 gives every infix TYPE operator one precedence, left-
   * associative (measured: `S ! State % S` is `(S ! State) % S`), so a
   * row is parenthesised: `Int ! (State % Int + Console)`. */
  type ![A, R <: Row] = Free[R, A]

  /** a value as a computation */
  def pure[R <: Row, A](a: A): A ! R = Free.Return(a)

  /** an operation as a computation */
  def effect[R <: Row, A](e: R#Op[A]): A ! R = Free.Inject(e)

  /** the term-level name every `!.run` / `!.relay` call site spells
   * in the Scala 3 core; the object's own name is `Effects` */
  val ! : Effects.type = Effects

  // ---------------------------------------------------------- Cont

  /** the parameterised continuation monad, a facade over the tree:
   * `Cont[A, S, R]` means `(A => S) => R`. The type is ABSTRACT
   * outside `ContModule`, which is Scala 2's opaque type: only the
   * implementation sees `Free[Shift, A]` under it. */
  val Cont: ContModule = ContImpl
  type Cont[A, S, R] = Cont.Rep[A, S, R]

  /** A /> R is Cont[A, R, R] — the ordinary continuation monad, "A
   * delivered into the answer R" */
  type />[A, R] = Cont[A, R, R]
  /** what reset can delimit: the value and its inner answer coincide */
  type ^[A, R] = Cont[A, A, R]

  /** capture the current continuation (Danvy–Filinski, with answer-type modification) */
  def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = Cont.shift(f)
  /** delimit: run the computation with the identity continuation */
  def reset[A, R](c: A ^ R): R = Cont.run(c)(identity)

  /** the function encoding: the reference implementation of Control,
   * fast, fused, NOT stack-safe */
  type Func[A, S, R] = (A => S) => R

  // ------------------------------------------------------ handlers

  /** a handler of F with the answers S: an interpretation of F in the
   * continuation monad — handlers are continuations */
  type !>[F <: Row, S] = Interpr[F, S]

  /** fix the parameter of a binary signature: `State % S` IS `State[S]`.
   * Prefer the applied form in a row — `State[Int] + Writer[String]` —
   * because Scala 2 parses `A + B % C` as `(A + B) % C`. */
  type %[F[_], S] = F[S]

  /** the one-argument `Throws`: failure with nothing to say */
  type Abort = Throws[Unit]

  /** stop: nothing to answer with */
  def abort[A]: A ! Abort = Throws.raise[Unit, A](())

  implicit final class ProgOps[R <: Row, A](private val p: A ! R) extends AnyVal {
    /** land in the row R2, which must CONTAIN every signature of this
     * program's row — one cast, licensed by the witness (RowLift in the
     * Scala 3 core) */
    def at[R2 <: Row](implicit ev: Sub[R, R2]): A ! R2 = { val _ = ev; Member.coerce(p) }

    /** add G to whatever row this program already has: membership by
     * construction, no witness */
    def plus[G <: Row]: A ! (R + G) = Member.coerce(p)

    /** run every operation by a comonadic Handler */
    def runWith(implicit H: Handler[R]): A = Effects.runFree(p)

    /** THE rotation: normalize to a head form — `Return`, `Inject`, or
     * `Bind(Inject, k)` — in constant stack */
    def resume: A ! R = Free.resume(p)

    /** step through the next n operations by the Handler */
    def next(steps: Long = 1)(implicit H: Handler[R]): A ! R = Effects.next(p, steps)

    /** peek the nearest answer: the value, or the first operation handled */
    def peek(implicit H: Handler[R]): Any = Effects.peek(p)

    /** a bind across rows: the continuation may answer in ANOTHER row,
     * and the result is the union of the two */
    def bind[B, G <: Row](f: A => B ! G): B ! (R + G) =
      Member.coerce[A, R, R + G](p).flatMap(a => Member.coerce[B, G, R + G](f(a)))

    /** the same, the answer dropped: `p andThen q` runs p, then q */
    def andThen[B, G <: Row](q: => B ! G): B ! (R + G) =
      Member.coerce[A, R, R + G](p).flatMap(_ => Member.coerce[B, G, R + G](q))
  }

  /** the Throws recoveries, in the row rather than around it */
  implicit final class ThrowsOps[A, E, F <: Row](private val p: A ! (Throws[E] + F)) extends AnyVal {
    /** answer the failure, seeing the error */
    def recover(h: E => A ! (Throws[E] + F)): A ! (Throws[E] + F) =
      Throws.runEitherAt[A, E, F](p).at[Throws[E] + F].flatMap(_.fold(h, (a: A) => pure[Throws[E] + F, A](a)))

    /** answer the failure, ignoring the error */
    def orElse(q: => A ! (Throws[E] + F)): A ! (Throws[E] + F) = recover(_ => q)
  }

  implicit final class ContOps[A, S, R](private val c: Cont[A, S, R]) extends AnyVal {
    def flatMap[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R] = Cont.bind(c)(f)
    def map[B](f: A => B): Cont[B, S, R] = Cont.mapped(c)(f)
    /** apply to a continuation, as the function (A => S) => R it means */
    def /(k: A => S): R = Cont.run(c)(k)
  }

  implicit final class HandlerOps[F <: Row](private val h: Handler[F]) extends AnyVal {
    /** every handler can be a recording one: the operations are
     * already data, so recording is a decorator */
    def tracing(log: Any => Unit): Handler[F] = new Handler[F] {
      def handle[A](a: F#Op[A]): A = { log(a); h.handle(a) }
    }
  }
}
