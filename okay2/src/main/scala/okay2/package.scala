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
 *   application, so `F + G` cannot be `[A] =>> F[A] | G[A]`.
 * - `F + G` is `F with G` and `Free` is contravariant in the row
 *   (stage 8): the union of OPERATIONS is the intersection of
 *   REQUIREMENTS, so it commutes, widening is subtyping, and a handler
 *   names the rest of the row as a type parameter scalac infers.
 * - A signature is a Row with its operations in the companion —
 *   `sealed trait Console extends Row { type Op[+A] = Console.Op[A] }`
 *   — where Scala 3 writes `enum Console[+A] derives Effect`.
 * - A LONE operation (`Inject(e)` with no continuation) is handled as
 *   a Bind with a pure continuation: scalac 2 refines the existential
 *   answer type under a `Bind` and cannot at a bare `Inject`.
 * - No `inline`: the hot paths are ordinary methods for the JIT.
 */
package object okay2 extends Provides with Monads {

  /** a computation of A performing the operations of the row R: A ! R.
   * Scala 2 gives every infix TYPE operator one precedence, left-
   * associative (measured: `S ! State % S` is `(S ! State) % S`), so a
   * row is parenthesised: `Int ! (State % Int + Console)`. */
  type ![A, R] = Free[R, A]

  /** the union of two rows — their INTERSECTION as requirements: a
   * program in `F + G` may perform the operations of both, and needs a
   * handler for each. `with` commutes and associates up to subtyping,
   * so the order a row is written in does not matter, and `Free` is
   * contravariant in it, so widening is subtyping (stage 8).
   *
   * THE PARAMETER TRAP: scalac 2 does not look through this alias (or
   * `!`) to solve a row VARIABLE — a parameter `a: A ! (State[S] + R)`
   * solves `R` as the whole row. Every row-generic parameter in okay2
   * is spelled `Free[State[S] with R, A]`; results and concrete rows
   * use `!` and `+` freely. */
  type +[F <: Row, G <: Row] = F with G

  /** the empty requirement: no operations, so a computation over it is
   * PURE. It is `Row` itself, the TOP of the row order — every row is
   * below it, so `A ! Pure` is a program in any row, and `run` accepts
   * only it */
  type Pure = Row

  /** a value as a computation */
  def pure[R <: Row, A](a: A): A ! R = Free.Return[R, A](a)

  /** an operation as a computation */
  def effect[R <: Row, A](e: R#Op[A]): A ! R = Free.Inject[R, A](e)

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

  /** the eager encoding (see `EagerModule`): `Eager[F, A]` is abstract
   * here, `import Eager._` brings its `Effects` instance */
  val Eager: EagerModule = EagerImpl
  type Eager[F, A] = Eager.Rep[F, A]

  /** the tagless operations as methods, for a program at any encoding M
   * (a member of the same name wins, so `Free`'s own are untouched) */
  implicit final class EffectsSyntax[M[_, _], F <: Row, A](private val m: M[F, A]) extends AnyVal {
    def flatMap[B](f: A => M[F, B])(implicit E: Effects[M]): M[F, B] = E.flatMap(m)(f)
    def map[B](f: A => B)(implicit E: Effects[M]): M[F, B] = E.map(m)(f)
    def runWith(implicit E: Effects[M], H: Handler[F]): A = E.runWith(m)
  }

  /** a Loop: the body of an open-recursive function A => R whose
   * continuation is the recursive call (see `Generate`) */
  type Loop[A, R] = Cont[A, R, A => R]

  /** a program of productions (see `Produce`) */
  type Producer[A] = A ! Produce

  /** produce one value: an operation that IS its answer */
  def produce[A](a: A): A ! Produce = Produce.produce(a)

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

  /** one of the given alternatives (nondeterminism, see `Choose`) */
  def choose[A](as: A*): A ! Choose = Choose.choose(as: _*)

  /** all the results of all the branches, the rest of the row forwarded */
  def runChoice[A, R <: Row](a: Free[Choose with R, A]): Seq[A] ! R = Choose.runChoice[A, R](a)

  /**
   * Bracket over any Handler-able row F: acquire, use, release — the
   * use-program runs to completion inside one suspension, so no outer
   * handler can skip or repeat the release; a fiber's cancellation is
   * an interrupt exception, and the finally sees it. For a release
   * scoped to a whole program of arbitrary effects, use the `Resource`
   * effect instead.
   */
  def bracket[R, A, F <: Row](acquire: => R)(release: R => Unit)(use: R => A ! F)(implicit H: Handler[F]): A ! F =
    pure[F, Unit](()).flatMap { _ =>
      val r = acquire
      try pure[F, A](use(r).runWith)
      finally release(r)
    }

  /** typed tokens compared (see `Same`): `a === b` hands over the
   * witness `A =:= B`, `a =!= b` is the boolean, `a sameAs b` is `===`
   * by name — okay's top-level extensions */
  implicit final class SameOps[K[_], A](private val a: K[A]) extends AnyVal {
    def sameAs[B](b: K[B])(implicit s: Same[K]): Option[A =:= B] = s.same(a, b)
    def ===[B](b: K[B])(implicit s: Same[K]): Option[A =:= B] = s.same(a, b)
    def =!=[B](b: K[B])(implicit s: Same[K]): Boolean = s.same(a, b).isEmpty
  }

  implicit final class ProgOps[R <: Row, A](private val p: Free[R, A]) extends AnyVal {
    /** land in the row R2, which must CONTAIN every signature of this
     * program's row. Since stage 8 that is subtyping (`R2 <: R`), so this
     * is the identity: kept so a call site can NAME the row it wants */
    def at[R2 <: R]: A ! R2 = p

    /** add G to whatever row this program already has — the identity */
    def plus[G <: Row]: A ! (R + G) = p

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
    def bind[B, G <: Row](f: A => B ! G): B ! (R + G) = p.flatMap[R + G, B](f)

    /** the same, the answer dropped: `p andThen q` runs p, then q */
    def andThen[B, G <: Row](q: => Free[G, B]): B ! (R + G) = p.flatMap[R + G, B](_ => q)

    /*
     * The class syntax AT A PROGRAM (stage 13). The generic syntax in
     * Monad.scala reaches a program only when its static type is spelled
     * `Free[R, A]`: partial unification takes the `!` alias AS WRITTEN,
     * and `A ! R` has its parameters the other way round, so `F[A]`
     * against `Int ! State[Int]` solves `F = [R] Int ! R` and the kind
     * check refuses it (measured, TestMonad). These are the same
     * operations with the program's own shape, which a conversion
     * reaches through subtyping, where the alias does not matter — and
     * they win over the generic ones where both apply, being more
     * specific.
     */

    /** sequence, keep the right */
    def *>[B](q: Free[R, B]): B ! R = p.flatMap[R, B](_ => q)
    /** sequence, keep the left */
    def <*[B](q: Free[R, B]): A ! R = p.flatMap[R, A](a => q.map(_ => a))
    def >>=[B](f: A => Free[R, B]): B ! R = p.flatMap[R, B](f)
  }

  /** Selective's conditionals at a program of Boolean: ONE branch is
   * built and run (by name), as `Selective.ifS` */
  implicit final class ProgBoolOps[R <: Row](private val cond: Free[R, Boolean]) extends AnyVal {
    def ifS[A](t: => Free[R, A])(e: => Free[R, A]): A ! R = cond.flatMap[R, A](b => if (b) t else e)
    def whenS(body: => Free[R, Unit]): Unit ! R = ifS(body)(pure[R, Unit](()))
    def unlessS(body: => Free[R, Unit]): Unit ! R = ifS(pure[R, Unit](()))(body)
  }

  /** the Throws recoveries, in the row rather than around it */
  implicit final class ThrowsOps[A, E, F <: Row](private val p: Free[Throws[E] with F, A]) extends AnyVal {
    /** answer the failure, seeing the error */
    def recover(h: E => A ! (Throws[E] + F))(implicit d: Distinct[Throws[E] with F]): A ! (Throws[E] + F) =
      Throws.runEither[A, E, F](p).flatMap[Throws[E] + F, A](_.fold(h, (a: A) => pure[Throws[E] + F, A](a)))

    /** answer the failure, ignoring the error */
    def orElse(q: => Free[Throws[E] with F, A])(implicit d: Distinct[Throws[E] with F]): A ! (Throws[E] + F) = recover(_ => q)
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
      def handle[A](a: In[A]): A = handleOp[A](a)
      override def handleOp[A](op: Any): A = { log(op); h.handleOp[A](op) }
    }
  }
}
