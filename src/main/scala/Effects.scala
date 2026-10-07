package okay

import okay.cont.Handler
import scala.language.implicitConversions

/**
 * Extensible effects: THE INTERFACE, over ROWS, AND THE FACADE (specs/freer-min.md, stages 47–48). `Effects[M]` is
 * what every encoding of a program implements — the machine's `Free[R, A]` (okay-cont) and the classic tree
 * under it (`okay.freer.Rowed`) — and what code generic in the encoding is written over. A row is a nominal
 * list of effects, `Ask +: Say +: Pure`, written either way (`Pure + Ask + Say`); an operation is performed by
 * its PATH in the row (`Member`, the compiler builds it), a handler takes its effect off the row WHEREVER it is
 * (`Removed`): the order of effects in a type says nothing, the order of handlers everything. Handlers are the
 * machine's: `Answering` answers in place (state, reader, writer), `Handler` has the continuation (choose,
 * dialogue); an encoding runs them its own way.
 *
 * THE WORDS A PROGRAM IS WRITTEN IN ARE MEMBERS OF THE INSTANCE (the operator, stage 48: the facade defined once,
 * as aliases over the typeclass, dispatched statically): `A ! R`, `effect`, `op.perform`, `p.handle(h)`,
 * `p.value`, each an alias over the primitives above, so they name the REAL types of one encoding and no other.
 * Which encoding is the IMPORT's choice: `Effects.machine` is the machine's (`okay.*` exports it, the default),
 * `okay.freer.tree` the tree's, a third encoding's is its given. The rows are one vocabulary, below.
 */

/** the rows, the machine's, named at the core's door: aliases, not an `export` — a top-level export here and the
 * facade's (`export machine.*`, Facade.scala) resolve against each other and the compiler drops both (stage 48) */
type Row = okay.cont.Row
infix type +:[E[+_], T <: Row] = okay.cont.+:[E, T]
infix type +[R <: Row, E[+_]] = okay.cont.+[R, E]
infix type %[F[_, +_], S] = okay.cont.%[F, S]
type Pure = okay.cont.Pure
type Union[R <: Row, X] = okay.cont.Union[R, X]
type Member[E[+_], R <: Row] = okay.cont.Member[E, R]
type Removed[E[+_], R <: Row] = okay.cont.Removed[E, R]
type Sub[R1 <: Row, R2 <: Row] = okay.cont.Sub[R1, R2]
type Tagged[R <: Row, +X] = okay.cont.Tagged[R, X]
val Tagged: okay.cont.Tagged.type = okay.cont.Tagged
type Members[R <: Row] = okay.cont.Members[R]
val Members: okay.cont.Members.type = okay.cont.Members

trait Effects[M[_ <: Row, _]]:
  def pure[R <: Row, A](a: A): M[R, A]
  /** an operation, by its path in the row */
  def perform[E[+_], R <: Row, X](op: E[X])(using Member[E, R]): M[R, X]
  /** a bind whose left side is deferred, forced only when the encoding's interpreter reaches it, so that
   * mutually recursive functions returning `M[R, A]` call each other in tail position with no JVM frame each */
  def defer[R <: Row, A, B](thunk: () => M[R, A])(f: A => M[R, B]): M[R, B]
  /** a tail call to a mutually recursive function, for code written over any `M: Effects` */
  def tailcall[R <: Row, A](thunk: => M[R, A]): M[R, A] = defer(() => thunk)(pure)

  extension [R <: Row, A](m: M[R, A])
    /** at the one row: a program's row is declared, its operations find their paths in it */
    def flatMap[B](f: A => M[R, B]): M[R, B]
    inline def map[B](f: A => B): M[R, B] = m.flatMap(a => pure(f(a)))

  /** the effect `E` handled, wherever it is in the row; the result over the row without it. A handler that
   * answers in place (`Answering`, `h.inPlace`) takes the road with no delimiter */
  def handle[E[+_], A, Ans, R <: Row](h: Handler[E, A, Ans])(m: M[R, A])(using rm: Removed[E, R]): M[rm.Out, Ans]
  /** a program with nothing left to handle, to its value */
  def run[A](m: M[Pure, A]): A

  // THE FACADE: the words, each an alias over the primitives above

  /** a program of `A` over the row `R`: `Int ! (State % Int +: Say +: Pure)`, `Int ! (Pure + State % Int + Say)` */
  infix type ![A, R <: Row] = M[R, A]

  /** an operation as a program, over any row that has its effect (`Op`): the row is the program's it is bound into */
  def effect[F[+_], X](op: F[X]): Op[F, X] = Op(op)

  extension [F[+_], X](op: F[X])
    /** an operation performed, postfix: `State.Get[Int]().perform` */
    def perform: Op[F, X] = Op(op)

  extension [R <: Row, A](p: M[R, A])
    /** the effect handled, wherever it is in the row: `p.handle(State(0))` */
    def handle[F[+_], Ans](h: Handler[F, A, Ans])(using rm: Removed[F, R]): M[rm.Out, Ans] = this.handle[F, A, Ans, R](h)(p)

  extension [A](p: M[Pure, A])
    /** a program with nothing left to handle, run: its value */
    def value: A = run(p)

  /**
   * AN OPERATION AS A PROGRAM OVER ANY ROW THAT HAS ITS EFFECT: the row is not the operation's to say — it is the
   * program's it is bound into, so `flatMap` and `map` take it from the expected type (`Member`, the path the
   * compiler builds), as the classic's `effect` took its signature from the context; a bare operation becomes a
   * program by the same path where one is expected (`at`, or the conversion). A `for` over mixed effects declares
   * the program's row and nothing else.
   */
  final class Op[F[+_], X](val op: F[X]):
    def flatMap[R <: Row, B](f: X => M[R, B])(using m: Member[F, R]): M[R, B] = perform[F, R, X](op).flatMap(f)
    def map[R <: Row, B](f: X => B)(using m: Member[F, R]): M[R, B] = perform[F, R, X](op).map(f)
    /** the operation as a program at a row, written */
    def at[R <: Row](using m: Member[F, R]): M[R, X] = perform[F, R, X](op)

  /** a bare operation where a program over `R` is expected */
  given opToProgram[F[+_], X, R <: Row](using m: Member[F, R]): Conversion[Op[F, X], M[R, X]] = _.at[R]

object Effects:
  /** THE DEFAULT: the machine's program itself — the instance found with no import, as a companion's given is,
   * and the facade `okay.*` exports (below) */
  given machine: Effects[okay.cont.Free] with
    def pure[R <: Row, A](a: A): okay.cont.Free[R, A] = okay.cont.Free.pure(a)
    def perform[E[+_], R <: Row, X](op: E[X])(using m: Member[E, R]): okay.cont.Free[R, X] = okay.cont.Free.inject(op).at[R]
    def defer[R <: Row, A, B](thunk: () => okay.cont.Free[R, A])(f: A => okay.cont.Free[R, B]): okay.cont.Free[R, B] =
      okay.cont.Free.defer(thunk)(f)
    override def tailcall[R <: Row, A](thunk: => okay.cont.Free[R, A]): okay.cont.Free[R, A] = okay.cont.Free.delay(() => thunk)
    extension [R <: Row, A](m: okay.cont.Free[R, A])
      def flatMap[B](f: A => okay.cont.Free[R, B]): okay.cont.Free[R, B] = m.flatMap(f)
    def handle[E[+_], A, Ans, R <: Row](h: Handler[E, A, Ans])(m: okay.cont.Free[R, A])(using rm: Removed[E, R]): okay.cont.Free[rm.Out, Ans] =
      h.inPlace match
        case Some(a) => okay.cont.Free.handle(a)(m)
        case None => okay.cont.Free.handle(h)(m)
    def run[A](m: okay.cont.Free[Pure, A]): A = okay.cont.Machine.value(okay.cont.Free.top(m))

  /** any encoding in direct style: `M[R, *]` as a monad, for `direct[[A] =>> M[R, A]]` over `Effects[M]` */
  def monad[M[_ <: Row, _], R <: Row](using E: Effects[M]): Monad[[A] =>> M[R, A]] = new Monad[[A] =>> M[R, A]]:
    def pure[A](a: A): M[R, A] = E.pure(a)
    extension [A](a: M[R, A])
      def flatMap[B](f: A => M[R, B]): M[R, B] = E.flatMap(a)(f)

  /** the instance in scope, by its encoding: `Effects[Free]` */
  inline def apply[M[_ <: Row, _]](using E: Effects[M]): E.type = E

/** `import okay.*` is the machine's facade: `A ! R`, `effect`, `op.perform`, `p.handle(h)`, `p.value`, `Op` */
export Effects.machine.{given, *}
