package okay

import okay.cont.Handler

/**
 * Extensible effects: THE INTERFACE, over ROWS (specs/freer-min.md, stage 47). `Effects[M]` is what every encoding of
 * a program implements — the machine's `Free[R, A]` (okay-cont; the program `A ! R` of the core, Bang.scala) and
 * the classic tree under it (`okay.freer.Rowed`) — and what code generic in the encoding is written over. A row
 * is a nominal list of effects, `Ask +: Say +: Pure`, written either way (`Pure + Ask + Say`); an operation is
 * performed by its PATH in the row (`Member`, the compiler builds it), a handler takes its effect off the row
 * WHEREVER it is (`Removed`): the order of effects in a type says nothing, the order of handlers everything.
 * Handlers are the machine's: `Answering` answers in place (state, reader, writer), `Handler` has the
 * continuation (choose, dialogue); an encoding runs them its own way.
 */

/** the rows, the machine's, named at the core's door */
export okay.cont.{Row, +:, Pure, Union, Member, Removed, Sub, Tagged, Members}

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

object Effects:
  /** THE DEFAULT: the machine's program itself, `A ! R` — found with no import, as a companion's given is */
  given given_Effects_Free: Effects[okay.cont.Free] with
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
