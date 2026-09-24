package okay2

import scala.annotation.tailrec

/**
 * The freer monad (Kiselyov–Ishii 2015): free over any row R with no
 * Functor requirement, because Bind keeps the continuation as a plain
 * function. The same four nodes as the Scala 3 core's `enum Free`,
 * and the same rotation; also the tree under `Cont`.
 */
sealed abstract class Free[-R, +A] {
  /** sequencing is a data node: nothing runs until an interpreter walks the tree.
   * The continuation may need MORE than this program: the result needs both
   * (R1 <: R), and scalac finds that row itself */
  def flatMap[R1 <: R, B](f: A => Free[R1, B]): Free[R1, B] = Free.Bind[R1, A, B](this, f)

  def map[B](f: A => B): Free[R, B] = flatMap(a => Free.Return(f(a)))

  /**
   * the eliminator: p interprets values, h interprets operations
   * together with their continuations — over the head form `resume`
   * leaves. `h` receives the operation and its continuation at ONE
   * existential answer type, which is what a Scala 2 handler can use
   * without a cast (a lone `Inject` is given the pure continuation).
   */
  final def fold[R1 <: R, A1 >: A, B](p: A1 => B)(h: Free.Step[R1, A1, B]): B = Free.resume[R1, A1](this) match {
    case Free.Return(a) => p(a)
    case Free.Inject(e) => h(e, (x: A1) => Free.Return(x))
    case Free.Bind(Free.Inject(e), k) => h(e, k)
    case other => throw new IllegalStateException("resume left a non-head form: " + other)
  }
}

object Free {
  /** a finished computation */
  final case class Return[R, +A](a: A) extends Free[R, A]

  /** a single operation of the row R, held as `Any`: a row's `#Op` is
   * not a type to read at (Row.scala), so the typed view comes from
   * `Split`, at one signature, after its class test */
  final case class Inject[R, +A](a: Any) extends Free[R, A]

  /** sequencing: run a, then feed its value to the plain-function continuation f */
  final case class Bind[R, X, +A](a: Free[R, X], f: X => Free[R, A]) extends Free[R, A]

  /** a deferred subprogram: forced by the interpreter's loop and
   * continued AS IS — see `delay`; `defer` is this under a `Bind` */
  final case class Delay[R, +A](thunk: () => Free[R, A]) extends Free[R, A]

  /** what `fold` hands its handler: an operation with its continuation */
  trait Step[R, A, B] { def apply[X](e: Any, k: X => Free[R, A]): B }

  /** a value as a tree */
  def pure[R, A](a: A): Free[R, A] = Return(a)

  /** `Free[R, *]` is a Monad for every row R, with no constraint on R —
   * in Free's companion, so every query for a class of the hierarchy at
   * a program finds it with no import (Monad.scala) */
  implicit def monad[R <: Row]: Monad[({ type L[A] = Free[R, A] })#L] = new Monad[({ type L[A] = Free[R, A] })#L] {
    def pure[A](a: A): Free[R, A] = Return(a)
    def flatMap[A, B](a: Free[R, A])(f: A => Free[R, B]): Free[R, B] = a.flatMap[R, B](f)
    override def fmap[A, B](a: Free[R, A], f: A => B): Free[R, B] = a.map(f)
  }

  /** an operation as a tree */
  def inject[R <: Row, A](a: R#Op[A]): Free[R, A] = Inject[R, A](a)

  /** a bind whose LEFT side is deferred: the thunk is not forced at
   * construction, only when an interpreter's loop reaches this node —
   * what lets two mutually-recursive functions returning `A ! R` call
   * each other in tail position without a JVM frame per call */
  def defer[R, A, B](thunk: () => Free[R, A])(f: A => Free[R, B]): Free[R, B] =
    Bind(Delay(thunk), f)

  /** a deferred call with NOTHING to do afterwards — `!.tailcall`'s
   * node. Not `defer(thunk)(pure)`: that would push a `.flatMap(pure)`
   * tail down every bind of the deferred subprogram (delay-node) */
  def delay[R, A](thunk: () => Free[R, A]): Free[R, A] = Delay(thunk)

  /**
   * THE rotation: normalize to a head form — `Return(a)`, `Inject(e)`
   * or `Bind(Inject(e), k)` — in constant stack. Sound by the monad
   * associativity law, linear-time amortized for programs built by
   * `foldLeft`. A static loop rather than a member: scalac 2 refuses
   * `@tailrec` on a polymorphic member whose `this` type changes under
   * a GADT match ("it changes type of 'this' on a polymorphic
   * recursive call").
   */
  @tailrec def resume[R, A](p: Free[R, A]): Free[R, A] = p match {
    case Bind(Bind(a, f), g) => resume(Bind(a, (x: Any) => f(x).flatMap(g)))
    case Bind(Return(a), f) => resume(f(a))
    // the deferred subprogram is forced HERE, in the loop, and its own
    // binds then rotate through the cases above — constant stack
    case Delay(t) => resume(t())
    case Bind(Delay(t), g) => resume(Bind(t(), g))
    case a => a
  }
}
