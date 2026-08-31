package okay.java

import okay.{!, Async, Fold, async}
import okay.given
import java.util.function.{
  BiFunction, BinaryOperator, Consumer, Function as JFunction,
  Predicate, Supplier, UnaryOperator}

/**
 * `java.util.function` and this library.
 *
 * Most of it is a one-liner each way — a `Function[A, B]` is an
 * `A => B` and Scala 3's SAM conversion already goes one way by
 * itself. What is worth writing down is the two that are NOT
 * one-liners, because they are the ones with a semantics:
 *
 * `Supplier[A]` is a computation that has not run. That is not an
 * `A`; it is an `A ! Async` — deferring it is the whole point of the
 * type, and the row is where deferral lives here. Turning one into a
 * bare value at the boundary would run it there, which is exactly the
 * bug the type exists to prevent.
 *
 * `Consumer[A]` is a sink, and a sink over a stream is a `Fold`. So
 * consuming into one is `Fold`, not a loop with a side effect — which
 * means it composes, merges and distributes like every other fold in
 * this library.
 */
object Functions {

  // ----------------------------------------------------- the plain ones

  def fn[A, B](f: JFunction[A, B]): A => B = a => f.apply(a)
  def jfn[A, B](f: A => B): JFunction[A, B] = a => f(a)

  def pred[A](p: Predicate[A]): A => Boolean = a => p.test(a)
  def jpred[A](p: A => Boolean): Predicate[A] = a => p(a)

  def bifn[A, B, C](f: BiFunction[A, B, C]): (A, B) => C = (a, b) => f.apply(a, b)
  def jbifn[A, B, C](f: (A, B) => C): BiFunction[A, B, C] = (a, b) => f(a, b)

  def unary[A](f: UnaryOperator[A]): A => A = a => f.apply(a)
  def junary[A](f: A => A): UnaryOperator[A] = a => f(a)

  def binary[A](f: BinaryOperator[A]): (A, A) => A = (a, b) => f.apply(a, b)
  def jbinary[A](f: (A, A) => A): BinaryOperator[A] = (a, b) => f(a, b)

  // ------------------------------------------------------ the two that mean something

  /**
   * A Supplier as a PROGRAM, not as a value. `get()` may block, fail,
   * or observe the world; running it at the boundary would throw away
   * everything the type was saying. In the row it can be retried,
   * raced, timed out or run on a fiber like anything else.
   */
  def supply[A](s: Supplier[A]): A ! Async = async(s.get())

  /** and back: a program becomes a Supplier that runs it on `get` */
  def jsupply[A](p: A ! Async)(using okay.Handler[Async]): Supplier[A] =
    () => p.runWith

  /**
   * A Consumer as a `Fold` — a sink over a stream is exactly that, so
   * feeding one is the same operation as any other fold, and inherits
   * the same composition.
   */
  def sink[A](c: Consumer[A]): Fold[A, Unit] = new Fold[A, Unit]:
    def init: Unit = ()
    def add(s: Unit, a: A): Unit = c.accept(a)

  /** and back: a fold's step as a Consumer over a cell */
  def jsink[A, S](f: Fold[A, S]): (Collect.Cell[S], Consumer[A]) =
    val cell = Collect.Cell(f.init)
    (cell, (a: A) => cell.value = f.add(cell.value, a))
}
