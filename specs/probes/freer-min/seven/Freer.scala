//> using scala 3.9.0
//> using options -Werror -Wunused:all -feature -deprecation
package okay.k7

import scala.annotation.unused

type Pure = [S, R, A] =>> Nothing
infix type +[G[_, _, +_], H[_, _, +_]] = [S, R, A] =>> G[S, R, A] | H[S, R, A]

/** an entry of the delimiter stack: the delimiter's row, answer index and value, as the inhabited program type
 * `Freer[H, EmptyTuple, S, S, Y]`, wrapped invariantly. No name: the nearest delimiter is the head of the stack */
final class At[K]
type Entry[H[_, _, +_], S, Y] = At[Freer[H, EmptyTuple, S, S, Y]]

/** the delimiter stack in force where a program is WRITTEN: `reset` gives its body one, `shift0` reads it */
final class In[Σ <: Tuple]
given top: In[EmptyTuple] = In()

/**
 * `(A => S) => R`, or `R => (S, A)`, over the row `G`, under the delimiters `Σ`. The pair `S`, `R` is the answer
 * type (what an operation moves); the stack `Σ` says which delimiters are in force, and its HEAD is the one a
 * capture goes to. Seven nodes; every operation is three-place, on the diagonal or moving the index.
 */
enum Freer[+G[_, _, +_], Σ <: Tuple, S, R, +A]:
  case Return[Σ <: Tuple, R, A](a: A) extends Freer[Pure, Σ, R, R, A]
  case Perform[G[_, _, +_], Σ <: Tuple, S, R, A](op: G[S, R, A]) extends Freer[G, Σ, S, R, A]
  case Bind[G[_, _, +_], Σ <: Tuple, S, T, R, A, B](m: Freer[G, Σ, T, R, A], k: A => Freer[G, Σ, S, T, B]) extends Freer[G, Σ, S, R, B]
  case Delay[G[_, _, +_], Σ <: Tuple, S, R, A](t: () => Freer[G, Σ, S, R, A]) extends Freer[G, Σ, S, R, A]
  /** the delimiter: its body runs with it on the stack */
  case Reset[H[_, _, +_], Σ <: Tuple, S, R, Y](body: Freer[H, Entry[H, S, Y] *: Σ, S, R, Y]) extends Freer[H, Σ, S, R, Y]
  /** capture to the NEAREST delimiter, it included; `f(k)` goes on in its place, on the stack under it */
  case Shift0[H[_, _, +_], O <: Tuple, S, T, R, X, Y](f: (X => Freer[H, O, S, T, Y]) => Freer[H, O, S, R, Y])
    extends Freer[H, Entry[H, S, Y] *: O, T, R, X]
  case Resume[H[_, _, +_], X, O <: Tuple, S, T, Y](x: X, k: Captured[H, X, ?, T, O, S, Y]) extends Freer[H, O, S, T, Y]

  def flatMap[H[_, _, +_], S2, B](f: A => Freer[H, Σ, S2, S, B]): Freer[G + H, Σ, S2, R, B] = Bind(this, f)
  def map[B](f: A => B): Freer[G, Σ, S, R, B] = Bind(this, a => Return(f(a)))

def pure[A, Σ <: Tuple, R](a: A): Freer[Pure, Σ, R, R, A] = Freer.Return(a)
def perform[G[_, _, +_], Σ <: Tuple, S, R, A](op: G[S, R, A]): Freer[G, Σ, S, R, A] = Freer.Perform(op)
def delay[G[_, _, +_], Σ <: Tuple, S, R, A](t: => Freer[G, Σ, S, R, A]): Freer[G, Σ, S, R, A] = Freer.Delay(() => t)
def defer[G[_, _, +_], Σ <: Tuple, S, T, R, A, B](t: => Freer[G, Σ, T, R, A])(f: A => Freer[G, Σ, S, T, B]): Freer[G, Σ, S, R, B] =
  Freer.Bind(Freer.Delay(() => t), f)

/** a delimiter's TYPES, named once: its row, answer index and value. No identity — two delimiters of one kind are
 * told apart by position alone, the nearest wins — and nothing in the machine holds one; it only lets `reset`
 * and `shift0` infer, where a body's type cannot say what it is delimited by */
final class Delimiter[H[_, _, +_], S, Y]

/** `reset body` at the delimiter's answer index: the body sees the stack with the delimiter on top */
def reset[H[_, _, +_], Σ <: Tuple, S, Y](@unused d: Delimiter[H, S, Y])(using In[Σ])
         (body: In[Entry[H, S, Y] *: Σ] ?=> Freer[H, Entry[H, S, Y] *: Σ, S, S, Y]): Freer[H, Σ, S, S, Y] =
  Freer.Reset(body(using In()))

/** `shift0[X](d)(k => …)`: to the nearest delimiter, which the stack in force says is one of `d`'s kind; the body
 * is written inside it but runs outside it, so it sees the stack under it */
def shift0[X]: Shift0At[X] = Shift0At[X]()
final class Shift0At[X]:
  def apply[H[_, _, +_], O <: Tuple, S, Y](@unused d: Delimiter[H, S, Y])(using In[Entry[H, S, Y] *: O])
           (f: In[O] ?=> (X => Freer[H, O, S, S, Y]) => Freer[H, O, S, S, Y]): Freer[H, Entry[H, S, Y] *: O, S, S, X] =
    Freer.Shift0[H, O, S, S, S, X, Y](f(using In()))

/** `ret $ body`, derived: the body's value leaves the delimiter by an abort and meets `ret` outside */
def dollar[H[_, _, +_], Σ <: Tuple, S, T, R, X, Y](@unused d: Delimiter[H, S, Y])(using In[Σ])(ret: X => Freer[H, Σ, S, T, Y])
          (body: In[Entry[H, S, Y] *: Σ] ?=> Freer[H, Entry[H, S, Y] *: Σ, T, R, X]): Freer[H, Σ, S, R, Y] =
  Freer.Reset(Freer.Bind(body(using In()), (x: X) => Freer.Shift0[H, Σ, S, S, T, Y, Y](_ => ret(x))))
