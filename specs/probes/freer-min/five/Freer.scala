//> using scala 3.9.0
//> using options -Werror -Wunused:all -feature -deprecation
package okay.k5

type Pure = [S, R, A] =>> Nothing
infix type +[G[_, _, +_], H[_, _, +_]] = [S, R, A] =>> G[S, R, A] | H[S, R, A]
type Diag[F[+_]] = [S, R, A] =>> F[A]

/** a delimiter's identity is its LABEL, a literal type; it names the row of the context it delimits and its value */
final class Prompt[L <: String & Singleton, H[_, _, +_], S, Y](val label: L):
  override def toString: String = label
object Prompt:
  def apply[H[_, _, +_], S, Y]: Make[H, S, Y] = Make[H, S, Y]()
  final class Make[H[_, _, +_], S, Y]:
    def apply[L <: String & Singleton](label: L): Prompt[L, H, S, Y] = new Prompt(label)

/** the stack entry of a prompt: its label, with its row, answer index and value riding inside an inhabited program
 * type (a match type refuses an uninhabited selector, and `Pure` applied is `Nothing`) */
type Entry[L, H[_, _, +_], S, Y] = At[L, Freer[H, EmptyTuple, S, S, Y]]

/** an entry of the delimiter stack: the label, with the row and value riding inside an inhabited program type */
final class At[L, K]

/** `P` is on the stack `Σ`, `O` under it: a GADT the machine walks */
enum Has[Σ <: Tuple, P, O <: Tuple]:
  case Head[P, T <: Tuple]() extends Has[P *: T, P, T]
  case Tail[Q, T <: Tuple, P, O <: Tuple](rest: Has[T, P, O]) extends Has[Q *: T, P, O]
object Has:
  given head[P, T <: Tuple]: Has[P *: T, P, T] = Head()
  given tail[Q, T <: Tuple, P, O <: Tuple](using r: Has[T, P, O]): Has[Q *: T, P, O] = Tail(r)

/** the stack under `P` in `Σ`, on a concrete stack */
type Under[Σ <: Tuple, P] <: Tuple = Σ match
  case P *: t => t
  case ? *: t => Under[t, P]

/** the delimiter stack in force where a program is WRITTEN */
final class In[Σ <: Tuple]
given top: In[EmptyTuple] = In()

/**
 * `(A => S) => R`, or `R => (S, A)`, over the row `G`, under the delimiters `Σ`. Two indexes with two jobs: the
 * pair `S`, `R` is the answer type (a state, a protocol — what `Perform` moves), the stack `Σ` is which delimiters
 * are in force (what `Reset` pushes and `Shift0` captures to). Every node keeps `Σ` but `Reset`'s body.
 */
enum Freer[+G[_, _, +_], Σ <: Tuple, S, R, +A]:
  case Return[Σ <: Tuple, R, A](a: A) extends Freer[Pure, Σ, R, R, A]
  case Inject[G[_, _, +_], Σ <: Tuple, T, A](op: G[T, T, A]) extends Freer[G, Σ, T, T, A]
  case Perform[G[_, _, +_], Σ <: Tuple, S, R, A](op: G[S, R, A]) extends Freer[G, Σ, S, R, A]
  case Bind[G[_, _, +_], Σ <: Tuple, S, T, R, A, B](m: Freer[G, Σ, T, R, A], k: A => Freer[G, Σ, S, T, B]) extends Freer[G, Σ, S, R, B]
  case Delay[G[_, _, +_], Σ <: Tuple, S, R, A](t: () => Freer[G, Σ, S, R, A]) extends Freer[G, Σ, S, R, A]
  case Reset[H[_, _, +_], Σ <: Tuple, S, R, Y, L <: String & Singleton](
      p: Prompt[L, H, S, Y], body: Freer[H, At[L, Freer[H, EmptyTuple, S, S, Y]] *: Σ, S, R, Y]) extends Freer[H, Σ, S, R, Y]
  case Shift0[H[_, _, +_], Σ <: Tuple, O <: Tuple, S, T, R, X, Y, L <: String & Singleton](
      p: Prompt[L, H, S, Y], has: Has[Σ, At[L, Freer[H, EmptyTuple, S, S, Y]], O],
      f: (X => Freer[H, O, S, T, Y]) => Freer[H, O, S, R, Y]) extends Freer[H, Σ, T, R, X]
  case Resume[H[_, _, +_], X, O <: Tuple, S, T, Y](x: X, k: Captured[H, X, ?, T, O, S, Y, ?]) extends Freer[H, O, S, T, Y]

  def flatMap[H[_, _, +_], S2, B](f: A => Freer[H, Σ, S2, S, B]): Freer[G + H, Σ, S2, R, B] = Bind(this, f)
  def map[B](f: A => B): Freer[G, Σ, S, R, B] = Bind(this, a => Return(f(a)))

def pure[A, Σ <: Tuple, R](a: A): Freer[Pure, Σ, R, R, A] = Freer.Return(a)
def inject[F[+_], Σ <: Tuple, T, A](op: F[A]): Freer[Diag[F], Σ, T, T, A] = Freer.Inject[Diag[F], Σ, T, A](op)
def perform[G[_, _, +_], Σ <: Tuple, S, R, A](op: G[S, R, A]): Freer[G, Σ, S, R, A] = Freer.Perform(op)
def delay[G[_, _, +_], Σ <: Tuple, S, R, A](t: => Freer[G, Σ, S, R, A]): Freer[G, Σ, S, R, A] = Freer.Delay(() => t)
def defer[G[_, _, +_], Σ <: Tuple, S, T, R, A, B](t: => Freer[G, Σ, T, R, A])(f: A => Freer[G, Σ, S, T, B]): Freer[G, Σ, S, R, B] =
  Freer.Bind(Freer.Delay(() => t), f)

/** `reset_p body` at the prompt's answer index; the body sees the stack with `p` on top */
def reset[H[_, _, +_], Σ <: Tuple, S, Y, L <: String & Singleton](p: Prompt[L, H, S, Y])(using In[Σ])
         (body: In[At[L, Freer[H, EmptyTuple, S, S, Y]] *: Σ] ?=> Freer[H, At[L, Freer[H, EmptyTuple, S, S, Y]] *: Σ, S, S, Y]): Freer[H, Σ, S, S, Y] =
  Freer.Reset(p, body(using In()))

/** `shift0[X](p)(k => …)` at the prompt's answer index: `Σ` from the stack in force, the stack under `p` computed,
 * the witness searched last. No delimiter of `p` in force: no witness, a compile error */
def shift0[X]: Shift0At[X] = Shift0At[X]()
final class Shift0At[X]:
  def apply[H[_, _, +_], Σ <: Tuple, S, Y, L <: String & Singleton](p: Prompt[L, H, S, Y])(using In[Σ])
           (f: (X => Freer[H, Under[Σ, At[L, Freer[H, EmptyTuple, S, S, Y]]], S, S, Y]) => Freer[H, Under[Σ, At[L, Freer[H, EmptyTuple, S, S, Y]]], S, S, Y])
           (using has: Has[Σ, At[L, Freer[H, EmptyTuple, S, S, Y]], Under[Σ, At[L, Freer[H, EmptyTuple, S, S, Y]]]]): Freer[H, Σ, S, S, X] =
    Freer.Shift0[H, Σ, Under[Σ, At[L, Freer[H, EmptyTuple, S, S, Y]]], S, S, S, X, Y, L](p, has, f)

/** `ret $_p body`, derived: typed as `Bind(body, ret)` is, the delimiter between the two */
def dollar[H[_, _, +_], Σ <: Tuple, S, T, R, X, Y, L <: String & Singleton](p: Prompt[L, H, S, Y])(using In[Σ])(ret: X => Freer[H, Σ, S, T, Y])
          (body: In[At[L, Freer[H, EmptyTuple, S, S, Y]] *: Σ] ?=> Freer[H, At[L, Freer[H, EmptyTuple, S, S, Y]] *: Σ, T, R, X]): Freer[H, Σ, S, R, Y] =
  Freer.Reset(p, Freer.Bind(body(using In()),
    (x: X) => Freer.Shift0[H, At[L, Freer[H, EmptyTuple, S, S, Y]] *: Σ, Σ, S, S, T, Y, Y, L](p, Has.Head(), _ => ret(x))))
