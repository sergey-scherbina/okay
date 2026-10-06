//> using scala 3.9.0
//> using options -Werror -Wunused:all -feature -deprecation
package okay.ts

/** rows are UNARY: an operation carries no index */
type Pure = [A] =>> Nothing
infix type +[G[+_], H[+_]] = [A] =>> G[A] | H[A]

/** a delimiter's identity is its LABEL, a literal type: `Prompt("p")` is `Prompt["p", H, Y]`. Two prompts of one
 * label are one delimiter, by type; the machine never looks at a prompt value. It names the row of the context it
 * delimits and its value */
final class Prompt[L <: String & Singleton, H[+_], Y](val label: L):
  override def toString: String = label
object Prompt:
  def apply[H[+_], Y]: Make[H, Y] = Make[H, Y]()
  final class Make[H[+_], Y]:
    def apply[L <: String & Singleton](label: L): Prompt[L, H, Y] = new Prompt(label)

/** what the index holds for a delimiter in force: the prompt's label with its row and value, so a delimiter found
 * by the witness has, by GADT, the row and value of the prompt captured to. The row and value ride inside a
 * program type, which is INHABITED whatever the row (`Return`): a match type refuses an uninhabited selector, and
 * `Pure` applied is `Nothing` */
final class At[L, K]

/** the stack under the delimiter `P` in `S`: computed on a concrete stack (labels are literal types, so a head that
 * is not `P` is PROVABLY not `P`); the machine, on an abstract stack, walks the `Has` witness instead */
type Under[S <: Tuple, P] <: Tuple = S match
  case P *: t => t
  case ? *: t => Under[t, P]

/** `P` is on the stack `S`, `O` is the stack under it. A GADT: the machine WALKS it, and the stack's typing follows */
enum Has[S <: Tuple, P, O <: Tuple]:
  case Head[P, T <: Tuple]() extends Has[P *: T, P, T]
  case Tail[Q, T <: Tuple, P, O <: Tuple](rest: Has[T, P, O]) extends Has[Q *: T, P, O]
object Has:
  given head[P, T <: Tuple]: Has[P *: T, P, T] = Head()
  given tail[Q, T <: Tuple, P, O <: Tuple](using r: Has[T, P, O]): Has[Q *: T, P, O] = Tail(r)

/**
 * The freer monad whose ONE INDEX IS THE PROMPT STACK, a tuple of prompt singleton types. Every node keeps it
 * but `Reset`, whose body runs with its prompt pushed; `Shift0` needs the witness that its prompt is on the
 * stack and runs its body on the stack under that prompt. A capture to a prompt with no delimiter does not
 * typecheck. There is no answer-type modification here: that is the other job an index can do, and this
 * variant gives the index to the prompts alone.
 */
enum Freer[+G[+_], S <: Tuple, +A]:
  case Return[S <: Tuple, A](a: A) extends Freer[Pure, S, A]
  case Inject[G[+_], S <: Tuple, A](op: G[A]) extends Freer[G, S, A]
  case Bind[G[+_], S <: Tuple, A, B](m: Freer[G, S, A], k: A => Freer[G, S, B]) extends Freer[G, S, B]
  case Delay[G[+_], S <: Tuple, A](t: () => Freer[G, S, A]) extends Freer[G, S, A]
  case Reset[H[+_], S <: Tuple, Y, L <: String & Singleton](p: Prompt[L, H, Y], body: Freer[H, At[L, Freer[H, EmptyTuple, Y]] *: S, Y]) extends Freer[H, S, Y]
  case Shift0[H[+_], S <: Tuple, X, Y, L <: String & Singleton, O <: Tuple](
      p: Prompt[L, H, Y], has: Has[S, At[L, Freer[H, EmptyTuple, Y]], O], f: (X => Freer[H, O, Y]) => Freer[H, O, Y]) extends Freer[H, S, X]
  case Resume[H[+_], X, O <: Tuple, Y](x: X, k: Captured[H, X, ?, O, Y, ?]) extends Freer[H, O, Y]

  def flatMap[H[+_], B](f: A => Freer[H, S, B]): Freer[G + H, S, B] = Bind(this, f)
  def map[B](f: A => B): Freer[G, S, B] = Bind(this, a => Return(f(a)))

def pure[A, S <: Tuple](a: A): Freer[Pure, S, A] = Freer.Return(a)
def inject[F[+_], S <: Tuple, A](op: F[A]): Freer[F, S, A] = Freer.Inject(op)
def delay[G[+_], S <: Tuple, A](t: => Freer[G, S, A]): Freer[G, S, A] = Freer.Delay(() => t)
def defer[G[+_], S <: Tuple, A, B](t: => Freer[G, S, A])(f: A => Freer[G, S, B]): Freer[G, S, B] = Freer.Bind(Freer.Delay(() => t), f)
/** the stack in force where a program is WRITTEN: `reset` gives its body one, `shift0` reads it. A lexical given,
 * because an expected type does not reach a method's receiver, and `S` must be exact */
final class In[S <: Tuple]

/** the top: no delimiter in force. In scope everywhere in the package; a user imports it */
given top: In[EmptyTuple] = In()

/** `reset_p body`: on the stack in force, the body sees it with `p` on top */
def reset[H[+_], S <: Tuple, Y, L <: String & Singleton](p: Prompt[L, H, Y])(using In[S])
         (body: In[At[L, Freer[H, EmptyTuple, Y]] *: S] ?=> Freer[H, At[L, Freer[H, EmptyTuple, Y]] *: S, Y]): Freer[H, S, Y] =
  Freer.Reset(p, body(using In()))

/** `shift0[X](p)(k => …)`: `S` from the stack in force, the stack under `p` computed from it, the witness searched
 * last, when both are known. No delimiter of `p` in force: no witness, a compile error */
def shift0[X]: Shift0At[X] = Shift0At[X]()
final class Shift0At[X]:
  def apply[H[+_], S <: Tuple, Y, L <: String & Singleton](p: Prompt[L, H, Y])(using In[S])
           (f: (X => Freer[H, Under[S, At[L, Freer[H, EmptyTuple, Y]]], Y]) => Freer[H, Under[S, At[L, Freer[H, EmptyTuple, Y]]], Y])
           (using has: Has[S, At[L, Freer[H, EmptyTuple, Y]], Under[S, At[L, Freer[H, EmptyTuple, Y]]]]): Freer[H, S, X] =
    Freer.Shift0[H, S, X, Y, L, Under[S, At[L, Freer[H, EmptyTuple, Y]]]](p, has, f)
/** `ret $_p body`, derived as before */
def dollar[H[+_], S <: Tuple, X, Y, L <: String & Singleton](p: Prompt[L, H, Y])(using In[S])(ret: X => Freer[H, S, Y])
          (body: In[At[L, Freer[H, EmptyTuple, Y]] *: S] ?=> Freer[H, At[L, Freer[H, EmptyTuple, Y]] *: S, X]): Freer[H, S, Y] =
  Freer.Reset(p, Freer.Bind(body(using In()), (x: X) => Freer.Shift0[H, At[L, Freer[H, EmptyTuple, Y]] *: S, Y, Y, L, S](p, Has.Head(), _ => ret(x))))
