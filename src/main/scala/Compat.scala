package okay

/**
 * OKAY-FREER'S WORDS ON THE KERNEL'S PROGRAM (specs/freer-on-core-howto.md): the names a module on the classic
 * road wrote — `!.run`, `!.tailcall`, `!.widen`, `p.plus[G]`, `Row.at`, `Row.union`, `Free.delay` — as wrappers
 * over the root's words, so a module moves by its imports (okay-freer's `X` becomes `okay.X`) and keeps its
 * signatures. Nothing here does work of its own: each word is one of the root's.
 */
object `!`:
  /** a program needing nothing: its value (`p.run`) */
  def run[A](p: A ! Pure): A = p.run
  /** a tail call: built when the run gets to it (`defer`) */
  def tailcall[A, Es[_]](p: => A ! Es): A ! Es = defer(p)
  /** the same program needing one effect more (`p.at[F + G]`) */
  def widen[A, F[_], G[_]](p: A ! F): A ! F + G = p.at[F + G]
  /** `tailRecM` for programs: from `s`, a `Left` goes on, a `Right` answers; each turn deferred */
  def loop[S, A, Es[_]](s: S)(f: S => (Either[S, A] ! Es)): A ! Es = Programs.tailRecM[Es].tailRecM(s)(f)
  /** a fold that performs, built right-nested — each step's continuation builds the next, nothing rotated; indexed
   * and flat; `f` runs when the program does. Stack-safe: the recursion is in the binds' continuations */
  def foldM[X, B, Es[_]](xs: Iterable[X])(z: B)(f: (B, X) => B ! Es): B ! Es =
    val v = scala.collection.immutable.ArraySeq.untagged.from(xs)
    val n = v.length
    def go(i: Int, acc: B): B ! Es =
      if i >= n then okay.pure[B, Es](acc) else Run.flatMap(f(acc, v(i)))(y => go(i + 1, y))(using Join.same[Es])
    defer(go(0, z))
  /** a fold whose step is ONE bind: each element's program, its answer combined into the accumulator purely */
  def foldEach[X, A, B, Es[_]](xs: Iterable[X])(z: B)(f: X => A ! Es)(combine: (B, A) => B): B ! Es =
    val v = scala.collection.immutable.ArraySeq.untagged.from(xs)
    val n = v.length
    def go(i: Int, acc: B): B ! Es =
      if i >= n then okay.pure[B, Es](acc) else Run.flatMap(f(v(i)))(a => go(i + 1, combine(acc, a)))(using Join.same[Es])
    defer(go(0, z))
  /** `f` on each element, in order, for its effects */
  def each[X, Es[_]](xs: Iterable[X])(f: X => Unit ! Es): Unit ! Es = foldM[X, Unit, Es](xs)(())((_, x) => f(x))
  /** `F` translated into the rest `G` (`p.translateInto`) */
  def translate[A, F[_], G[_]](p: A ! F + G)(h: [X] => F[X] => X ! G)(using KeyOf[F]): A ! G = p.translateInto[F, G](h)
  /** `F` translated into `G` beside the rest `H` */
  def interpret[A, F[_], G[_], H[_]](p: A ! F + H)(h: [X] => F[X] => X ! G + H)(using KeyOf[F]): A ! G + H =
    p.translate[F, G + H](h).at[G + H]

/** okay-freer's `Row` words: widening by membership, a sum's answers from its parts */
object Row:
  extension [A, Es[_]](p: A ! Es)
    /** the same program needing `G` more: `A ! Es + G` */
    def plus[G[_]]: A ! Es + G = p.at[Es + G]
    /** the same program needing more effects, `Es` among `Es2` */
    def at[Es2[_]](using w: Within[Es, Es2]): A ! Es2 = Run.at(p)[Es2]
    /** the same as `at` */
    def up[Es2[_]](using w: Within[Es, Es2]): A ! Es2 = Run.at(p)[Es2]
  /** a sum's answers, each part's own (`Answers.sum`) */
  def union[F[_], G[_]](using f: Answers[F], g: Answers[G]): Answers[F + G] = Answers.sum[F, G]

/** okay-freer's `Free` words for a program */
object Free:
  /** a value, needing `Es` */
  def pure[Es[_], A](a: A): A ! Es = okay.pure[A, Es](a)
  /** built when the run gets to it */
  def delay[Es[_], A](thunk: () => A ! Es): A ! Es = defer(thunk())
  /** `!.loop` */
  def loop[S, A, Es[_]](s: S)(f: S => (Either[S, A] ! Es)): A ! Es = !.loop(s)(f)

/** okay-freer's evidence that a union's effects are distinct: on the kernel two of one effect are two places, so
 * there is nothing to forbid — always given, kept so a signature that asked for it compiles unchanged */
sealed trait Distinct[R[_]]
object Distinct:
  private object Any extends Distinct[[x] =>> scala.Any]
  given any[R[_]]: Distinct[R] = Any.asInstanceOf[Distinct[R]]

extension [E[_], X](op: E[X])
  /** an operation as a program, `op.perform` or `perform(op)`: `effect(op)` */
  def perform(using KeyOf[E]): X ! E = effect(op)

/** THE PATHS OF `F` ERASED TO ONE PLACE: every effect of `F` at the place of one effect `T` that takes any operation
 * (a foreign facade's `Top`: okay-java, okay-scala2) — a sum's by its sides, found at compile time from the shape of
 * `F` as written in its type. An abstract `F` is taken as one effect */
trait Erased[F[_]]:
  def paths[T[_], R <: okay.core.Row](in: okay.core.In[T, R]): okay.core.Paths[F, R]

object Erased extends ErasedOne:
  given sum[F[_], G[_]](using f: Erased[F], g: Erased[G]): Erased[F + G] = new Erased[F + G]:
    def paths[T[_], R <: okay.core.Row](in: okay.core.In[T, R]): okay.core.Paths[F + G, R] =
      okay.core.Paths.Both(f.paths(in), g.paths(in))
  given pure: Erased[Pure] = new Erased[Pure]:
    def paths[T[_], R <: okay.core.Row](in: okay.core.In[T, R]): okay.core.Paths[Pure, R] = okay.core.Paths.none

trait ErasedOne:
  given one[F[_]]: Erased[F] = new Erased[F]:
    /** THE CAST, and why it is sound: `T` takes any operation, so an operation of `F` performed at its place is one
     * of `T`'s — the facade answers it after a test of its class */
    def paths[T[_], R <: okay.core.Row](in: okay.core.In[T, R]): okay.core.Paths[F, R] =
      okay.core.Paths.One(in.asInstanceOf[okay.core.In[F, R]])
