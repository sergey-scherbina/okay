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

/** THE EFFECTS OF `F` ERASED INTO ONE, `T`, that takes any operation (a foreign facade's `Top`: okay-java,
 * okay-scala2): each of `F`'s operations performed again at `T`'s key — the keys of `F` found at compile time from
 * the shape of `F` as written in its type (an abstract `F` alone: every operation by key that comes) */
final class Erased[F[_]](val keys: KeysOf[F])

object Erased:
  inline given derived[F[_]]: Erased[F] = Erased(KeysOf.derived[F])
  /** `p`'s effects erased into `T` */
  def erase[A, F[_], T[_]](p: A ! F)(using e: Erased[F], t: KeyOf[T]): A ! T =
    Run.of(Program.relayed(Run.kernel[A, F, okay.one.row.Pure](p), e.keys, _ => t.in.asInstanceOf[okay.core.In[?, ?]]))
