package okay

import okay.core.{Eff, Paths}
import okay.one.handler.{Aborting, Answering, Deep, Linear, Stateful, Translating, Handler as OneHandler}
import okay.one.row.{In, Key, Removed}

/**
 * LEXICAL HANDLERS (specs/lexical-instances.md; Biernacki et al., "Binders by day, labels by night", POPL 2020): an
 * installation of a handler hands its body an INSTANCE, and an operation performed through the instance goes to THAT
 * installation, whatever handlers are installed between — two of one effect are two names, and an inner handler of
 * the same effect never catches the outer's operation (no accidental handling).
 *
 * On the kernel this is a PLACE BY KEY: each installation makes a fresh `Key`, its handler says the key is its own
 * (`Handler.at`), and the instance performs at the key's place (`In.At`), found at run time — so the effect never
 * enters the program's row: an instance's operation is `X ! Pure`, fitting any program. Any handler form installs
 * so (`Handler.answering`, `.state`, `.deep`, `.aborting`, or one written out): the strategy is the handler's —
 * a `Stateful` answered in place, a `Deep` with the rest as `k`, multi-shot. An instance used where its
 * installation is not in force (kept past it) is the kernel's error at run time, raised where it is performed: an
 * `IllegalStateException`, never a wrong answer.
 */
object Lexical:
  /** an INSTANCE of `E`: the name of one installation */
  final class Inst[E[_]] private[Lexical] (key: Key[E]):
    /** `op`, to this instance's installation */
    def perform[X](op: E[X]): X ! Pure = new Run[Pure, X]:
      def on[R <: okay.core.Row](ps: Paths[Pure, R]): Eff[R, X] = okay.one.Cont.perform[E, R, X](op)(using key.place[R])
    override def toString: String = s"Lexical.Inst($key)"

  /** `h` INSTALLED LEXICALLY around `body`: the body given its instance, a fresh one each run; the answer `h`'s */
  def handle[E[_], O[_], A, Es[_]](h: Handler[E, O])(body: Inst[E] => A ! Es): O[A] ! Es = new Run[Es, O[A]]:
    def on[R <: okay.core.Row](ps: Paths[Es, R]): Eff[R, O[A]] =
      val key = Key.fresh[E]()
      // the handler's effect in no place of the row (`Absent`): it is found by its key alone
      okay.one.Cont.handle(body(Inst(key)).fit(ps))(keyed(h.kernel[R, A], key))(using Removed.Absent[E, R]())

  /** `h` as it is, saying `key` is its own: each form delegated, the place by key the only difference */
  private def keyed[E[_], T <: okay.core.Row, A, B](h: OneHandler[E, T, A, B], key: Key[E]): OneHandler[E, T, A, B] =
    h match
      case a: Answering[E, T, A, B] => new Answering[E, T, A, B]:
        override def at[F[_], R <: okay.core.Row](in: In.At[F, R]): In.Same[E, F] = key.at(in)
        def ret(x: A) = a.ret(x)
        def value[X](op: E[X]): X = a.value(op)
      case t: Translating[E, T, A, B] => new Translating[E, T, A, B]:
        override def at[F[_], R <: okay.core.Row](in: In.At[F, R]): In.Same[E, F] = key.at(in)
        def ret(x: A) = t.ret(x)
        def apply[X](op: E[X]) = t(op)
      case l: Linear[E, T, A, B] => new Linear[E, T, A, B]:
        override def at[F[_], R <: okay.core.Row](in: In.At[F, R]): In.Same[E, F] = key.at(in)
        def ret(x: A) = l.ret(x)
        def apply[I, O, X](op: E[X], k: X => okay.one.Cont[T, I, O, B]) = l(op, k)
      case ab: Aborting[E, T, A, B] => new Aborting[E, T, A, B]:
        override def at[F[_], R <: okay.core.Row](in: In.At[F, R]): In.Same[E, F] = key.at(in)
        def ret(x: A) = ab.ret(x)
        def abort[X](op: E[X]) = ab.abort(op)
      case d: Deep[E, T, A, B] => new Deep[E, T, A, B]:
        override def at[F[_], R <: okay.core.Row](in: In.At[F, R]): In.Same[E, F] = key.at(in)
        def ret(x: A) = d.ret(x)
        def apply[X](op: E[X], k: X => okay.one.![B, T]) = d(op, k)
      case sf: Stateful[E, T, s, A, B] => new Stateful[E, T, s, A, B]:
        override def at[F[_], R <: okay.core.Row](in: In.At[F, R]): In.Same[E, F] = key.at(in)
        def initial: s = sf.initial
        def ret(st: s, x: A) = sf.ret(st, x)
        def value[X](st: s, op: E[X]): X = sf.value(st, op)
        def next[X](st: s, op: E[X]): s = sf.next(st, op)
