package okay

import okay.one.Cont
import okay.one.row.{Key, Pure as OnePure}

/**
 * LEXICAL HANDLERS (specs/lexical-instances.md; Biernacki et al., "Binders by day, labels by night", POPL 2020): an
 * installation of a handler hands its body an INSTANCE, and an operation performed through the instance goes to THAT
 * installation, whatever handlers are installed between — two of one effect are two names, and an inner handler of
 * the same effect never catches the outer's operation (no accidental handling).
 *
 * On the kernel this is a PLACE BY KEY: each installation makes a fresh `Key`, its handler says the key is its own
 * (`Handler.keyed`), and the instance performs at the key's place (`In.At`), found at run time — so the effect never
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
    def perform[X](op: E[X]): X ! Pure = Run.of(Cont.perform[E, OnePure, X](op)(using key.place[OnePure]))
    override def toString: String = s"Lexical.Inst($key)"

  /** `h` INSTALLED LEXICALLY around `body`: the body given its instance, a fresh one each run; the answer `h`'s */
  def handle[E[_], O[_], A, Es[_]](h: Handler[E, O])(body: Inst[E] => A ! Es): O[A] ! Es = Run.of(Cont.defer:
    val key = Key.fresh[E]()
    // the handler's effect in no place of the row (`Absent`): it is found by its key alone
    Cont.handle(Run.kernel[A, Es, OnePure](body(Inst(key))))(h.kernel[OnePure, A].keyed(key.ref))(
      using okay.one.row.Removed.byKey[E]))
