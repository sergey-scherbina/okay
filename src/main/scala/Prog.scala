package okay

import okay.cont.{Carrier, Ctx, In, Machine, Root, Top}
import okay.cont.Cont.{Bind, Delay, Return}

/**
 * THE MACHINE AS AN `Effects` ENCODING (specs/freer-min.md, stage 30): `Prog[F, A]` is a program of `A` over the
 * signature `F` — a row of the core's kind, a union — as a FUNCTION of how `F` is performed at a context, into
 * `Cont`. `Effects[Prog]` is in the companion, so `Effects[Prog]`, `prog[Prog]`, `summon[Effects[Prog]]` find it
 * with no import, beside `Effects[Free]` and `Effects[Eager]`: the encoding is chosen where the program is run,
 * by the given, at compile time; the extension syntax (`p.runWith`, `p.flatMap`) rides `import okay.cont.Prog.given`,
 * as `Eager`'s does. ITS CARRIER IS THE MACHINE'S OWN (stage 32): `Carrier[A, S, R]`, a program one level over
 * the top, is `(A => S) => R` as the machine has it, and its `Control` (`Control[Carrier]`, Control.scala) is `Shift0`, `Reset`
 * and `Bind`. So `foldCont` is nothing but the program run with the handler as its dispatch — an operation's
 * answer `h(op)` IS a program at that level — and `/` is the delimiter: `handle`, `convert`, `reify` go through
 * `control` and run on the machine, no continuation of the core's between. `runWith` answers every operation
 * in place; `tailcall` is `Delay`.
 */

/** a program of `A` over the signature `F`, run at any context given how `F` is performed there */
trait Prog[F[+_], +A]:
  def run(using d: Dispatch[F]): cont.Cont[d.c.Here, d.c.Here, A]

/** how the operations of `F` are performed at a context `c`: what a `Prog` is a function of — the one capability
 * for the whole row, where `Perform` is one per effect */
trait Dispatch[F[+_]]:
  val c: Ctx
  def apply[X](op: F[X]): cont.Cont[c.Here, c.Here, X]

object Prog:
  given given_Effects_Prog: Effects.Aux[Prog, Carrier] = ProgEffects

  object ProgEffects extends Effects[Prog]:
    type C = Carrier
    def control: Control[Carrier] = summon[Control[Carrier]]

    override def pure[F[+_], A](a: A): Prog[F, A] = new Prog[F, A]:
      def run(using d: Dispatch[F]): cont.Cont[d.c.Here, d.c.Here, A] = Return(a)
    override def perform[F[+_], A](e: F[A]): Prog[F, A] = new Prog[F, A]:
      def run(using d: Dispatch[F]): cont.Cont[d.c.Here, d.c.Here, A] = d(e)
    override def defer[F[+_], A, B](thunk: () => Prog[F, A])(f: A => Prog[F, B]): Prog[F, B] = new Prog[F, B]:
      def run(using d: Dispatch[F]): cont.Cont[d.c.Here, d.c.Here, B] =
        Bind(Delay(() => thunk().run), (a: A) => f(a).run)
    /** the node for exactly this */
    override def tailcall[F[+_], A](thunk: => Prog[F, A]): Prog[F, A] = new Prog[F, A]:
      def run(using d: Dispatch[F]): cont.Cont[d.c.Here, d.c.Here, A] = Delay(() => thunk.run)

    extension [F[+_], A](m: Prog[F, A])
      override def flatMap[B](f: A => Prog[F, B]): Prog[F, B] = new Prog[F, B]:
        def run(using d: Dispatch[F]): cont.Cont[d.c.Here, d.c.Here, B] = m.run.flatMap(a => f(a).run)

      /** the program at the top level, the handler its dispatch: an operation's answer is `h(op)`, a program
       * at that level, so nothing is built around it; `/` puts the delimiter on */
      override def foldCont[S](h: Interpr[F, Carrier, S]): Carrier[A, S, S] =
        m.run(using Dispatch.by[F, S](h))

      /** every operation answered in place, at the top */
      override def runWith(using H: Answers[F]): A = Machine.value(m.run(using Dispatch.answering(H)))

    override def run[A](m: Prog[Pure, A]): A = Machine.value(m.run(using Dispatch.none))

object Dispatch:
  /** at the top, every operation answered by `H` where the machine gets to it */
  def answering[F[+_]](H: Answers[F]): Dispatch[F] { val c: Root.type } = new Dispatch[F]:
    val c: Root.type = Root
    def apply[X](op: F[X]): Top[X] = Delay(() => Return(H.handle(op)))

  /** at the top level, written at the answer `S`: every operation answered by `h`, a program at that level */
  def by[F[+_], S](h: Interpr[F, Carrier, S]): Dispatch[F] { val c: In[S, Root.type] } = new Dispatch[F]:
    val c: In[S, Root.type] = In.at[S](using Root)
    def apply[X](op: F[X]): cont.Cont[c.Here, c.Here, X] = h(op)

  /** nothing to perform */
  def none: Dispatch[Pure] { val c: Root.type } = new Dispatch[Pure]:
    val c: Root.type = Root
    def apply[X](op: Nothing): Top[X] = op
