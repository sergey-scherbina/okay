package okay.cont

import okay.{Answers, Control, Effects, Pure, !>, />}

/**
 * THE MACHINE AS AN `Effects` ENCODING (specs/freer-min.md, stage 30): `Prog[F, A]` is a program of `A` over the
 * signature `F` — a row of the core's kind, a union — as a FUNCTION of how `F` is performed at a context, into
 * `Cont`. `Effects[Prog]` is in the companion, so `Effects[Prog]`, `prog[Prog]`, `summon[Effects[Prog]]` find it
 * with no import, beside `Effects[Free]` and `Effects[Eager]`: the encoding is chosen where the program is run,
 * by the given, at compile time; the extension syntax (`p.runWith`, `p.flatMap`) rides `import okay.cont.Prog.given`,
 * as `Eager`'s does. What the machine does natively here: `runWith` answers every operation in
 * place; `foldCont` runs the program under one delimiter at the top, each operation's continuation a run of its
 * own inside the continuation `h` is given, so the fold is constant-stack; `tailcall` is `Delay`. The rest of
 * the interface — `handle`, `shift`, `reset`, `foldMap` — is the interface's own, by `foldCont` and `reify`.
 */
trait Prog[F[+_], +A]:
  def run(using d: Dispatch[F]): Cont[d.c.Here, d.c.Here, A]

/** how the operations of `F` are performed at a context `c`: what a `Prog` is a function of — the one capability
 * for the whole row, where `Perform` is one per effect */
trait Dispatch[F[+_]]:
  val c: Ctx
  def apply[X](op: F[X]): Cont[c.Here, c.Here, X]

object Prog:
  given Effects[Prog] with
    /** the core's continuations, for now: the fold below runs the machine under one delimiter and answers
     * into them; a carrier of the machine's own is the next stage */
    type C = okay.Cont
    def control: Control[okay.Cont] = summon[Control[okay.Cont]]

    override def pure[F[+_], A](a: A): Prog[F, A] = new Prog[F, A]:
      def run(using d: Dispatch[F]): Cont[d.c.Here, d.c.Here, A] = Cont.Return(a)
    override def perform[F[+_], A](e: F[A]): Prog[F, A] = new Prog[F, A]:
      def run(using d: Dispatch[F]): Cont[d.c.Here, d.c.Here, A] = d(e)
    override def defer[F[+_], A, B](thunk: () => Prog[F, A])(f: A => Prog[F, B]): Prog[F, B] = new Prog[F, B]:
      def run(using d: Dispatch[F]): Cont[d.c.Here, d.c.Here, B] =
        Cont.Bind(Cont.Delay(() => thunk().run), (a: A) => f(a).run)
    /** the node for exactly this */
    override def tailcall[F[+_], A](thunk: => Prog[F, A]): Prog[F, A] = new Prog[F, A]:
      def run(using d: Dispatch[F]): Cont[d.c.Here, d.c.Here, A] = Cont.Delay(() => thunk.run)

    extension [F[+_], A](m: Prog[F, A])
      override def flatMap[B](f: A => Prog[F, B]): Prog[F, B] = new Prog[F, B]:
        def run(using d: Dispatch[F]): Cont[d.c.Here, d.c.Here, B] = m.run.flatMap(a => f(a).run)

      /** the program under one delimiter at the top, every operation a capture to it: its clause answers with
       * `h`'s continuation, whose own continuation runs the captured rest — a run of its own, at the top, from
       * which the next operation's clause returns at once. The fold is a value that runs when its `k` is given */
      override def foldCont[S](h: F !> S): A /> S =
        val clause = new Clause[F, EmptyTuple, A /> S]:
          def apply[X](op: F[X], k: X => Top[A /> S]): Top[A /> S] =
            Cont.Return(h(op).flatMap(x => Machine.value(k(x))))
        Machine.value(okay.cont.reset[A /> S](using Root): in ?=>
          m.run(using Dispatch.reaching(in, clause)).map(okay.Cont.Pure(_)))

      /** every operation answered in place, at the top */
      override def runWith(using H: Answers[F]): A = Machine.value(m.run(using Dispatch.answering(H)))

    override def run[A](m: Prog[Pure, A]): A = Machine.value(m.run(using Dispatch.none))

object Dispatch:
  /** at the top, every operation answered by `H` where the machine gets to it */
  def answering[F[+_]](H: Answers[F]): Dispatch[F] { val c: Root.type } = new Dispatch[F]:
    val c: Root.type = Root
    def apply[X](op: F[X]): Top[X] = Cont.Delay(() => Cont.Return(H.handle(op)))

  /** under the delimiter whose body `in` is, every operation a capture to it, answered by the clause */
  def reaching[F[+_], Ans](in: In[Ans, Root.type], clause: Clause[F, EmptyTuple, Ans]): Dispatch[F] { val c: in.type } = new Dispatch[F]:
    val c: in.type = in
    def apply[X](op: F[X]): Cont[in.Here, in.Here, X] = Cont.Op[in.Here, X, EmptyTuple, Ans, F](Reach.Here(), op, clause)

  /** nothing to perform */
  def none: Dispatch[Pure] { val c: Root.type } = new Dispatch[Pure]:
    val c: Root.type = Root
    def apply[X](op: Nothing): Top[X] = op
