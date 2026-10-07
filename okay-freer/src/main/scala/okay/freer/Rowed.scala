package okay.freer

import okay.{Effects, Row, Member, Removed, Members, Tagged, Pure}

import okay.cont.{Clause, Cont, Ctx, Handler, Has, Cap, In, Machine, Reach, Reaches, Root, Target, Top}

/**
 * THE CLASSIC TREE UNDER THE FACADE (specs/freer-min.md, stage 47): the tree over a ROW — its operations tagged
 * with their path in it (`Tagged`), so that a handler tells its operations from the rest's by the path and no
 * class test — as an instance of the core's `Effects`, the interface every encoding has. `pure`, `perform`,
 * `flatMap`, `defer` are the tree's own. A handler is the machine's (`Handler`, `Answering`), and the tree
 * runs it ON THE MACHINE: the tree folded into the machine's program over the row (each operation performed by
 * its path), handled there, and the handled program REIFIED back into a tree over the rest of the row — run at
 * the top with every remaining effect's capability a clause that puts the operation into the tree and resumes
 * the machine when the tree is (`Reifying`). `run` folds into the machine and runs it. The classic's own
 * spelling, `A ! F` over a union with its own handlers, is the classic's (`Classic`); this is the same tree
 * where the facade is spoken.
 */
type Rowed[R <: Row, +A] = Free[[X] =>> Tagged[R, X], A]

given given_Effects_Rowed: Effects[Rowed] with
  def pure[R <: Row, A](a: A): Rowed[R, A] = Free.Return(a)
  def perform[E[+_], R <: Row, X](op: E[X])(using m: Member[E, R]): Rowed[R, X] = Free.Inject(Tagged.At(op, m))
  def defer[R <: Row, A, B](thunk: () => Rowed[R, A])(f: A => Rowed[R, B]): Rowed[R, B] = Free.defer(thunk)(f)
  override def tailcall[R <: Row, A](thunk: => Rowed[R, A]): Rowed[R, A] = Free.delay(() => thunk)
  extension [R <: Row, A](m: Rowed[R, A])
    def flatMap[B](f: A => Rowed[R, B]): Rowed[R, B] = m.flatMap(f)
  def handle[E[+_], A, Ans, R <: Row](h: Handler[E, A, Ans])(m: Rowed[R, A])(using rm: Removed[E, R]): Rowed[rm.Out, Ans] =
    val handled = h.inPlace match
      case Some(a) => okay.cont.Free.handle(a)(Rowed.machine(m))(using rm)
      case None => okay.cont.Free.handle(h)(Rowed.machine(m))(using rm)
    Rowed.reify[rm.Out, Ans](handled, rm.rest)
  def run[A](m: Rowed[Pure, A]): A = Machine.value(okay.cont.Free.top(Rowed.machine(m)))

object Rowed:
  /** the tree as the machine's program over its row: each operation performed by its path, at the context */
  def machine[R <: Row, A](m: Rowed[R, A]): okay.cont.Free[R, A] = new okay.cont.Free[R, A]:
    def run(using c: Ctx, has: Has[R, c.type]): Cont[c.Here, c.Here, A] =
      def go(m: Rowed[R, A]): Cont[c.Here, c.Here, A] =
        Free.fold(m)(a => Cont.Return(a))([X] => (t: Tagged[R, X]) => (k: X => Rowed[R, A]) =>
          t.perform(c)(has).flatMap(x => Cont.Delay(() => go(k(x)))))
      go(m)

  /** the machine's program over a row, as a tree over it: run at the top, under one delimiter whose answer is the
   * tree, every effect of the row a clause that puts its operation into the tree and resumes the machine when
   * the tree is bound further — one hop of the machine per operation, so the depth is constant */
  def reify[R <: Row, A](p: okay.cont.Free[R, A], ms: Members[R]): Rowed[R, A] =
    val in: In[Rowed[R, A], Root.type] = In.at[Rowed[R, A]](using Root)
    Machine.value(Cont.Reset[EmptyTuple, EmptyTuple, Rowed[R, A], Rowed[R, A]](
      p.run(using in, Reifying.has[R, R, A](ms, in)([E[+_]] => (m: Member[E, R]) => m)).map(Free.Return(_))))

  /** the capabilities of a row at the reifying context: by the row's members, each a clause into the tree; `path`
   * takes a member of the row walked to its place in the whole row */
  private object Reifying:
    def has[T <: Row, R <: Row, A](ms: Members[T], in: In[Rowed[R, A], Root.type])(path: [E[+_]] => Member[E, T] => Member[E, R]): Has[T, in.type] =
      ms match
        case Members.MNil() => Has.HNil()
        case mc: Members.MCons[e, t] =>
          Has.HCons(
            Cap.Reaching(reaches[e, R, A](in, path[e](Member.Head[e, t]()))),
            has[t, R, A](mc.tail, in)([E[+_]] => (m: Member[E, t]) => path[E](Member.Tail[E, e, t](m))))
    def reaches[E[+_], R <: Row, A](in: In[Rowed[R, A], Root.type], m: Member[E, R]): Reaches[E, in.type] = new Reaches[E, in.type]:
      def target(c: in.type): Target[E, c.Here] =
        Target[E, c.Here, EmptyTuple, Rowed[R, A]](Reach.Here[EmptyTuple, Rowed[R, A]](), new Clause[E, EmptyTuple, Rowed[R, A]]:
          def apply[X](op: E[X], k: X => Top[Rowed[R, A]]): Top[Rowed[R, A]] =
            Cont.Return(Free.Inject(Tagged.At(op, m)).flatMap(x => Free.delay(() => Machine.value(k(x))))))
