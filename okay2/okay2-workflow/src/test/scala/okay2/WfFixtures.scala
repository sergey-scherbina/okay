package okay2

/** the fixtures the workflow suites share, at the top level */
object WfFixtures {
  type P = Pure
  type Rw = Delim + P

  /** a drive that was expected to finish */
  def done[Q, R](s: Wf.Step[Q, R]): R = s match {
    case Wf.Step.Done(r) => r
    case other => throw new AssertionError(s"expected the drive to finish, it said $other")
  }

  /** a signature with a state behind it: each `Add` moves a counter,
   * and says how, so a fold needs no match at the answer type */
  sealed trait Tick extends Row { type Op[+A] = Tick.Op[A] }
  object Tick {
    sealed trait Op[+A] { def step: Int => (Int, A) }
    final case class Add(n: Int) extends Op[Int] { def step: Int => (Int, Int) = s => (s + n, s + n) }

    /** the smallest monad that can SEE a doubled step: a counter */
    type Counting[A] = Int => (Int, A)
    implicit val counting: Monad[Counting] = new Monad[Counting] {
      def pure[A](a: A): Counting[A] = s => (s, a)
      def flatMap[A, B](fa: Counting[A])(f: A => Counting[B]): Counting[B] = s => { val (s2, a) = fa(s); f(a)(s2) }
    }
    val run: Static.To[Tick, Counting] = new Static.To[Tick, Counting] { def apply[X](op: Op[X]): Counting[X] = op.step }
  }
}
