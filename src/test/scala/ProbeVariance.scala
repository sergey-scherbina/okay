package okay

/** Probe: constrain the row with evidence instead of leaving F free. */
object ProbeVariance:

  trait In[F[+_], R[+_]]:
    def inj[A](fa: F[A]): R[A]

  trait InLow:
    /** LOW priority: matching the whole row commits it, so it must
     * lose to the structural case or a mixed row never forms */
    given self[F[+_]]: In[F, F] = new In[F, F]:
      def inj[A](fa: F[A]): F[A] = fa

  object In extends InLow:
    given left[F[+_], G[+_]]: In[F, F + G] = new In[F, F + G]:
      def inj[A](fa: F[A]): (F + G)[A] = fa

  inline def op[F[+_], R[+_], A](fa: F[A])(using i: In[F, R]): A ! R =
    effect[R, A](i.inj(fa))

  inline def tell[W, R[+_]](w: W)(using In[Writer % W, R]): Unit ! R =
    op[Writer % W, R, Unit](Writer(w))
  inline def get[S, R[+_]](using In[State % S, R]): S ! R =
    op[State % S, R, S](State.Get())

  // 1. narrow row
  val narrow: Unit ! (Writer % String) = tell("a")
  // 2. mixed row, no annotation on the operations
  type Mix = State % Int + Writer % String
  val mixed: Int ! Mix =
    for
      n <- get[Int, Mix]
      _ <- tell[String, Mix]("saw")
    yield n
  // 3. can the row be left to inference?
  val inferred: Int ! Mix =
    for
      n <- get
      _ <- tell("saw")
    yield n

  // 4. THE POSITION THAT BROKE with a free F: a foldLeft seed, no
  // expected type. Under evidence the low-priority self applies and
  // the row is the narrow one — the sensible answer, not Any.
  val seed = (1 to 3).foldLeft(tell[Int, Writer % Int](0))((m, i) => m.flatMap(_ => tell(i)))

  def main(args: Array[String]): Unit =
    println("NARROW " + !.run(Writer.run[String, Unit, Pure](narrow))._1)
    println("MIXED  " + State.run[Int, (Seq[String], Int)](7)(
      Writer.run[String, Int, State % Int](mixed)))
    println("INFER  " + State.run[Int, (Seq[String], Int)](7)(
      Writer.run[String, Int, State % Int](inferred)))
    println("SEED   " + !.run(Writer.run[Int, Unit, Pure](seed))._1)
