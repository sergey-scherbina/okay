package okay

/**
 * Probe: can the row be attached AFTER the fact, leaving every
 * constructor exactly as it ships today?
 *
 * Currying the type parameters the obvious way does not work here.
 * `Direct.direct[F]` gets away with it (an applier value class, then
 * `apply[A]`) because nobody calls `direct[F]` and expects a value.
 * A constructor is different: if `State.get[S]` returns an applier,
 * then `State.get[Int]` stops being a program and every call site
 * that reads it as one breaks — the migration the row parameter was
 * trying to avoid, just moved.
 *
 * So this probe curries the other way round: the constructors keep
 * their narrow types, and a POSTFIX `.at[R]` moves a finished program
 * into a wider row. Purely additive — no signature changes, no
 * migration, and the complement never has to be named.
 */
object ProbeCurried:

  // ---- the evidence, as in ProbeVariance
  trait In[F[+_], R[+_]]:
    def inj[A](fa: F[A]): R[A]

  trait InLow:
    given self[F[+_]]: In[F, F] = new In[F, F]:
      def inj[A](fa: F[A]): F[A] = fa

  object In extends InLow:
    given left[F[+_], G[+_]]: In[F, F + G] = new In[F, F + G]:
      def inj[A](fa: F[A]): (F + G)[A] = fa
    given deeper[F[+_], G[+_], H[+_]](using i: In[F, G]): In[F, G + H] =
      new In[F, G + H]:
        def inj[A](fa: F[A]): (G + H)[A] = i.inj(fa)

  /**
   * The postfix move. `!.widen` APPENDS a row on the right; this
   * lands the program in whatever row the evidence can reach, which
   * is what lets the caller name only the target and never the
   * complement. Same tree walk widen does — Free is invariant in F,
   * so this rebuilds rather than casts.
   */
  extension [A, F[+_]](p: A ! F)
    def at[R[+_]](using i: In[F, R]): A ! R =
      import okay.!.*
      (p.resume: @unchecked) match
        case Pure(a) => Free.Pure(a)
        case Effect(e) => Free.inject(i.inj(e))
        case Bind(Effect(e), k) =>
          Free.inject(i.inj(e)).flatMap(x => k(x).at[R])

  /** the operator's ergonomics point: partially applied as a type
   * lambda, In becomes a CONTEXT BOUND right after the parameter —
   * `[R[+_] : Has[State % Int]]` instead of a using clause */
  type Has[F[+_]] = [R[+_]] =>> In[F, R]

  type Mix = State % Int + Writer % String

  // 1. today's constructors, untouched, in their own narrow row
  val narrow: Int ! (State % Int) = State.get[Int]

  // 2. the same constructors moved into a mixed row — the target is
  //    named, the complement never is
  val mixed: Int ! Mix =
    for
      n <- State.get[Int].at[Mix]
      _ <- Writer.tell("saw").at[Mix]
    yield n

  // 3. can the target be left to inference?
  val inferred: Int ! Mix =
    for
      n <- State.get[Int].at
      _ <- Writer.tell("saw").at
    yield n

  // 4. a helper needing two effects, called from a row carrying a
  //    third — the case that separated the designs before
  type Mix3 = State % Int + Writer % String + Reader % Boolean

  def bump[R[+_]](by: Int)(using In[State % Int, R], In[Writer % String, R])
  : Int ! R =
    for
      n <- State.get[Int].at[R]
      _ <- Writer.tell(s"bump $n").at[R]
    yield n + by

  /** the same, as context bounds */
  def bumpBound[R[+_] : Has[State % Int] : Has[Writer % String]](by: Int)
  : Int ! R =
    for
      n <- State.get[Int].at[R]
      _ <- Writer.tell(s"bump $n").at[R]
    yield n + by

  val used: Int ! Mix3 = bump[Mix3](1)
  val usedBound: Int ! Mix3 = bumpBound[Mix3](1)

  def main(args: Array[String]): Unit =
    println("NARROW " + State.run[Int, Int](7)(narrow))
    def runMix(p: Int ! Mix) =
      State.run[Int, (Seq[String], Int)](7)(
        Writer.run[String, Int, State % Int](p))
    println("MIXED  " + runMix(mixed))
    println("INFER  " + runMix(inferred))
    println("BOUND  " + !.run(Reader.run[Boolean, (Int, (Seq[String], Int)), Pure](true)(
      State.handle[Int](7)(
        Writer.run[String, Int, State % Int + Reader % Boolean](usedBound)))))
    println("HELPER " + !.run(Reader.run[Boolean, (Int, (Seq[String], Int)), Pure](true)(
      State.handle[Int](7)(
        Writer.run[String, Int, State % Int + Reader % Boolean](used)))))
