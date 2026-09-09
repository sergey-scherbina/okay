package okay

/**
 * PROBE (rowlift): the one-cast design, exercised.
 *
 * `RowLift` (src/main) carries the witness and the single cast; this
 * checks the four shapes that have to work: a narrow program left
 * alone, an explicit target, an inferred target, and a polymorphic
 * helper written against a context bound.
 */
object ProbeOneCast:
  import RowLift.{Has, at}

  type Mix  = State % Int + Writer % String
  type Mix3 = State % Int + Writer % String + Reader % Boolean

  val narrow: Int ! (State % Int) = State.get[Int]

  /** the target is named; the complement never is */
  val mixed: Int ! Mix =
    for
      n <- State.get[Int].at[Mix]
      _ <- Writer.tell("saw").at[Mix]
    yield n

  /** and it can be left to inference */
  val inferred: Int ! Mix =
    for
      n <- State.get[Int].at
      _ <- Writer.tell("saw").at
    yield n

  /** a helper needing two effects, callable from any row carrying them */
  def bump[R[+_] : Has[State % Int] : Has[Writer % String]](by: Int): Int ! R =
    for
      n <- State.get[Int].at[R]
      _ <- Writer.tell(s"bump $n").at[R]
    yield n + by

  val used: Int ! Mix3 = bump[Mix3](1)

  def main(args: Array[String]): Unit =
    println("NARROW " + State.run[Int, Int](7)(narrow))
    def runMix(p: Int ! Mix) =
      State.run[Int, (Seq[String], Int)](7)(Writer.run[String, Int, State % Int](p))
    println("MIXED  " + runMix(mixed))
    println("INFER  " + runMix(inferred))
    println("BOUND  " + !.run(Reader.run[Boolean, (Int, (Seq[String], Int)), Pure](true)(
      State.handle[Int, (Seq[String], Int), Reader % Boolean](7)(
        Writer.run[String, Int, State % Int + Reader % Boolean](used)))))
