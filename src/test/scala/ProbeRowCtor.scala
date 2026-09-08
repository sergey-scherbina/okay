package okay

/** Probe: can ONE row-polymorphic constructor replace the narrow one? */
object ProbeRowCtor:
  type Store = Map[Long, String]

  // the proposed shape, next to the existing State.get[S]
  inline def getIn[S, F[+_]]: S ! (State % S + F) = effect(State.Get())
  inline def setIn[S, F[+_]](s: S): S ! (State % S + F) = effect(State.Set(s))
  inline def tellIn[W, F[+_]](w: W): Unit ! (Writer % W + F) = effect(Writer(w))

  // 1. does the empty row collapse? (State % S + Pure =:= State % S)
  val narrow: Store ! (State % Store) = getIn[Store, Pure]

  // 2. is F inferable from an expected type in a mixed row?
  type R = State % Store + Writer % String
  val mixed: Option[String] ! R =
    for
      m <- getIn[Store, Writer % String]
      _ <- tellIn[String, State % Store]("looked")
    yield m.get(7L)

  // 3a. THE COMPATIBILITY QUESTION: does a BARE call still land in a
  // single-effect row, i.e. does F infer to Pure with no annotation?
  val bareNarrow: Store ! (State % Store) = getIn
  val bareNarrowSet: Store ! (State % Store) = setIn(Map(1L -> "x"))
  val bareNarrowTell: Unit ! (Writer % String) = tellIn("hi")

  // 3b. and does it still work as the LAST step of a for-comprehension
  // in a single-effect row, the everyday shape?
  val narrowFor: Store ! (State % Store) =
    for
      m <- getIn
      _ <- setIn(m + (2L -> "y"))
    yield m

  // 3. the interesting one: can F be left to inference entirely?
  val inferred: Option[String] ! R =
    for
      m <- getIn
      _ <- tellIn("looked")
    yield m.get(7L)

  def main(args: Array[String]): Unit =
    println("NARROW " + State.run[Store, Store](Map(1L -> "a"))(narrow))
    println("MIXED  " + State.run[Store, (Seq[String], Option[String])](Map(7L -> "z"))(
      Writer.run[String, Option[String], State % Store](mixed)))
    println("INFER  " + State.run[Store, (Seq[String], Option[String])](Map(7L -> "z"))(
      Writer.run[String, Option[String], State % Store](inferred)))
