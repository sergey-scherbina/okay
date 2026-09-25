package okay

/**
 * A FRESH STACK for the rest of a direct-style Cont program on Scala
 * Native (specs/stack-safety.md stage 1c): a platform thread with a
 * large stack, as on the JVM below JDK 21.
 */
private[okay] object StackSwitch:
  val firstRoom: Int = 256
  private val bigStack = 1L << 30
  private val bigRoom = (bigStack / 2048).toInt

  def fresh[R](body: Int => R): R =
    var out: Either[Throwable, R] | Null = null
    val t = new Thread(null, () => out = try Right(body(bigRoom)) catch case e: Throwable => Left(e), "okay-cont-stack", bigStack)
    t.start()
    t.join()
    out match
      case Right(r) => r
      case Left(e) => throw e
      case null => throw IllegalStateException("okay: a Cont stack switch finished without an answer")
