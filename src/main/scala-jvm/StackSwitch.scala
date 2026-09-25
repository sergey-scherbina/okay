package okay

import java.lang.invoke.{MethodHandle, MethodHandles, MethodType}

/**
 * A FRESH STACK for the rest of a direct-style Cont program
 * (specs/stack-safety.md stage 1c), on the JVM.
 *
 * `Cont`'s runner counts, in a field of the continuation it hands a
 * shift's body, how many more nested levels the current stack takes.
 * At zero it calls `fresh`, which runs the rest on a new thread and
 * waits for the answer — no exception, no replay: the waiting frames,
 * the bodies' own included, stay where they are.
 *
 * On JDK 21+ the new thread is VIRTUAL: a waiting virtual thread
 * unmounts, so its frames are frozen into heap StackChunks and only the
 * running segment is on a carrier stack. Measured (scratchpad probe
 * StackHop, 2026-09-25): 1M levels of a body using k's answer on a
 * 128 KB caller stack, 0.2–0.4 µs a level past the switch, 60–110 B of
 * heap a level. Below JDK 21 (this core's floor is 17) it is a
 * PLATFORM thread with a 1 GB stack, whose pages are committed only as
 * they are touched: 3M levels in 6 switches, 129 ms (probe BigStack).
 *
 * The API is JDK 21's, reached through a MethodHandle because the core
 * compiles against JDK 17 (`-java-output-version 17`); the lookup runs
 * once, at class init.
 */
private[okay] object StackSwitch:

  /** levels the CALLER's stack is asked to hold before the first
   * switch: about 1.2 KB a level in a cold JVM, so 256 is ~300 KB of a
   * normal thread's 1 MB. `-Dokay.cont.room=N` changes it. */
  val firstRoom: Int = Integer.getInteger("okay.cont.room", 256)

  private val virtualStart: MethodHandle | Null =
    try
      MethodHandles.publicLookup().findStatic(classOf[Thread], "startVirtualThread",
        MethodType.methodType(classOf[Thread], classOf[Runnable]))
    catch case _: ReflectiveOperationException => null

  /** a waiting virtual thread FREEZES its segment into one heap
   * StackChunk, and HotSpot refuses a chunk that would be humongous
   * ("StackOverflowError: Humongous stack chunk", measured at 512 levels
   * a segment); the probe ran clean at 32 and 64 */
  private val virtualRoom = 64

  /** a platform thread's stack when there are no virtual threads, and
   * the levels it takes at a generous 2 KB each */
  private val bigStack = 1L << 30
  private val bigRoom = (bigStack / 2048).toInt

  def fresh[R](body: Int => R): R =
    var out: Either[Throwable, R] | Null = null
    val start = virtualStart
    val t =
      if start != null then
        val task: Runnable = () => out = run(body, virtualRoom)
        start.invoke(task).asInstanceOf[Thread] // the MethodHandle's declared return: Thread
      else
        val th = new Thread(null, () => out = run(body, bigRoom), "okay-cont-stack", bigStack)
        th.start()
        th
    t.join()
    out match
      case Right(r) => r
      case Left(e) => throw e
      case null => throw IllegalStateException("okay: a Cont stack switch finished without an answer")

  private def run[R](body: Int => R, room: Int): Either[Throwable, R] =
    try Right(body(room)) catch case e: Throwable => Left(e)
