package okay2

/** okay2-state-modify-op: the Scala 3 core's ProbeRowCost (specs/effect-row-cost.md, D1) — EXACT bytes per level
 * of a mutual recursion over State, read by ThreadMXBean, warm, load-independent. `modify` is one `Update` since
 * okay2-level1-api; `getThenSet` is the two operations it replaced, written out */
class ProbeRowCost extends munit.FunSuite {
  val N = 100000
  type S = State[Int]

  def mEven(n: Int): Boolean ! S = if (n == 0) pure[S, Boolean](true) else State.modify[Int](_ + 1).flatMap(_ => mOdd(n - 1))
  def mOdd(n: Int): Boolean ! S = if (n == 0) pure[S, Boolean](false) else State.modify[Int](_ + 1).flatMap(_ => mEven(n - 1))

  def gsEven(n: Int): Boolean ! S = if (n == 0) pure[S, Boolean](true) else State.get[Int].flatMap(s => State.set(s + 1)).flatMap(_ => gsOdd(n - 1))
  def gsOdd(n: Int): Boolean ! S = if (n == 0) pure[S, Boolean](false) else State.get[Int].flatMap(s => State.set(s + 1)).flatMap(_ => gsEven(n - 1))

  def sEven(n: Int): Boolean ! S = if (n == 0) pure[S, Boolean](true) else State.set[Int](n).flatMap(_ => sOdd(n - 1))
  def sOdd(n: Int): Boolean ! S = if (n == 0) pure[S, Boolean](false) else State.set[Int](n).flatMap(_ => sEven(n - 1))

  def gEven(n: Int): Boolean ! S = if (n == 0) pure[S, Boolean](true) else State.get[Int].flatMap(_ => gOdd(n - 1))
  def gOdd(n: Int): Boolean ! S = if (n == 0) pure[S, Boolean](false) else State.get[Int].flatMap(_ => gEven(n - 1))

  def tEven(n: Int): Boolean ! Pure = if (n == 0) pure[Pure, Boolean](true) else !.tailcall(tOdd(n - 1))
  def tOdd(n: Int): Boolean ! Pure = if (n == 0) pure[Pure, Boolean](false) else !.tailcall(tEven(n - 1))

  private val mx = java.lang.management.ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
  private def perLevel(name: String)(run: => Any): Double = {
    for (_ <- 1 to 30) { val _ = run }
    val id = Thread.currentThread().threadId()
    val b0 = mx.getThreadAllocatedBytes(id)
    for (_ <- 1 to 10) { val _ = run }
    val b1 = mx.getThreadAllocatedBytes(id)
    val per = (b1 - b0).toDouble / 10 / N
    println(f"PROBE $name%-36s $per%8.1f B/level")
    per
  }

  test("bytes per level: modify is one operation, at set's level") {
    val modify = perLevel("State, modify (one Update)")(State.run[Int, Boolean](0)(mEven(N)))
    val getSet = perLevel("State, get then set (two ops)")(State.run[Int, Boolean](0)(gsEven(N)))
    val set = perLevel("State, set (one Update)")(State.run[Int, Boolean](0)(sEven(N)))
    perLevel("State, get (one op)")(State.run[Int, Boolean](0)(gEven(N)))
    perLevel("Pure tailcall")(!.run(tEven(N)))
    assert(modify < getSet, s"modify $modify B/level, get+set $getSet")
    assert(modify <= set + 32, s"modify $modify B/level, set $set")
  }
}
