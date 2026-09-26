package okay

import okay.Row.at

/** effect-row-recursion-cost: EXACT bytes per level of the mutual
 * recursion's okay variants (ThreadMXBean, warm), load-independent */
class ProbeRowCost extends munit.FunSuite:
  type Fx = State % Int + Writer % String
  val N = 100000

  def stepFx(n: Int): Int ! Fx =
    val counted = State.modify[Int](_ + 1).at[Fx]
    if n % 1000 == 0 then counted.flatMap(k => Writer.tell(s"level $n").at[Fx].map(_ => k)) else counted
  def even(n: Int): Boolean ! Fx = if n == 0 then pure(true) else stepFx(n).flatMap(_ => odd(n - 1))
  def odd(n: Int): Boolean ! Fx = if n == 0 then pure(false) else stepFx(n).flatMap(_ => even(n - 1))

  def sEven(n: Int): Boolean ! State % Int = if n == 0 then pure(true) else State.modify[Int](_ + 1).flatMap(_ => sOdd(n - 1))
  def sOdd(n: Int): Boolean ! State % Int = if n == 0 then pure(false) else State.modify[Int](_ + 1).flatMap(_ => sEven(n - 1))

  def oEven(n: Int): Boolean ! State % Int = if n == 0 then pure(true) else State.set[Int](n).flatMap(_ => oOdd(n - 1))
  def oOdd(n: Int): Boolean ! State % Int = if n == 0 then pure(false) else State.set[Int](n).flatMap(_ => oEven(n - 1))

  def gEven(n: Int): Boolean ! State % Int = if n == 0 then pure(true) else State.get[Int].flatMap(_ => gOdd(n - 1))
  def gOdd(n: Int): Boolean ! State % Int = if n == 0 then pure(false) else State.get[Int].flatMap(_ => gEven(n - 1))

  def tEven(n: Int): Boolean ! Pure = if n == 0 then pure(true) else !.tailcall(tOdd(n - 1))
  def tOdd(n: Int): Boolean ! Pure = if n == 0 then pure(false) else !.tailcall(tEven(n - 1))

  private val mx = java.lang.management.ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
  private def perLevel(name: String)(run: => Any): Unit =
    for _ <- 1 to 30 do { val _ = run }
    val id = Thread.currentThread().threadId()
    val b0 = mx.getThreadAllocatedBytes(id)
    for _ <- 1 to 10 do { val _ = run }
    val b1 = mx.getThreadAllocatedBytes(id)
    println(f"PROBE $name%-44s ${(b1 - b0).toDouble / 10 / N}%8.1f B/level")

  test("bytes per level") {
    perLevel("row State.run(Writer.run(p))")(State.run[Int, (Seq[String], Boolean)](0)(Writer.run[String, Boolean, State % Int](even(N))))
    perLevel("row swapped Writer.run(State.handle(p))")(!.run(Writer.run[String, (Int, Boolean), Pure](State.handle[Int](0)(even(N)))))
    perLevel("State only, modify (get+set)")(State.run[Int, Boolean](0)(sEven(N)))
    perLevel("State only, set (one op)")(State.run[Int, Boolean](0)(oEven(N)))
    perLevel("State only, get (one op, no boxing)")(State.run[Int, Boolean](0)(gEven(N)))
    perLevel("Pure tailcall")(!.run(tEven(N)))
  }
