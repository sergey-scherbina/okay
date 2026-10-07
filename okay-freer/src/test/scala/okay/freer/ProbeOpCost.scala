package okay.freer


/** effect-op-cost: EXACT bytes per operation, the operation built fresh
 * (as `State.get` builds it) against a shared node */
class ProbeOpCost extends munit.FunSuite:
  val N = 100000

  // the shipping road: effect(Get()) per call
  def fEven(n: Int): Boolean ! State % Int = if n == 0 then pure(true) else State.get[Int].flatMap(_ => fOdd(n - 1))
  def fOdd(n: Int): Boolean ! State % Int = if n == 0 then pure(false) else State.get[Int].flatMap(_ => fEven(n - 1))

  // PROBE ONLY: one Inject(Get()) node shared by every call — the cast is
  // the question this probe asks (erasure makes Get[Int] and Get[Any] the
  // same object), not a proposal
  private val sharedGet: Int ! State % Int = Free.Inject(State.Get[Int, Int]())
  def sEven(n: Int): Boolean ! State % Int = if n == 0 then pure(true) else sharedGet.flatMap(_ => sOdd(n - 1))
  def sOdd(n: Int): Boolean ! State % Int = if n == 0 then pure(false) else sharedGet.flatMap(_ => sEven(n - 1))

  def rEven(n: Int): Boolean ! Reader % Int = if n == 0 then pure(true) else Reader.ask[Int].flatMap(_ => rOdd(n - 1))
  def rOdd(n: Int): Boolean ! Reader % Int = if n == 0 then pure(false) else Reader.ask[Int].flatMap(_ => rEven(n - 1))
  private val sharedAsk: Int ! Reader % Int = Free.Inject(Reader.Ask[Int, Int]())
  def raEven(n: Int): Boolean ! Reader % Int = if n == 0 then pure(true) else sharedAsk.flatMap(_ => raOdd(n - 1))
  def raOdd(n: Int): Boolean ! Reader % Int = if n == 0 then pure(false) else sharedAsk.flatMap(_ => raEven(n - 1))

  def tEven(n: Int): Boolean ! Pure = if n == 0 then pure(true) else !.tailcall(tOdd(n - 1))
  def tOdd(n: Int): Boolean ! Pure = if n == 0 then pure(false) else !.tailcall(tEven(n - 1))

  private val mx = java.lang.management.ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
  private def perLevel(name: String)(run: => Any): Unit =
    for _ <- 1 to 30 do { val _ = run }
    val id = Thread.currentThread().threadId()
    val b0 = mx.getThreadAllocatedBytes(id)
    val t0 = System.nanoTime()
    for _ <- 1 to 20 do { val _ = run }
    val t1 = System.nanoTime()
    val b1 = mx.getThreadAllocatedBytes(id)
    println(f"PROBE $name%-36s ${(b1 - b0).toDouble / 20 / N}%7.1f B/level  ${(t1 - t0).toDouble / 20 / N}%6.2f ns/level (rough)")

  test("bytes per operation, fresh against shared") {
    perLevel("State.get, fresh (shipping)")(State.run[Int, Boolean](0)(fEven(N)))
    perLevel("State.get, shared node")(State.run[Int, Boolean](0)(sEven(N)))
    perLevel("Reader.ask, fresh (shipping)")(!.run(Reader.run[Int, Boolean, Pure](0)(rEven(N))))
    perLevel("Reader.ask, shared node")(!.run(Reader.run[Int, Boolean, Pure](0)(raEven(N))))
    perLevel("!.tailcall, no effect")(!.run(tEven(N)))
  }
