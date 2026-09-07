package okay

import scala.concurrent.Future

/**
 * stm-js-direct-bench: the two STM handlers priced on whichever
 * platform compiles this file — the same program through `Stm.tl2`
 * (versions, a CAS-owned commit) and through `Stm.direct` (one
 * thread, no log: a transaction is atomic by construction, the JS
 * given), single fibre, no contention. What is measured is the
 * handler's own cost per transaction, which is why a `control` lane
 * runs the same chain with `async(i)` in place of the transaction:
 * the difference between a lane and the control is the transaction.
 *
 * Same harness rules as `BenchCross` (read its header): Live-tagged,
 * `runAsync`, thirty warmups, the median of twenty and the minimum,
 * in microseconds, one line per lane:
 *
 *   bench-stm | <platform> | <lane> | median us | min us
 *
 * Run it per platform with the include-tags form BenchCross shows;
 * `BENCH_LANES=` filters as there.
 */
class BenchStmCross extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private given scala.concurrent.ExecutionContext = munitExecutionContext

  final val N = 4000
  final val Warmup = 30
  final val Runs = 20
  private val expected: Long = N.toLong * (N - 1) / 2

  private val platform: String =
    val vm = System.getProperty("java.vm.name", "")
    if vm.contains("Scala.js") then "js"
    else if vm.contains("Native") then "native"
    else "jvm"

  private val only: Set[String] =
    val raw: java.lang.String | Null = System.getenv("BENCH_LANES")
    if raw == null then Set.empty
    else
      val parts: Array[java.lang.String] = raw.split(",")
      parts.toList.map(p => p.trim).filter(p => p.nonEmpty).toSet

  private def lane(name: String)(mk: () => Long ! Async): Future[Unit] =
    if only.nonEmpty && !only(name) then return Future.unit
    def once(): Future[Long] =
      val t0 = System.nanoTime()
      Async.runAsync(mk()).map { sum =>
        val dt = System.nanoTime() - t0
        assertEquals(sum, expected, s"$name answered wrong")
        dt
      }
    def loop(i: Int, acc: List[Long]): Future[List[Long]] =
      if i >= Warmup + Runs then Future.successful(acc)
      else once().flatMap(dt => loop(i + 1, if i < Warmup then acc else dt :: acc))
    loop(0, Nil).map { samples =>
      val sorted = samples.sorted
      val median = sorted(sorted.length / 2) / 1000.0
      val min = sorted.head / 1000.0
      println(f"bench-stm | $platform%-6s | $name%-16s | $median%9.1f | $min%9.1f")
    }

  /** N transactions in a chain, each answering the ref's OLD value, so
   * the sum is the same 0..N-1 the control adds up */
  private def chain(s: Stm[Async], tx: TRef[Long] => Long ! Tx): Long ! Async =
    val r = TRef(0L)
    def go(i: Long, acc: Long): Long ! Async =
      if i >= N then okay.pure(acc)
      else s.atomically(tx(r)).flatMap(x => go(i + 1, acc + x))
    go(0L, 0L)

  private def modify(r: TRef[Long]): Long ! Tx = Tx.modify(r)(x => (x + 1, x))
  private def readWrite(r: TRef[Long]): Long ! Tx =
    Tx.read(r).flatMap(x => Tx.write(r, x + 1).map(_ => x))

  test("bench: control -- the same chain with async(i), no transaction") {
    lane("control") { () =>
      def go(i: Long, acc: Long): Long ! Async =
        if i >= N then okay.pure(acc)
        else async(i).flatMap(x => go(i + 1, acc + x))
      go(0L, 0L)
    }
  }

  test("bench: tl2Modify -- one Modify per transaction, Stm.tl2") {
    lane("tl2Modify") { () => chain(Stm.tl2, modify) }
  }

  test("bench: directModify -- one Modify per transaction, Stm.direct") {
    lane("directModify") { () => chain(Stm.direct, modify) }
  }

  test("bench: tl2ReadWrite -- a Read then a Write per transaction, Stm.tl2") {
    lane("tl2ReadWrite") { () => chain(Stm.tl2, readWrite) }
  }

  test("bench: directReadWrite -- a Read then a Write per transaction, Stm.direct") {
    lane("directReadWrite") { () => chain(Stm.direct, readWrite) }
  }
