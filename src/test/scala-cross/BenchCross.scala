package okay

import okay.given
import scala.concurrent.Future

/**
 * bench-cross: the same four shapes timed on whichever platform
 * compiles this file -- JVM, JS or Native -- from one source.
 *
 * WHY A TEST AND NOT A BENCHMARK. JMH runs on the JVM only, and it is
 * the reference there; JS and Native have no JMH and no `@main` under
 * test, so the one cross-platform entry point is a munit suite. It is
 * tagged `Live`, so it is outside the default gate -- a measurement is
 * not a law -- and it runs on purpose. The build's `--exclude-tags=Live`
 * beats a `--include-tags` given after `--` (a test must match the
 * include AND escape the exclude, so it runs nothing), which is why
 * the `integrationTest` alias replaces the options instead; do the
 * same, per platform:
 *
 *   sbt 'set every Test / testOptions := Seq(Tests.Argument(TestFrameworks.MUnit, "--include-tags=Live"))' \
 *       "okayJVM/testOnly okay.BenchCross"
 *   ... and okayJS, okayNative in place of okayJVM.
 *
 * THE JVM COLUMN IS NOT JMH. Thirty warmup runs is not JMH's seconds
 * of warmup, and `runAsync` is not `runWith`: the first run of this
 * harness put the JVM's elementwise channel read at a 2949 median over
 * a 616 minimum, the JIT still compiling through the twenty samples.
 * Read the JVM column against its own minimum and against the JMH
 * figure for the same shape (docs/benchmarks.md §18); read JS and
 * Native, which this harness exists for, as they are -- Native is AOT
 * and its median sits on its minimum.
 *
 * WHAT IT MEASURES. Each lane is one program built the way the JMH
 * lanes build theirs, run through `Async.runAsync` -- the universal
 * terminal, no `CanBlock` anywhere, so JS can run it too -- a warmup,
 * then the median of `Runs`. It prints one line per lane:
 *
 *   bench-cross | <platform> | <lane> | median us | min us
 *
 * and asserts only that the answer was right. A ruler, not a scale:
 * the numbers are comparable ACROSS platforms for one lane and
 * against the JVM's JMH figure for the same shape; they are not JMH's
 * forks-and-iterations, and the ledger says so when they are copied.
 */
class BenchCross extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private given scala.concurrent.ExecutionContext = munitExecutionContext

  final val N = 4000
  final val Warmup = 30
  final val Runs = 20
  private val list: List[Long] = (0L until N.toLong).toList
  private val expected: Long = N.toLong * (N - 1) / 2

  private val platform: String =
    val vm = System.getProperty("java.vm.name", "")
    if vm.contains("Scala.js") then "js"
    else if vm.contains("Native") then "native"
    else "jvm"

  /** run `mk()` Warmup + Runs times in sequence, timing each, and
   * report the median and the minimum; the answer is checked every time */
  private def lane(name: String)(mk: () => Long ! Async): Future[Unit] =
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
      println(f"bench-cross | $platform%-6s | $name%-16s | $median%9.1f | $min%9.1f")
    }

  test("bench: rangeFold -- Source.range through runForeach, N elements") {
    lane("rangeFold") { () =>
      var sum = 0L
      Source.range(0L, N.toLong).runForeach(x => okay.effect[Async, Unit](Async.Run(() => sum += x)))
        .map(_ => sum)
    }
  }

  test("bench: channelElem -- Channel.buffer(1024).drained read one element at a time") {
    lane("channelElem") { () =>
      var sum = 0L
      Channel.buffer(1024)(list).drained.runForeach(x => okay.effect[Async, Unit](Async.Run(() => sum += x)))
        .map(_ => sum)
    }
  }

  test("bench: channelChunks -- the same channel through drainedChunks") {
    lane("channelChunks") { () =>
      var sum = 0L
      Channel.buffer(1024)(list).drainedChunks.runForeach(ch => okay.effect[Async, Unit](Async.Run(() =>
        var i = 0
        while i < ch.length do { sum += ch(i); i += 1 })))
        .map(_ => sum)
    }
  }

  test("bench: bindChain -- N nested flatMaps through the interpreter, no channel") {
    lane("bindChain") { () =>
      def go(i: Long, acc: Long): Long ! Async =
        if i >= N then okay.pure(acc)
        else async(i).flatMap(x => go(i + 1, acc + x))
      go(0L, 0L)
    }
  }
