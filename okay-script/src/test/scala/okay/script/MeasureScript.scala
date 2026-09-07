package okay.script

import okay.*
import okay.given
import okay.http.{Http, Request, Response as HttpResponse}

import java.nio.file.{Files, Path}

/** What a page COSTS -- the numbers okay-script had never had
 * (okay-script-measured). See docs/benchmarks.md "okay-script".
 *
 * Deliberately NOT JMH: every sample here is milliseconds to seconds
 * (a page's first render is the Scala compiler), and each JMH fork
 * would pay every compile again for a number whose variance is
 * dominated by dotc, not by the harness. So: medians of `n` samples
 * (the median absorbing the first slow runs, see `msOf`), the host's
 * load average recorded beside
 * them, printed as a table. The ASSERTIONS are sanity bounds only --
 * a page renders, a warm render is cheaper than a cold one -- never a
 * millisecond threshold, which on a loaded CI box is a red build that
 * says nothing about the code.
 */
class MeasureScript extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private def median(xs: Vector[Double]): Double =
    val s = xs.sorted
    if s.isEmpty then 0.0 else if s.length % 2 == 1 then s(s.length / 2) else (s(s.length / 2 - 1) + s(s.length / 2)) / 2

  /**
   * The MEDIAN of `n` runs. It discards no warmup — the median is
   * what absorbs the first slow runs, and that is enough HERE
   * because every body below is hundreds of milliseconds of dotc (or
   * the warm arm, which takes 200 samples).
   *
   * It would not be enough on a short body. `sql-plan-cells`
   * measured a 0.6 ms body this way with 3 samples and published a
   * number three times too high: two runs of the same code differed
   * by half, and it took 50 discarded warmups and 31 samples to
   * settle. Anything sub-millisecond wants a real warmup, or JMH.
   */
  private def msOf(n: Int)(body: => Unit): Double =
    val xs = Vector.fill(n) {
      val t = System.nanoTime()
      body
      (System.nanoTime() - t) / 1e6
    }
    median(xs)

  private val rows = scala.collection.mutable.ArrayBuffer.empty[(String, String, String)]
  private def row(what: String, value: String, note: String): Unit =
    rows += ((what, value, note))
    println(f"  $what%-46s $value%12s   $note")

  override def afterAll(): Unit =
    println("\n| what | median | note |")
    println("|---|---:|---|")
    rows.foreach((w, v, n) => println(s"| $w | $v | $n |"))
    val load = java.lang.management.ManagementFactory.getOperatingSystemMXBean.getSystemLoadAverage
    val cpus = Runtime.getRuntime.availableProcessors
    println(f"%nhost: $cpus cpus, load average $load%.2f")

  private def text(r: HttpResponse): String = Async.run[String, Pure](Http.text(r)).runWith

  private def tempRoot(): Path = Files.createTempDirectory("okay-script-measure-")
  private def rmrf(p: Path): Unit =
    Files.walk(p).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(q => Files.deleteIfExists(q): Unit)

  private val tiny = "one line, no metadata\n"
  private val code = "```scala\nimport okay.script.api.*\nval n = (1 to 10).sum\n```\nsum ${n} for ${Web.current.path}\n"
  private val meta =
    """---
      |title: Measured
      |cache: 60
      |---
      |# ${okay.script.Meta.current("title")}
      |
      |```scala
      |import okay.script.api.*
      |val items = Vector("a", "b", "c")
      |```
      |
      |```yaml
      |kind: page
      |```
      |
      |${items.mkString(", ")} at ${Web.current.path}
      |""".stripMargin

  test("what a page costs: cold compile, warm invoke, recompile, static, 304, memory") {
    val root = tempRoot()
    try
      Files.writeString(root.resolve("tiny.md"), tiny): Unit
      Files.writeString(root.resolve("code.md"), code): Unit
      Files.writeString(root.resolve("meta.md"), meta): Unit
      Files.writeString(root.resolve("style.css"), "body{color:red}\n" * 40): Unit
      val site = Site(root)
      try
        // the FIRST render of the FIRST page pays dotc's own warmup as
        // well as this page's compile: reported separately, because a
        // server pays it once and a benchmark that hides it lies
        val firstEver = msOf(1)(site.handle(Request.get("/tiny")): Unit)
        row("cold render, first page of the process", f"$firstEver%.0f ms", "includes dotc's own warmup")

        def coldOf(name: String, content: String): Double =
          msOf(5) {
            val f = root.resolve(s"$name-${System.nanoTime()}.md")
            Files.writeString(f, content): Unit
            val s2 = Site(root)
            try s2.handle(Request.get(s"/${f.getFileName.toString.dropRight(3)}")): Unit
            finally
              s2.close()
              Files.deleteIfExists(f): Unit
          }
        val coldTiny = coldOf("tiny", tiny)
        val coldCode = coldOf("code", code)
        val coldMeta = coldOf("meta", meta)
        row("cold render, prose only", f"$coldTiny%.0f ms", "compile + invoke, warm JVM")
        row("cold render, a code block", f"$coldCode%.0f ms", "compile + invoke")
        row("cold render, front-matter + yaml + code", f"$coldMeta%.0f ms", "the Meta plumbing too")

        site.handle(Request.get("/code")): Unit // compiled once
        val warm = msOf(200)(site.handle(Request.get("/code")): Unit)
        row("warm render (cached compile, re-invoked)", f"$warm%.3f ms", "what a request actually costs")
        assert(warm < coldCode, f"a warm render ($warm%.3f ms) must be cheaper than a cold one ($coldCode%.0f ms)")

        var stamp = System.currentTimeMillis()
        val reload = msOf(5) {
          stamp += 2000
          Files.setLastModifiedTime(root.resolve("code.md"), java.nio.file.attribute.FileTime.fromMillis(stamp)): Unit
          site.handle(Request.get("/code")): Unit
        }
        row("hot reload (mtime changed, recompiled)", f"$reload%.0f ms", "the edit-refresh loop")

        val static = msOf(200)(site.handle(Request.get("/style.css")): Unit)
        val etag = site.handle(Request.get("/style.css")).headers.collectFirst { case (k, v) if k.equalsIgnoreCase("etag") => v }.get
        val notModified = msOf(200)(site.handle(Request.get("/style.css", Seq("If-None-Match" -> etag))): Unit)
        row("static file, 200 (read + ETag)", f"$static%.3f ms", "size+mtime ETag, no digest")
        row("static file, 304 (validated)", f"$notModified%.3f ms", "not read at all")

        // memory a compiled page holds: measure a Site with N pages
        // against one with none, after settling the heap
        def used(): Long =
          System.gc(); Thread.sleep(150); System.gc(); Thread.sleep(150)
          Runtime.getRuntime.totalMemory - Runtime.getRuntime.freeMemory
        val pages = 20
        val dir = tempRoot()
        for i <- 1 to pages do Files.writeString(dir.resolve(s"p$i.md"), code): Unit
        val before = used()
        val loaded = Site(dir)
        try
          for i <- 1 to pages do loaded.handle(Request.get(s"/p$i")): Unit
          val after = used()
          val perPage = (after - before).toDouble / pages / 1024
          row("memory held per compiled page", f"$perPage%.0f KiB", s"$pages pages, heap delta after GC")
        finally
          loaded.close()
          rmrf(dir)
      finally site.close()
    finally rmrf(root)
  }

  test("concurrency: renders per second on one Site, one page, many threads") {
    val root = tempRoot()
    try
      Files.writeString(root.resolve("index.md"), code): Unit
      val site = Site(root)
      try
        site.handle(Request.get("/")): Unit // compile once
        for threads <- Vector(1, 2, 4, 8) do
          // long enough to be a measurement rather than a timer read:
          // at ~20k renders/s per thread, this is about a second each
          val perThread = 20000
          val pool = java.util.concurrent.Executors.newFixedThreadPool(threads)
          val latch = new java.util.concurrent.CountDownLatch(threads)
          val t0 = System.nanoTime()
          for _ <- 1 to threads do
            pool.execute { () =>
              try for _ <- 1 to perThread do site.handle(Request.get("/")): Unit
              finally latch.countDown()
            }
          latch.await()
          val secs = (System.nanoTime() - t0) / 1e9
          pool.shutdown()
          val rps = threads * perThread / secs
          row(s"renders/second, $threads thread(s)", f"$rps%.0f", f"${threads * perThread} renders in $secs%.2f s")
        assert(text(site.handle(Request.get("/"))).contains("sum 55"))
      finally site.close()
    finally rmrf(root)
  }
