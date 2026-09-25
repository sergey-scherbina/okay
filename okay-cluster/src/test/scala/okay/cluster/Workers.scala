package okay.cluster

import java.io.InputStream
import java.util.concurrent.{CompletableFuture, ExecutionException, TimeUnit, TimeoutException}
import scala.concurrent.duration.*

/**
 * WORKER JVMs FOR A TEST, WITH DEADLINES (cluster-forked-stall,
 * 2026-09-25). A test that spawns `WorkerMain` processes waits for each
 * to print its port, and then for a run over their sockets. Both waits
 * had no deadline. Three default gates that day stalled with the test
 * fork and four `WorkerMain` children at 0% CPU for eight minutes and
 * nothing to say why, until the gate's watchdog killed the lot, workers
 * undumped.
 *
 * Past a deadline these FAIL, and the failure carries every worker's
 * thread dump (`jcmd <pid> Thread.print`), so the next stall names its
 * own cause.
 */
object Workers:

  /** `n` worker JVMs on the build's classpath, each running `registrar`
   * (its initialisation registers the jobs); `props(i)` are worker i's
   * `-D` options */
  def spawn(n: Int, registrar: String, props: Int => Seq[String] = _ => Nil): IndexedSeq[Process] =
    val cp = System.getProperty("okay.cluster.cp")
    munit.Assertions.assume(cp != null, "the test classpath was not handed over (see build.sbt)")
    (0 until n).map { i =>
      val cmd = Seq("java") ++ props(i) ++ Seq("-cp", cp, "okay.cluster.WorkerMain", "0", registrar)
      val pb = ProcessBuilder(cmd*)
      pb.redirectErrorStream(true)
      pb.start()
    }

  /** the line each worker prints once its socket is bound */
  def ports(procs: Seq[Process], deadline: FiniteDuration = 120.seconds): Vector[String] =
    announced(procs.map(_.getInputStream), deadline, () => dumps(procs))

  /**
   * The first `worker listening …` line of each stream, waited for at
   * most `deadline` in all. A stream that ends first fails at once; one
   * that says nothing until the deadline fails with `evidence()`.
   */
  def announced(streams: Seq[InputStream], deadline: FiniteDuration, evidence: () => String): Vector[String] =
    val lines = streams.map(in => CompletableFuture.supplyAsync(() =>
      scala.io.Source.fromInputStream(in).getLines().find(_.startsWith("worker listening"))))
    val end = System.nanoTime() + deadline.toNanos
    lines.zipWithIndex.map { (f, i) =>
      val left = math.max(0L, end - System.nanoTime())
      try f.get(left, TimeUnit.NANOSECONDS).getOrElse(munit.Assertions.fail(s"worker $i ended without announcing a port"))
      catch case _: TimeoutException =>
        munit.Assertions.fail(s"worker $i never announced a port within $deadline\n${evidence()}")
    }.toVector

  /** `body`, which must finish within `deadline`; past it, a failure
   * carrying `evidence()`. The body's own failure is rethrown as is. */
  def within[A](deadline: FiniteDuration, evidence: () => String)(body: => A): A =
    val f = CompletableFuture.supplyAsync(() => body)
    try f.get(deadline.toNanos, TimeUnit.NANOSECONDS)
    catch
      case _: TimeoutException =>
        munit.Assertions.fail(s"the run did not finish within $deadline\n${evidence()}")
      case e: ExecutionException if e.getCause != null => throw e.getCause

  /** every worker's thread dump, or why there is none */
  def dumps(procs: Seq[Process]): String =
    procs.map { p =>
      val head = s"---- worker pid ${p.pid()} alive=${p.isAlive}"
      if !p.isAlive then head
      else
        try
          val j = ProcessBuilder("jcmd", p.pid().toString, "Thread.print").redirectErrorStream(true).start()
          val out = CompletableFuture.supplyAsync(() => String(j.getInputStream.readAllBytes()))
          val text = out.get(30, TimeUnit.SECONDS)
          s"$head\n$text"
        catch case e: Exception => s"$head (no dump: $e)"
    }.mkString("\n")
