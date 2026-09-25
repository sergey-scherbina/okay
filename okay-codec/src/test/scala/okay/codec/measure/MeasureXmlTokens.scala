package okay.codec.measure

import okay.codec.Xml
import okay.lex.Scan

/** xml-tokens-stream's measurement, not a test: `scan` against `tokens`
 * over real files, tokens counted, time and bytes allocated per pass.
 * `Test/runMain okay.codec.measure.MeasureXmlTokens <file>…` */
object MeasureXmlTokens:
  private val bean = java.lang.management.ManagementFactory.getThreadMXBean
    .asInstanceOf[com.sun.management.ThreadMXBean]
  private def allocated = bean.getThreadAllocatedBytes(Thread.currentThread.threadId)

  def main(args: Array[String]): Unit =
    val files = args.toVector.map(java.nio.file.Path.of(_))
    def viaScan(): Long = files.map { f =>
      val text = java.nio.file.Files.readString(f)
      Scan.fold(Xml.scan)(text)(0L)((n, _) => n + 1)
    }.sum
    def viaTokens(): Long = files.map { f =>
      var n = 0L
      val in = java.nio.file.Files.newBufferedReader(f)
      try Xml.tokens(in)(_ => n += 1) finally in.close()
      n
    }.sum
    def once(run: () => Long): (Long, Long, Long) =
      System.gc()
      val (a0, t0) = (allocated, System.nanoTime)
      val n = run()
      ((System.nanoTime - t0) / 1_000_000, (allocated - a0) / 1_048_576, n)
    // warm both, then alternate: five rounds each
    val _ = (once(viaScan), once(viaTokens))
    val rounds = (1 to 5).map(_ => (once(viaScan), once(viaTokens)))
    def med(xs: Seq[Long]) = xs.sorted.apply(xs.size / 2)
    val (s, t) = rounds.unzip
    println(s"tokens: ${s.head._3} (scan) ${t.head._3} (tokens)")
    println(s"scan   ms ${med(s.map(_._1))} MB ${med(s.map(_._2))}")
    println(s"tokens ms ${med(t.map(_._1))} MB ${med(t.map(_._2))}")
