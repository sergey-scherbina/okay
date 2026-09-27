package okay.testkit

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*

/**
 * A ROUND, MANY TIMES, COUNTING (specs/okay-test.md). A round answers None
 * when it held and Some(diagnosis) when it did not. The report says how
 * many failed and keeps the first diagnoses. `parallel` rounds run at
 * once, which is how the pool's wrong answer was found: a single loop of
 * 300 rounds never failed, and 8 concurrent loops failed 3 times in the
 * first few dozen.
 */
object Stress:
  final case class Report(rounds: Int, failed: Int, first: Vector[String]):
    def held: Boolean = failed == 0
    override def toString: String =
      s"$failed of $rounds rounds failed" + first.map("\n  " + _).mkString

  def repeat(n: Int, parallel: Int = 1, keep: Int = 3)(round: Int => Option[String]): Report =
    val failed = AtomicInteger(0)
    val first = ConcurrentLinkedQueue[String]()
    def worker(w: Int): Unit =
      var i = w
      while i < n do
        round(i) match
          case Some(d) =>
            failed.incrementAndGet(): Unit
            if first.size < keep then first.add(s"round $i: $d"): Unit
          case None => ()
        i += parallel
    if parallel <= 1 then worker(0)
    else
      val ts = (0 until parallel).map { w =>
        val t = new Thread(() => worker(w), s"okay-testkit-stress-$w")
        t.start()
        t
      }
      ts.foreach(_.join())
    Report(n, failed.get, first.asScala.toVector.take(keep))
