package okay

import scala.jdk.CollectionConverters.*

/**
 * cont-stack-cold-bytes-per-level: the count road sized for a COLD level. Each case runs `ColdRoomMain` in a
 * JVM of its own: the rooms are fixed when `StackSwitch` initialises, and only a fresh process starts cold.
 * Under `-Xint` a strict `k`'s level on `Delimited` takes ~2 520 B (2026-10-04: 367 levels fit 1 MB, 784
 * fit 2 MB, 1 617 fit 4 MB), so a room counted at 1 200 B a level is more than the stack holds.
 */
class TestColdRoom extends munit.FunSuite:

  override def munitTimeout = scala.concurrent.duration.Duration(5, "min")

  /** `ColdRoomMain depth levels` in a new JVM with `flags`: its last line, and everything it printed */
  private def cold(flags: List[String], depth: Int, levels: Int): (String, String) =
    val java = s"${System.getProperty("java.home")}/bin/java"
    val cmd = (java :: flags) ++ List("-cp", System.getProperty("java.class.path"), "okay.ColdRoomMain", depth.toString, levels.toString)
    val p = new ProcessBuilder(cmd.asJava).redirectErrorStream(true).start()
    val out = scala.io.Source.fromInputStream(p.getInputStream).mkString
    val code = p.waitFor()
    val last = out.linesIterator.toList.lastOption.getOrElse("")
    (last, s"exit $code, flags ${flags.mkString(" ")}, depth $depth, levels $levels:\n$out")

  test("a 1 MB default stack, interpreted, its caller 200 frames deep: the first room switches before it overflows") {
    val (last, all) = cold(List("-Xint", "-Xss1m"), depth = 200, levels = 3000)
    assert(last.startsWith("ok 6000 "), all)
    assert(last.split(' ')(2).toLong >= 1, s"no switch: 3 000 levels cannot fit a 1 MB stack\n$all")
  }

  test("a 2 MB default stack, interpreted: the same") {
    val (last, all) = cold(List("-Xint", "-Xss2m"), depth = 200, levels = 3000)
    assert(last.startsWith("ok 6000 "), all)
  }

  test("the fresh 1 GB stack, interpreted: its room fits 450 000 cold levels") {
    val (last, all) = cold(List("-Xint", "-Xss1m", "-Dokay.cont.room=100"), depth = 0, levels = 450000)
    assert(last.startsWith("ok 900000 "), all)
  }
