package okay

/**
 * The count road's first room in a COLD JVM (cont-stack-cold-bytes-per-level), run as its own process by
 * TestColdRoom: a thread of the VM's default size, its caller `depth` frames deep already, then `levels` opaque
 * answer-using shifts (`k(x + 1) + 1`, the strict leaf), each a level on this stack until the room runs out.
 * Prints `ok <answer> <switches>`, or `overflow` when the stack ran out before the room did.
 */
object ColdRoomMain:

  private def deep(n: Int, levels: Int): Int =
    if n == 0 then
      Cont.reset((1 to levels).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) =>
        m.flatMap(x => Cont.shiftLeaf[Int, Int, Int](k => k(x + 1) + 1))))
    else deep(n - 1, levels) + 0

  def main(args: Array[String]): Unit =
    val depth = args(0).toInt
    val levels = args(1).toInt
    var out = "never ran"
    val t = new Thread(null, () =>
      out =
        try
          val before = StackSwitch.switches.get()
          val r = deep(depth, levels)
          s"ok $r ${StackSwitch.switches.get() - before}"
        catch case _: StackOverflowError => "overflow",
      "cold", 0L)
    t.start()
    t.join()
    println(s"firstRoom=${StackSwitch.firstRoom} default=${StackSwitch.defaultStackBytes}")
    println(out)
