package okay

/** Verifies that the A/B switches scripts/ab-defaults.sh drives actually
 * reach the running JVM. The channel class is SentinelChannel in BOTH
 * arms — `.each(n)` returns a Strong whose build wraps the buffer — so
 * the arm is only visible in the BUFFER, which is what this reads. */
object AbSwitchProbe:
  private def bufferOf(c: Any): String =
    var k: Class[?] = c.getClass
    val out = scala.collection.mutable.ListBuffer[String]()
    while k != null do
      k.getDeclaredFields.foreach { f =>
        try
          f.setAccessible(true)
          val v = f.get(c)
          if v != null && v.getClass.getSimpleName.matches(".*(Fifo|Ring|Segments|Buffer).*") then
            out += v.getClass.getSimpleName
        catch case _: Throwable => ()
      }
      k = k.getSuperclass
    if out.isEmpty then "?" else out.distinct.mkString(",")

  def main(args: Array[String]): Unit =
    val c = Channel[Int](8)
    println(s"BUFFER ${sys.props.getOrElse("okay.channel.buffer", "(unset)")} -> ${c.getClass.getSimpleName}[${bufferOf(c)}]")
    println(s"SCHED  ${sys.props.getOrElse("okay.scheduler", "(unset)")} -> ${summon[Scheduler].getClass.getSimpleName}")
    println(s"SPAWN  ${Async.spawn(async(42)).joinEither()}")
