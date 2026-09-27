package okay.testkit

import java.util.concurrent.atomic.AtomicBoolean

/**
 * EVERY CORE BUSY BESIDE THE BODY: the loaded box a whole-build gate is,
 * reproduced in one suite. Two flakes of 2026-09-27 went from never to
 * several in a few dozen rounds under it (own-monitor-burst-load-flake,
 * pool-repeat-post-early-status). The burners stop when the body ends,
 * normally or by a throw.
 */
object Load:
  def cores: Int = Runtime.getRuntime.availableProcessors

  def burners[A](n: Int = cores)(body: => A): A =
    val go = AtomicBoolean(true)
    val ts = (0 until n).map { i =>
      val t = new Thread(() => {
        var x = 0L
        while go.get do x += System.nanoTime() & 1
        if x < 0 then println(x) // keeps the loop from being optimised away
      }, s"okay-testkit-burner-$i")
      t.setDaemon(true)
      t.start()
      t
    }
    try body
    finally
      go.set(false)
      ts.foreach(_.join())
