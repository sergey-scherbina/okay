package okay.cluster

import scala.scalajs.js
import okay.{!, Async, async, await}
import okay.codec.Json

/**
 * The JS end of the acceptance run: connect to the JVM server over
 * Node's `net`, send the shared source's frames, await the folded
 * answer through the event loop (runAsync — nothing here may block),
 * and verify it against the SAME shared-source computation. Exit 0
 * is the acceptance.
 */
object Client {

  def main(args: Array[String]): Unit =
    val argv = okay.Web.Process.argv
    val port = argv(2).toInt
    val net = js.Dynamic.global.require("net")

    val prog: Boolean ! Async =
      for
        sock <- async(net.connect(port, "127.0.0.1"))
        _ <- async {
          Acceptance.frames.foreach(f => sock.write(f + "\n"))
          sock.end()   // half-close: the server folds at EOF, then answers
        }
        line <- await[String] { k =>
          var buf = ""
          var fired = false
          val _ = sock.on("data", { (d: js.Any) =>
            buf += d.toString
            val i = buf.indexOf('\n')
            if i >= 0 && !fired then
              fired = true
              k(buf.substring(0, i))
          })
          ()
        }
      yield Json.read[Double](line).exists(v => math.abs(v - Acceptance.expected) < 1e-9)

    Async.runAsync(prog).foreach { ok =>
      if !ok then { val _ = js.Dynamic.global.console.error("acceptance mismatch") }
      js.Dynamic.global.process.exit(if ok then 0 else 1)
    }(using scala.scalajs.concurrent.JSExecutionContext.queue)
}
