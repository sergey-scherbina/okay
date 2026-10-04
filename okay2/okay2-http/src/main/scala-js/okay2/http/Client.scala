package okay2.http

import scala.concurrent.ExecutionContext
import scala.scalajs.js
import scala.util.{Failure, Success}
import okay2.async.Async
import okay2.platform.Web

/**
 * The JS end of the acceptance run (okay-http's scala-js Client.scala).
 *
 * It runs `Acceptance.check` — the SAME program the JVM suite runs
 * against its own transports — over `fetch` and the global `WebSocket`,
 * and exits 0 only if every line of it holds. Exit 0 is the acceptance;
 * what is printed is for a human reading a failure.
 *
 * Nothing here blocks: `Async.runAsync` drives the tree through the
 * event loop, the one terminal on this platform (there is no `CanBlock`
 * on JS, a compile error rather than a hang — okay2 stage 32).
 */
object Client {

  def main(args: Array[String]): Unit = {
    val port = Web.Process.argv(2).toInt

    Async.runAsync(Acceptance.check(Transports.fetch, Transports.sockets(), port)).onComplete {
      case Success(results) =>
        results.foreach { case (what, ok) => println(s"${if (ok) "ok" else "FAILED"}  $what") }
        exit(if (results.forall(_._2)) 0 else 1)
      case Failure(e) =>
        println(s"FAILED  $e")
        exit(1)
    }(ExecutionContext.parasitic)
  }

  // Node's `process.exit`, the one call `Web.Process` does not type:
  // okay-http's Client reaches it the same way
  private def exit(code: Int): Unit = {
    val _ = js.Dynamic.global.process.exit(code)
  }
}
