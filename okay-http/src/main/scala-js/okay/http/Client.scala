package okay.http

import okay.*
import okay.given
import scala.scalajs.js
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

/**
 * The JS end of the acceptance run.
 *
 * It runs `Acceptance.check` — the SAME program the JVM suite runs
 * against its own transports — over `fetch` and the global
 * `WebSocket`, and exits 0 only if every line of it holds. Exit 0 is
 * the acceptance; anything printed is for a human reading a failure.
 *
 * Nothing here may block: `Async.runAsync` drives the tree through the
 * event loop, which is the one terminal that works on this platform
 * (there is no `CanBlock` on JS, by design — `cross-platform-async.md`
 * makes that a compile error rather than a hang).
 */
object Client {

  def main(args: Array[String]): Unit =
    val argv = okay.Web.Process.argv
    val port = argv(2).toInt

    Async.runAsync(
      Acceptance.check(Transports.fetch, Transports.sockets(), port)
    ).onComplete {
      case Success(results) =>
        results.foreach((what, ok) => println(s"${if ok then "ok" else "FAILED"}  $what"))
        exit(if results.forall(_._2) then 0 else 1)
      case Failure(e) =>
        println(s"FAILED  $e")
        exit(1)
    }

  private def exit(code: Int): Unit =
    js.Dynamic.global.process.exit(code)
    ()
}
