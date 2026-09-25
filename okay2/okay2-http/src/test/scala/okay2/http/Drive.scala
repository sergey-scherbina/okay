package okay2.http

import okay2.!
import okay2.async.{Accepted, Async, CanBlock, Handoff}

/** a runner that executes `Async.Run` in place and FAILS on any wait:
 * a handler answering a pure body never waits, so these suites run on
 * Scala.js and Scala Native too */
object Drive {
  implicit val noPark: CanBlock = new CanBlock {
    private def no: Nothing = throw new AssertionError("an Async wait where none was expected")
    def block[A](register: (A => Unit) => (() => Unit)): A = no
    def blockAccepted(register: Accepted => (() => Unit)): Boolean = no
    def handoff[A](): Handoff[A] = no
    def await(h: Handoff[_]): Unit = no
  }

  def run[A](p: A ! Async): A = !.run(Async.run[A, okay2.Pure](p))

  /** a router's answer: status, content-type, body text */
  def answer(r: Router, req: Request): (Int, String, String) = {
    val res = run(r.routes(req))
    val ct = res.headers.collectFirst { case (k, v) if k.equalsIgnoreCase("content-type") => v }.getOrElse("")
    (res.status, ct, run(Http.text(res)))
  }
}
