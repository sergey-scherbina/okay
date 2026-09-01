package okay.py

import okay.Handler

/**
 * The pool (stage 1): N resident workers behind ONE handler — the
 * parallelism story for a runtime whose GIL makes threads a dead
 * end. N processes hold N sets of imports; a virtual thread parks on
 * `take` when all are busy, which is the cluster's worker model, not
 * threads-under-GIL.
 *
 * Supervision is the parallel-resilience fault model: a worker that
 * DIES mid-call still throws to the caller (the in-flight answer is
 * gone and pretending otherwise would forge it), but the pool
 * replaces the corpse with a FRESH worker before rethrowing — the
 * caller's retry lands on live imports-cold state, correctness
 * unchanged, warmth re-earned.
 */
final class PyWorkers private (n: Int, python: String, env: Map[String, String]):

  private val pool = java.util.concurrent.ArrayBlockingQueue[PySubprocess](n)

  private[py] def prime(): Unit =
    var i = 0
    while i < n do { pool.put(PySubprocess.start(python, env)); i += 1 }

  /** the same shape as one worker's handler — programs cannot tell */
  def handler: Handler[PyEval] = new:
    def handle[A](e: PyEval[A]): A =
      val w = pool.take()
      var keep = true
      try w.handler.handle(e)
      catch
        case dead: IllegalStateException if dead.getMessage.contains("DEAD") =>
          keep = false
          w.close()
          pool.put(PySubprocess.start(python, env))   // the supervisor's move
          throw dead
      finally if keep then pool.put(w)

  /** verify on ONE worker — they are started identically, and the
   * environment either is or is not the one the program was written
   * against */
  def verify(packages: Map[String, String]): Vector[String] =
    val w = pool.take()
    try w.verify(packages) finally pool.put(w)

  def close(): Unit =
    var i = 0
    while i < n do
      val w = pool.poll()
      if w != null then w.close()
      i += 1

object PyWorkers:
  def start(n: Int, python: String = "python3",
            env: Map[String, String] = Map.empty): PyWorkers =
    require(n >= 1, "a pool needs a worker")
    val p = new PyWorkers(n, python, env)
    p.prime()
    p
