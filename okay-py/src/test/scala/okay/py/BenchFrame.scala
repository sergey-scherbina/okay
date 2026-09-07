package okay.py

import PyValue.*

/**
 * The measurement json-parse-fast-road came out of, kept because it
 * covers a path JMH cannot: a real frame through a real python3.
 *
 *   sbt "okayPy/Test/runMain okay.py.BenchFrame <dir with okay_bench.py>"
 *
 * The far side is one function:  def identity(frame): return frame
 */
import scala.util.boundary

object BenchFrame:
  def main(args: Array[String]): Unit = boundary:
    // `boundary/break` rather than a non-local return, which Scala 3
    // no longer supports (it warned, and the gate refuses warnings)
    val py = TestPy.python.getOrElse { println("no python3 on the PATH"); boundary.break() }
    val dir = args.headOption.getOrElse { println("usage: BenchFrame <dir holding okay_bench.py>"); boundary.break() }
    val w = PySubprocess.start(py, Map("PYTHONPATH" -> dir))
    try
      println("  rows | round trip | encode |  parse | lossless |   walk |     bytes")
      for rows <- Vector(1000, 10000, 100000, 500000) do
        val frame = PyFrame(Vector(
          "a" -> Vector.tabulate(rows)(i => F64(i.toDouble)),
          "b" -> Vector.tabulate(rows)(i => I64(i.toLong)),
          "s" -> Vector.tabulate(rows)(i => Str("row" + i))))
        w.handler.handle(PyEval.Frame("okay_bench:identity", frame, Vector.empty)): Unit  // warm

        val text = okay.codec.Json.print(Wire.encFrame(frame))
        val e0 = System.nanoTime()
        okay.codec.Json.print(Wire.encFrame(frame)): Unit
        val encMs = (System.nanoTime() - e0) / 1e6
        val f0 = System.nanoTime()
        val fast = okay.codec.Json.parse(text)
        val parseMs = (System.nanoTime() - f0) / 1e6
        val l0 = System.nanoTime()
        val slow = okay.codec.Json.lossless(text)
        val losslessMs = (System.nanoTime() - l0) / 1e6
        assert(fast == slow, "the two roads disagree")
        val d0 = System.nanoTime()
        Wire.decFrame(fast): Unit
        val walkMs = (System.nanoTime() - d0) / 1e6

        val t0 = System.nanoTime()
        val out = w.handler.handle(PyEval.Frame("okay_bench:identity", frame, Vector.empty))
        val ms = (System.nanoTime() - t0) / 1e6
        val back = out.map(_.cols.head._2.length).getOrElse(-1)
        assert(back == rows, s"round-tripped $back of $rows rows")
        println(f"$rows%6d | $ms%10.1f | $encMs%6.1f | $parseMs%6.1f | $losslessMs%8.1f | $walkMs%6.1f | $$${text.length}%9d")
    finally w.close()
