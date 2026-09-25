package okay.cluster

import java.io.{ByteArrayInputStream, PipedInputStream, PipedOutputStream}
import scala.concurrent.duration.*

/**
 * cluster-forked-stall: the waits a worker test makes end, and a stall
 * becomes a failure that carries its evidence (Workers). No process is
 * started here: a worker's output is a stream, and a silent worker is a
 * pipe nobody writes to.
 */
class TestWorkers extends munit.FunSuite {

  test("a worker that says its port is read, and one that ends first fails at once") {
    val ok = ByteArrayInputStream("booting\nworker listening 4711 knowing test.window\n".getBytes)
    assertEquals(Workers.announced(Seq(ok), 5.seconds, () => "unused"), Vector("worker listening 4711 knowing test.window"))
    val e = intercept[munit.FailException](Workers.announced(Seq(ByteArrayInputStream(Array.emptyByteArray)), 5.seconds, () => "unused"))
    assert(e.getMessage.contains("ended without announcing"), e.getMessage)
  }

  test("a SILENT worker fails at the deadline, carrying the evidence, instead of waiting for ever") {
    val silent = PipedInputStream(PipedOutputStream())
    val t0 = System.nanoTime()
    val e = intercept[munit.FailException](Workers.announced(Seq(silent), 300.millis, () => "THE DUMP"))
    assert((System.nanoTime() - t0) < 10.seconds.toNanos, "it waited far past the deadline")
    assert(e.getMessage.contains("never announced a port within 300 milliseconds"), e.getMessage)
    assert(e.getMessage.contains("THE DUMP"), e.getMessage)
  }

  test("a run past its deadline fails with the evidence; a run's own failure comes through unchanged") {
    val e = intercept[munit.FailException](Workers.within(300.millis, () => "THE DUMP")(Thread.sleep(5000)))
    assert(e.getMessage.contains("did not finish within"), e.getMessage)
    assert(e.getMessage.contains("THE DUMP"), e.getMessage)
    val own = intercept[IllegalStateException](Workers.within(5.seconds, () => "unused")(throw IllegalStateException("its own")))
    assertEquals(own.getMessage, "its own")
    assertEquals(Workers.within(5.seconds, () => "unused")(42), 42)
  }
}
