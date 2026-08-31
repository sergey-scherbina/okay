package okay.cluster

import okay.{Aggregator, Chunk, Chunks}
import okay.codec.Json
import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.{ServerSocket, Socket}

/** The fault model: a dead worker's chunks recompute on a survivor. */
class TestCluster extends munit.FunSuite {

  val agg = Aggregator.variance[Double]
  type Acc = Aggregator.Variance      // Welford: count, mean, m2, flat

  def source: Chunks[Double] = Chunks.map(Chunks.range(0, 1000, 16))(x => x * 0.5 + 1)

  def foldChunk(c: Chunk[Double]): Acc = c.foldLeft(agg.init)(agg.add)

  test("a killed in-process worker's chunks recompute; variance stays exact") {
    val whole = agg.present(Chunks.fold(source)(using agg.fold))

    var served = 0
    val dying: Cluster.Worker[Double, Acc] = c =>
      served += 1
      if served > 2 then throw RuntimeException("worker down")
      foldChunk(c)
    val healthy: Cluster.Worker[Double, Acc] = foldChunk

    val acc = Cluster.distribute(source, Vector(dying, healthy))(agg.init, agg.merge)
    // Welford merge drifts by float ulps depending on split — tolerance
    assert(math.abs(agg.present(acc) - whole) < 1e-9)
    assertEquals(acc._1, 1000L)   // every element counted exactly once
  }

  test("a socket worker dies mid-stream; the wire chunks recompute on the local one") {
    // the remote end: reads a JSON chunk per line, answers the SUM —
    // and drops the connection after two chunks (the kill)
    val server = ServerSocket(0)
    val remote = Thread.ofVirtual().start { () =>
      val sock = server.accept()
      val in = BufferedReader(InputStreamReader(sock.getInputStream))
      val out = PrintWriter(sock.getOutputStream, true)
      var n = 0
      var line = in.readLine()
      while line != null && n < 2 do
        Json.read[List[Double]](line).foreach(xs => out.println(Json.write(xs.sum)))
        n += 1
        line = if n < 2 then in.readLine() else null
      sock.close()
    }

    lazy val conn = Socket("localhost", server.getLocalPort)
    lazy val out = PrintWriter(conn.getOutputStream, true)
    lazy val in = BufferedReader(InputStreamReader(conn.getInputStream))
    val wire: Cluster.Worker[Double, Double] = c =>
      out.println(Json.write(c.toList))
      in.readLine() match
        case null => throw RuntimeException("connection lost")
        case s => Json.read[Double](s).fold(m => throw RuntimeException(m), identity)
    val local: Cluster.Worker[Double, Double] = _.sum

    val total = Cluster.distribute(source, Vector(wire, local))(0.0, _ + _)
    val expected = Chunks.fold(source)(using okay.Fold.sum[Double])
    assert(math.abs(total - expected) < 1e-9,
      s"lost or doubled a chunk: $total vs $expected")
    remote.join()
    server.close()
  }
}
