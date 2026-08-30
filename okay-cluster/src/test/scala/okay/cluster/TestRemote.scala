package okay.cluster

import okay.{Aggregator, Chunk, Chunks}
import okay.given
import okay.codec.Schema
import java.net.ServerSocket

/** Two ends of a wire: chunks cross, merges agree with the local run. */
class TestRemote extends munit.FunSuite {

  test("chunks cross the wire and the merged fold equals the local run") {
    val xs = (1 to 1000).map(_.toDouble).toList
    val (local, remote) = xs.splitAt(400)
    val agg = Aggregator.variance[Double]

    val server = ServerSocket(0)
    val received = Remote.listen[Double](server)

    // "the other node": ships its part in chunks of 64
    val sender = Remote.connect[Double]("localhost", server.getLocalPort)
    remote.grouped(64).foreach(g =>
      sender.send(Chunks.wrap[Double](g.map(_.asInstanceOf[AnyRef]).toArray)))
    sender.close()

    // this node: fold the remote chunks into a partial accumulator
    var remoteAcc = agg.init
    var c = received.receive()
    while c.isDefined do
      remoteAcc = c.get.foldLeft(remoteAcc)(agg.add)
      c = received.receive()

    val localAcc = local.foldLeft(agg.init)(agg.add)
    val together = agg.present(agg.merge(localAcc, remoteAcc))
    assert(math.abs(together - agg.run(xs)) / agg.run(xs) < 1e-9,
      s"$together vs ${agg.run(xs)}")
    server.close()
  }

  test("a damaged frame is dropped; the stream lives; close drains") {
    val server = ServerSocket(0)
    val received = Remote.listen[Long](server)
    val sock = java.net.Socket("localhost", server.getLocalPort)
    val out = java.io.PrintWriter(sock.getOutputStream, true)
    out.println("[1,2,3]")
    out.println("{{{ damaged")
    out.println("[4,5]")
    sock.close()
    var all = List.empty[Long]
    var c = received.receive()
    while c.isDefined do { all = all ++ c.get; c = received.receive() }
    assertEquals(all, List(1L, 2L, 3L, 4L, 5L))
    server.close()
  }
}
