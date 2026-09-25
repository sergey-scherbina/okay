package okay.cluster

import okay.Aggregator
import okay.given
import okay.codec.Schema
import java.net.ServerSocket

object RemoteModel:
  final case class Trade(id: Long, symbol: String, price: Double, qty: Int, note: Option[String]) derives Schema
  /**
   * A server on THE loopback address, which the sender then connects to by
   * that address — never "localhost". On macOS "localhost" tries ::1 first,
   * and on a box where other processes bind ephemeral ports a stranger can
   * be listening on the same port number over IPv6: the sender then talks
   * to it (a broken pipe, or bytes into nowhere while our listener waits in
   * `accept` for good). Both were seen measuring Remote (remote-arrow-frames),
   * and the stall's thread dump showed our listener still in accept after
   * the sender had finished.
   */
  def loopback(): ServerSocket = ServerSocket(0, 50, java.net.InetAddress.getLoopbackAddress)

  def trades(n: Int, from: Int = 0): List[Trade] = List.tabulate(n)(i =>
    Trade((from + i).toLong, Vector("AAPL", "MSFT", "чай")(i % 3), 100.0 + i * 0.25, i % 50, Option.when(i % 4 == 0)(s"n$i")))

/** Two ends of a wire: chunks cross, merges agree with the local run. */
class TestRemote extends munit.FunSuite {
  // binds a port (integration-test-gate: out of the default gate)
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  import RemoteModel.*

  test("chunks cross the wire and the merged fold equals the local run") {
    val xs = (1 to 1000).map(_.toDouble).toList
    val (local, remote) = xs.splitAt(400)
    val agg = Aggregator.variance[Double]

    val server = RemoteModel.loopback()
    val received = Remote.listen[Double](server)

    // "the other node": ships its part in chunks of 64
    val sender = Remote.connect[Double](server.getInetAddress.getHostAddress, server.getLocalPort)
    remote.grouped(64).foreach(g =>
      sender.send(okay.ChunkBuf.of(g)))
    sender.close()

    // this node: fold the remote chunks into a partial accumulator
    var remoteAcc = agg.init
    var c = received.receiveBlocking()
    while c.isDefined do
      remoteAcc = c.get.foldLeft(remoteAcc)(agg.add)
      c = received.receiveBlocking()

    val localAcc = local.foldLeft(agg.init)(agg.add)
    val together = agg.present(agg.merge(localAcc, remoteAcc))
    assert(math.abs(together - agg.run(xs)) / agg.run(xs) < 1e-9,
      s"$together vs ${agg.run(xs)}")
    server.close()
  }

  /** every format and compression one listener takes, records intact */
  private def drain(received: okay.Channel[okay.Chunk[Trade]]): List[Trade] =
    var all = List.empty[Trade]
    var c = received.receiveBlocking()
    while c.isDefined do { all = all ++ c.get; c = received.receiveBlocking() }
    all

  for (fmt, cmp) <- Vector(
      (RemoteFormat.arrow, RemoteCompression.none), (RemoteFormat.arrow, RemoteCompression.Zstd.zstd),
      (RemoteFormat.arrow, RemoteCompression.Lz4.lz4), (RemoteFormat.Cbor.cbor, RemoteCompression.none),
      (RemoteFormat.Cbor.cbor, RemoteCompression.Zstd.zstd), (RemoteFormat.Json.json, RemoteCompression.none))
  do
    test(s"records cross as ${fmt.name}${cmp.codec.fold("")(c => "+" + c.name)}, to a listener that was told nothing") {
      val server = RemoteModel.loopback()
      val received = Remote.listen[Trade](server)
      val sender = Remote.connect[Trade](server.getInetAddress.getHostAddress, server.getLocalPort)(using summon, fmt, cmp)
      val sent = trades(500)
      sent.grouped(128).foreach(g => sender.send(okay.ChunkBuf.of(g)))
      sender.close()
      assertEquals(drain(received), sent)
      server.close()
    }

  test("a damaged frame is dropped; the stream lives; close drains") {
    val server = RemoteModel.loopback()
    val received = Remote.listen[Long](server)
    val sock = java.net.Socket(server.getInetAddress.getHostAddress, server.getLocalPort)
    val out = java.io.DataOutputStream(sock.getOutputStream)
    def frame(fmt: Char, payload: Array[Byte]): Unit =
      out.writeInt(payload.length + 2); out.writeByte(fmt); out.writeByte(0); out.write(payload)
    frame('J', "[1,2,3]".getBytes)
    frame('A', "not arrow at all".getBytes)
    frame('?', Array[Byte](1, 2))
    frame('J', "[4,5]".getBytes)
    out.flush()
    sock.close()
    var all = List.empty[Long]
    var c = received.receiveBlocking()
    while c.isDefined do { all = all ++ c.get; c = received.receiveBlocking() }
    assertEquals(all, List(1L, 2L, 3L, 4L, 5L))
    server.close()
  }
}
