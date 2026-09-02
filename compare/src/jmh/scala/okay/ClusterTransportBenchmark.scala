package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import okay.cluster.Remote
import okay.codec.{Json, Schema}
import okay.http.Nio

/**
 * The measurement specs/http-backends.md deferred to, before anything
 * is changed: does NIO buy anything over a Loom-parked blocking socket
 * for okay-cluster's transport?
 *
 * Three lanes, and the third is the control that decides whether the
 * first two even matter. `remote` is the shipped transport — blocking
 * socket, BufferedReader, one JSON frame per line, a virtual thread
 * parked on the wire. `nio` is the same payload and the same framing
 * over `Nio.Conn` — since the nio-serve-stall fix (specs/nio.md) also
 * blocking channels on virtual threads, so the lane now measures the
 * same mechanics through the Conn API. `codecOnly` is the JSON encode+decode with no wire at
 * all: if it dominates, the transport choice is noise and the honest
 * change to cluster is the CBOR dialect its spec already plans, not a
 * socket API.
 *
 * One round = 100 chunks of 64 Longs over localhost, received back
 * into a fold. Localhost deliberately: it maximizes the share of the
 * API overhead being compared, which is the thing in question — a real
 * network would only shrink the difference.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 8, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ClusterTransportBenchmark {

  given Schema[List[Long]] = Schema.derived

  val chunks: Vector[Chunk[Long]] =
    Vector.tabulate(100)(i => ChunkBuf.of((0 until 64).map(j => (i * 64 + j).toLong)))

  val expected: Long = chunks.map(_.sum).sum

  /** the shipped transport: blocking socket, virtual thread parked */
  @Benchmark
  def remote: Long =
    val server = java.net.ServerSocket(0)
    val ch = Remote.listen[Long](server)
    val sender = Remote.connect[Long]("127.0.0.1", server.getLocalPort)
    chunks.foreach(sender.send)
    sender.close()
    var sum = 0L
    var c = ch.receiveBlocking()
    while c.isDefined do
      sum += c.get.sum
      c = ch.receiveBlocking()
    server.close()
    assert(sum == expected)
    sum

  /** the same payload and framing over Nio.Conn */
  @Benchmark
  def nio: Long =
    Resource.run[Long, Pure](
      Nio.listen(0) { conn =>
        // the serving end: send every frame, close
        def go(i: Int): Unit ! Async =
          if i >= chunks.length then conn.close()
          else conn.send(Json.write(chunks(i).toList) + "\n").flatMap(_ => go(i + 1))
        go(0)
      }.map { server =>
        Async.run[Long, Pure](
          Nio.connect("127.0.0.1", Nio.port(server)).flatMap { c =>
            given Fold[String, Long] = Fold.long[String](0L) { (s, line) =>
              Json.read[List[Long]](line).fold(_ => s, xs => s + xs.sum)
            }
            Writer.fold[String, Long, Unit, Async](
              through[Chunk[Byte], String, Async, Unit, Unit](c.bytes)(
                !.widen[Unit, Take % Chunk[Byte] + Writer % String, Async](
                  okay.http.Http.framing))).map(_._1)
          }).runWith
      }).runWith.ensuring(_ == expected)

  /**
   * The attribution lane: a BLOCKING socket with byte-level plumbing —
   * same framing as the nio lane, but a virtual thread parked on an
   * InputStream instead of a CompletionHandler.
   *
   * This is the lane that decides what the remote/nio difference IS.
   * `remote` reads through BufferedReader and writes through a
   * PrintWriter with autoflush; if this lane matches `nio`, the win is
   * the byte plumbing and Remote can be fixed with no new dependency;
   * if it matches `remote`, the win is NIO itself.
   */
  @Benchmark
  def blockingBytes: Long =
    val server = java.net.ServerSocket(0)
    val done = new java.util.concurrent.CompletableFuture[Unit]
    Thread.startVirtualThread { () =>
      try
        val sock = server.accept()
        val out = java.io.BufferedOutputStream(sock.getOutputStream, 1 << 16)
        chunks.foreach { c =>
          out.write((Json.write(c.toList) + "\n").getBytes("UTF-8"))
        }
        out.flush()
        sock.close()
        done.complete(())
      catch case e: Throwable => done.completeExceptionally(e)
      ()
    }
    val sock = java.net.Socket("127.0.0.1", server.getLocalPort)
    val in = sock.getInputStream
    given Fold[String, Long] = Fold.long[String](0L) { (s, line) =>
      Json.read[List[Long]](line).fold(_ => s, xs => s + xs.sum)
    }
    // a blocking byte source: the same shape Nio.Conn.bytes has, with
    // Async.Run instead of Async.Await
    type F = Writer % Chunk[Byte] + Async
    def go: Source[Chunk[Byte]] =
      effect[F, Chunk[Byte] | Null](Async.Run { () =>
        val buf = new Array[Byte](8192)
        val n = in.read(buf)
        if n < 0 then null
        else scala.collection.immutable.ArraySeq.unsafeWrapArray(
          java.util.Arrays.copyOf(buf, n))
      }).flatMap {
        case null => pure(())
        case c: Chunk[Byte] @unchecked =>
          effect[F, Unit](Writer(c)).flatMap(_ => go)
      }
    val sum = Async.run[Long, Pure](
      Writer.fold[String, Long, Unit, Async](
        through[Chunk[Byte], String, Async, Unit, Unit](go)(
          !.widen[Unit, Take % Chunk[Byte] + Writer % String, Async](
            okay.http.Http.framing))).map(_._1)).runWith
    done.get(30, TimeUnit.SECONDS)
    sock.close(); server.close()
    assert(sum == expected)
    sum

  /**
   * The DECONFOUNDING lane. `blockingBytes` differed from `remote` in
   * two ways at once — byte plumbing AND a single flush at the end —
   * and the rewrite that followed only the first bought nothing. This
   * lane is bytes WITH a flush per line, so the remaining difference
   * from `blockingBytes` is the flush policy alone, and the remaining
   * difference from `remote` is nothing.
   */
  @Benchmark
  def blockingBytesFlushed: Long =
    val server = java.net.ServerSocket(0)
    val done = new java.util.concurrent.CompletableFuture[Unit]
    Thread.startVirtualThread { () =>
      try
        val sock = server.accept()
        val out = java.io.BufferedOutputStream(sock.getOutputStream, 1 << 16)
        chunks.foreach { c =>
          out.write((Json.write(c.toList) + "\n").getBytes("UTF-8"))
          out.flush()
        }
        sock.close()
        done.complete(())
      catch case e: Throwable => done.completeExceptionally(e)
      ()
    }
    val sock = java.net.Socket("127.0.0.1", server.getLocalPort)
    val in = sock.getInputStream
    given Fold[String, Long] = Fold.long[String](0L) { (s, line) =>
      Json.read[List[Long]](line).fold(_ => s, xs => s + xs.sum)
    }
    type F = Writer % Chunk[Byte] + Async
    def go: Source[Chunk[Byte]] =
      effect[F, Chunk[Byte] | Null](Async.Run { () =>
        val buf = new Array[Byte](8192)
        val n = in.read(buf)
        if n < 0 then null
        else scala.collection.immutable.ArraySeq.unsafeWrapArray(
          java.util.Arrays.copyOf(buf, n))
      }).flatMap {
        case null => pure(())
        case c: Chunk[Byte] @unchecked =>
          effect[F, Unit](Writer(c)).flatMap(_ => go)
      }
    val sum = Async.run[Long, Pure](
      Writer.fold[String, Long, Unit, Async](
        through[Chunk[Byte], String, Async, Unit, Unit](go)(
          !.widen[Unit, Take % Chunk[Byte] + Writer % String, Async](
            okay.http.Http.framing))).map(_._1)).runWith
    done.get(30, TimeUnit.SECONDS)
    sock.close(); server.close()
    assert(sum == expected)
    sum

  /** the control: encode+decode with no wire — the codec's share */
  @Benchmark
  def codecOnly: Long =
    var sum = 0L
    chunks.foreach { c =>
      Json.read[List[Long]](Json.write(c.toList)) match
        case Right(xs) => sum += xs.sum
        case Left(_) => ()
    }
    assert(sum == expected)
    sum
}
