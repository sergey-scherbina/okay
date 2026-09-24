package okay.py

import java.io.{BufferedInputStream, BufferedOutputStream, InputStream, OutputStream}
import okay.codec.WireFrames
import java.net.{InetSocketAddress, Socket}

/**
 * Where the okay wire runs (polyglot-one-wire, specs/polyglot-one-wire.md):
 * the far side's handshake line, then messages — JSON LINES by default, or,
 * once a format or a compression other than the defaults has been
 * configured (stage 5a), FRAMES: a 4-byte big-endian length and that many
 * bytes. The foreign engine (`ForeignWorker`: typed calls, callbacks,
 * programs as data, multi-shot, `Durable`) runs over ANY link unchanged — a
 * child process's pipes, a socket to another machine, a function call into
 * native code or a WebAssembly module in this process.
 *
 * `None` from any of them is the far side gone: the in-flight call throws,
 * and a supervisor decides — the engine's fault model, whatever the transport.
 */
trait WireLink:
  /** the far side's first line */
  def hello(): Option[String]
  /** one request line out, its answer line in */
  def roundTrip(line: String): Option[String]
  /** one message of bytes out, its answer in: a frame on a stream, the bytes
   * alone in-process, where a call already delimits a message */
  def exchange(message: Array[Byte]): Option[Array[Byte]]
  def close(): Unit
  /** whether a message stays in this process (FFM, wasm): the default
   * compression does not compress there, where it would only cost */
  def inProcess: Boolean = false

object WireLink:

  /**
   * A byte stream pair: what pipes and sockets both are, framed by
   * `okay.codec.WireFrames`.
   */
  private abstract class Streams(out: OutputStream, in: InputStream) extends WireLink:
    private val input = BufferedInputStream(in)
    private val output = BufferedOutputStream(out)

    def hello(): Option[String] = WireFrames.readLine(input)

    def roundTrip(line: String): Option[String] =
      WireFrames.writeLine(output, line)
      WireFrames.readLine(input)

    def exchange(message: Array[Byte]): Option[Array[Byte]] =
      WireFrames.writeFrame(output, message)
      WireFrames.readFrame(input)

  /** a child process's stdin and stdout */
  def pipes(proc: Process): WireLink =
    new Streams(proc.getOutputStream, proc.getInputStream):
      def close(): Unit =
        try { proc.getOutputStream.close(); proc.getInputStream.close() } catch case _: Exception => ()
        proc.destroy()

  /**
   * A TCP connection to a worker serving the okay wire (`okay::serve_tcp`
   * in Rust, `okay.ServeTCP` in Go): another process, or another machine.
   * PLAIN TCP, unauthenticated — for a trusted network, or behind TLS or
   * SSH; the spec says so rather than implying otherwise.
   */
  def tcp(host: String, port: Int, connectTimeoutMs: Int = 10000): WireLink =
    val s = Socket()
    s.connect(InetSocketAddress(host, port), connectTimeoutMs)
    s.setTcpNoDelay(true)
    new Streams(s.getOutputStream, s.getInputStream):
      def close(): Unit = try s.close() catch case _: Exception => ()
