package okay.py

import java.io.{BufferedInputStream, BufferedOutputStream, ByteArrayOutputStream, InputStream, OutputStream}
import java.net.{InetSocketAddress, Socket}
import java.nio.charset.StandardCharsets.UTF_8

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

object WireLink:

  /**
   * A byte stream pair: what pipes and sockets both are. Lines are read
   * byte by byte off the raw stream rather than through a character
   * reader, so a switch from lines to frames loses nothing a reader had
   * buffered ahead.
   */
  private abstract class Streams(out: OutputStream, in: InputStream) extends WireLink:
    private val input = BufferedInputStream(in)
    private val output = BufferedOutputStream(out)

    private def readLine(): Option[String] =
      val b = ByteArrayOutputStream()
      var c = input.read()
      while c != -1 && c != '\n' do
        b.write(c)
        c = input.read()
      if c == -1 && b.size == 0 then None else Some(String(b.toByteArray, UTF_8).stripSuffix("\r"))

    def hello(): Option[String] = readLine()

    def roundTrip(line: String): Option[String] =
      output.write(line.getBytes(UTF_8)); output.write('\n'); output.flush()
      readLine()

    def exchange(message: Array[Byte]): Option[Array[Byte]] =
      val n = message.length
      output.write(Array((n >>> 24).toByte, (n >>> 16).toByte, (n >>> 8).toByte, n.toByte))
      output.write(message)
      output.flush()
      val len = input.readNBytes(4)
      if len.length < 4 then None
      else
        val m = ((len(0) & 0xff) << 24) | ((len(1) & 0xff) << 16) | ((len(2) & 0xff) << 8) | (len(3) & 0xff)
        val body = input.readNBytes(m)
        if body.length < m then None else Some(body)

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
