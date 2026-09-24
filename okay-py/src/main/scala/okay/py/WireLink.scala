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
  def tcp(host: String, port: Int, connectTimeoutMs: Int = 10000,
          /** TLS or plain (wire-tls) */
          security: okay.codec.WireSecurity = okay.codec.WireSecurity.Plain,
          /** how long the server may take to say hello: a TLS server waits
           * for the client to speak first, so a plain client would wait for
           * ever without a limit */
          helloMillis: Int = 10000): WireLink =
    val raw = Socket()
    raw.connect(InetSocketAddress(host, port), connectTimeoutMs)
    raw.setTcpNoDelay(true)
    val s: Socket = security match
      case okay.codec.WireSecurity.Plain => raw
      case t: okay.codec.WireSecurity.Tls =>
        val ctx = try t.context() catch case e: Exception => { raw.close(); throw e }
        ctx.getSocketFactory.createSocket(raw, host, port, true) match
          case ssl: javax.net.ssl.SSLSocket =>
            val p = ssl.getSSLParameters
            p.setEndpointIdentificationAlgorithm("HTTPS")   // the NAME is checked, not only the chain
            ssl.setSSLParameters(p)
            ssl.setSoTimeout(helloMillis)
            try ssl.startHandshake()
            catch case e: java.io.IOException =>
              raw.close()
              throw IllegalStateException(
                s"the worker at $host:$port did not complete a TLS handshake (${e.getMessage}): does it serve TLS " +
                  s"(OKAY_TLS_CERT, OKAY_TLS_KEY), and does ${t.trust.source} trust its certificate for $host?")
            ssl
          case other =>
            raw.close()
            throw IllegalStateException(s"the TLS socket factory made a ${other.getClass.getName}, not an SSLSocket")
    new Streams(s.getOutputStream, s.getInputStream):
      override def hello(): Option[String] =
        s.setSoTimeout(helloMillis)
        try super.hello()
        catch case _: java.net.SocketTimeoutException =>
          close()
          val hint = security match
            case okay.codec.WireSecurity.Plain =>
              " — a TLS server waits for the client to speak first: does it serve TLS? (this host's given WireSecurity is plain)"
            case _ => ""
          throw IllegalStateException(s"the worker at $host:$port said nothing for ${helloMillis}ms$hint")
        finally if !s.isClosed then s.setSoTimeout(0)
      def close(): Unit = try s.close() catch case _: Exception => ()
