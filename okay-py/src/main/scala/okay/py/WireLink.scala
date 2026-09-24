package okay.py

import java.io.{BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter}
import java.net.{InetSocketAddress, Socket}

/**
 * Where the okay wire runs (polyglot-one-wire, specs/polyglot-one-wire.md):
 * the far side's handshake line, then one request line out and one answer
 * line in. The foreign engine (`PySubprocess`: typed calls, callbacks,
 * programs as data, multi-shot, `Durable`) runs over ANY link unchanged —
 * a child process's pipes, a socket to another machine, a function call
 * into native code or a WebAssembly module in this process.
 *
 * `None` from either is the far side gone: the in-flight call throws, and a
 * supervisor decides — the engine's fault model, whatever the transport.
 */
trait WireLink:
  /** the far side's first line */
  def hello(): Option[String]
  /** one request out, its answer in */
  def roundTrip(line: String): Option[String]
  def close(): Unit

object WireLink:

  /** a line-oriented stream pair: what pipes and sockets both are */
  private abstract class Lines(out: BufferedWriter, in: BufferedReader) extends WireLink:
    def hello(): Option[String] = Option(in.readLine())
    def roundTrip(line: String): Option[String] =
      out.write(line); out.write("\n"); out.flush()
      Option(in.readLine())

  /** a child process's stdin and stdout */
  def pipes(proc: Process): WireLink =
    val out = BufferedWriter(OutputStreamWriter(proc.getOutputStream, "UTF-8"))
    val in = BufferedReader(InputStreamReader(proc.getInputStream, "UTF-8"))
    new Lines(out, in):
      def close(): Unit =
        try { out.close(); in.close() } catch case _: Exception => ()
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
    val out = BufferedWriter(OutputStreamWriter(s.getOutputStream, "UTF-8"))
    val in = BufferedReader(InputStreamReader(s.getInputStream, "UTF-8"))
    new Lines(out, in):
      def close(): Unit = try s.close() catch case _: Exception => ()
