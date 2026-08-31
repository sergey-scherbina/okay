package okay.mcp

import okay.*
import okay.given

import java.io.{BufferedReader, InputStream, InputStreamReader, OutputStream, PrintWriter}
import java.nio.charset.StandardCharsets.UTF_8

/**
 * The transport MCP actually uses: newline-delimited JSON over a pair
 * of pipes. Two directions, and each is four lines, because the
 * shapes they have to produce already exist — a stream of lines IS a
 * `Source[String]`, and reading one is an Async operation that parks
 * a virtual thread on the pipe.
 *
 * A server SPAWNED by us (`of(process)`) and this process being the
 * server (`std`) are the same Link with the streams crossed, which is
 * the whole difference between the two ends of MCP at the transport
 * layer.
 */
object Stdio {

  /** a link over any pair of byte streams */
  def of(in: InputStream, out: OutputStream): Link = new Link:
    private val reader = BufferedReader(InputStreamReader(in, UTF_8))
    private val writer = PrintWriter(java.io.OutputStreamWriter(out, UTF_8), true)

    def send(line: String): Unit ! Async =
      async(writer.println(line))

    /** lines as they arrive; the reader parks a virtual thread, and
     * end of stream ends the source — which is what a server exiting
     * looks like from here */
    def lines: Source[String] =
      def go: Source[String] =
        effect[Writer % String + Async, String](Async.Run(() => reader.readLine()))
          .flatMap(l =>
            if l == null then pure(())
            else effect[Writer % String + Async, Unit](Writer(l)).flatMap(_ => go))

      go

  /**
   * A spawned MCP server, over its own pipes: we write to its stdin
   * and read its stdout, which is the client's view. Its stderr is
   * left alone — servers log there, and a log line is not protocol.
   */
  def of(process: Process): Link = of(process.getInputStream, process.getOutputStream)

  /** spawn a server process (its stderr inherited, so its logs show) */
  def spawn(command: Seq[String], env: Map[String, String] = Map.empty): Process =
    val b = ProcessBuilder(command*)
    b.redirectError(ProcessBuilder.Redirect.INHERIT)
    env.foreach((k, v) => { b.environment().put(k, v); () })
    b.start()

  /**
   * THIS process as the server: stdin in, stdout out. Note what a
   * server may then never do — write anything but protocol to stdout;
   * a stray println is a damaged line to the client (which, being
   * total, survives it, but the message is still lost).
   */
  def std: Link = of(System.in, System.out)
}
