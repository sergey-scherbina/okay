package okay.mcp

import okay.*
import okay.http.{Http, Nio}

/**
 * A raw TCP connection AS an MCP link: newline-delimited lines, with no
 * HTTP anywhere.
 *
 * MCP's own framing is a line per message, and `Http.framing` already
 * turns a byte source into lines — so this is two lines of glue, and it
 * means an MCP server can be reached over a bare TCP socket as well as
 * over pipes and over a WebSocket.
 *
 * Was `okay.http.Nio.link` until http-mcp-agent-edge (2026-09-25).
 */
object NioLink:
  def apply(c: Nio.Conn): Link = new Link:
    def send(line: String): Unit ! Async = c.send(line + "\n")

    def lines: Source[String] =
      through[Chunk[Byte], String, Async, Unit, Unit](c.bytes)(
        !.widen[Unit, Take % Chunk[Byte] + Writer % String, Async](Http.framing))
