package okay.mcp

import okay.*
import okay.http.{Frame, Socket, Ws}

/**
 * A WebSocket AS an MCP link — which is the whole reason the shapes
 * were kept the same.
 *
 * MCP has two standard transports: stdio, which okay-mcp has, and
 * HTTP+SSE, which it did not. A `Link` is `send(line)` plus
 * `lines: Source[String]`, and a WebSocket is exactly that with frames
 * around it, so `Mcp.run(WsLink(socket), serving)` is the same server
 * over a different wire, with no protocol code changed.
 *
 * Was `okay.http.Ws.link` until http-mcp-agent-edge (2026-09-25): in
 * okay-mcp-http so okay-http does not depend on okay-mcp.
 */
object WsLink:
  def apply(s: Socket): Link = new Link:
    def send(line: String): Unit ! Async = s.send(Frame.Text(line))

    def lines: Source[String] =
      through[Frame, String, Async, Unit, Unit](s.frames)(
        !.widen[Unit, Take % Frame + Writer % String, Async](Ws.texts))
