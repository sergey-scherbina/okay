package okay.graphql.http

import okay.{Channel, Source, Stage, drained}
import okay.freer.*
import okay.graphql.{GraphQLParser, Limits, Schema, TransportWs}
import okay.http.{Frame, Request}

/** GRAPHQL SUBSCRIPTIONS OVER A WEBSOCKET a server accepts: `TransportWs.Connection` (graphql-transport-ws) per
 * session, as okay-http's server seam has it — a session a `Stage` that awaits the client's frames and the
 * server's pushes in arrival order and tells; the connection's frames out are pushes. For Jetty:
 * {{{
 * val ws = GraphQLWs.route(schema, "/graphql")
 * Jetty.serve(0)(routes)(ws.sessions, ws.pushes, subprotocol = ws.subprotocol)
 * }}} */
object GraphQLWs:
  /** a WebSocket path's sessions, their pushes, and the subprotocol they speak */
  final class Route(val sessions: PartialFunction[Request, Stage[Frame, Frame, Unit]],
      val pushes: PartialFunction[Request, Source[Frame]], val subprotocol: Request => Option[String])

  private val Out = '\u0001'
  private val Shut = '\u0002'

  /** every client connecting on `path`, a connection of its own; `accept` judges a `connection_init`'s payload */
  def route(schema: Schema, path: String, limits: Limits = Limits(),
      accept: okay.codec.Json => Boolean = _ => true)(using GraphQLParser): Route =
    val opening = java.util.Collections.synchronizedMap(java.util.IdentityHashMap[Request, Session]())
    def at(r: Request): Boolean = r.url.takeWhile(_ != '?') == path
    Route(
      { case r if at(r) =>
        val s = Session(schema, limits, accept)
        opening.put(r, s)
        s.stage
      },
      { case r if at(r) && opening.containsKey(r) => opening.remove(r).pushes },
      r => Option.when(at(r))(TransportWs.Subprotocol))

  /** one session: the connection's frames out marked as pushes, the client's into the connection */
  final class Session(schema: Schema, limits: Limits, accept: okay.codec.Json => Boolean)(using GraphQLParser):
    private val out = Channel[Frame](Int.MaxValue)
    val connection: TransportWs.Connection = TransportWs.Connection(schema,
      text => out.offer(Frame.Text(s"$Out$text")): Unit,
      c => out.offer(Frame.Text(s"$Shut${c.code}:${c.reason}")): Unit,
      limits, accept)

    def pushes: Source[Frame] = out.drained

    /** the session: the connection's frames to the client, the client's to the connection; a close its code */
    def stage: Stage[Frame, Frame, Unit] =
      Stage.await[Frame, Frame].flatMap {
        case Some(Frame.Text(t)) if t.nonEmpty && t.charAt(0) == Out =>
          Stage.tell[Frame, Frame](Frame.Text(t.substring(1))).flatMap(_ => stage)
        case Some(Frame.Text(t)) if t.nonEmpty && t.charAt(0) == Shut =>
          val (code, reason) = t.substring(1).span(_ != ':')
          connection.closedBySocket()
          Stage.tell[Frame, Frame](Frame.Close(code.toIntOption.getOrElse(4400), reason.drop(1)))
        case Some(Frame.Text(t)) =>
          connection.receive(t)
          stage
        case Some(Frame.Close(_, _)) | None =>
          connection.closedBySocket()
          out.close()
          pure(())
        case Some(_) => stage
      }
