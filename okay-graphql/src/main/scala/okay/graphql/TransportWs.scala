package okay.graphql

import okay.{Async, AsyncCont}
import okay.codec.Json
import okay.codec.Json.*
import okay.one.Cont.pure
import scala.collection.mutable
import scala.util.{Failure, Success}

/** GRAPHQL OVER A WEBSOCKET: the `graphql-transport-ws` protocol (the graphql-ws library's), its server side as a
 * connection that any socket drives — a text frame in is `receive`, a frame out is `send`, the socket closed with a
 * code is `close`. No socket here: Jetty, Netty, a test, all the same (`TestTransportWs`) */
object TransportWs:
  /** the subprotocol a client asks for (`Sec-WebSocket-Protocol`) */
  val Subprotocol = "graphql-transport-ws"

  /** a close of the socket: the protocol's code and reason */
  final case class Close(code: Int, reason: String)

  /** ONE CONNECTION'S STATE: initialised or not, its running operations by id. `accept` says whether a
   * `connection_init`'s payload is let in (authentication); a subscription's events are sent as they come, from
   * whatever thread its source answers on — `send` is called under the connection's lock, so frames keep order */
  final class Connection(schema: Schema, send: String => Unit, close: Close => Unit, limits: Limits = Limits(),
      accept: Json => Boolean = _ => true)(using parser: GraphQLParser):
    private var initialised = false
    private var acknowledged = false
    private var closed = false
    private val active = mutable.Map.empty[String, Option[Async.Running[Unit]]]

    /** a text frame from the client */
    def receive(text: String): Unit = synchronized {
      if !closed then Json.parse(text) match
        case JObj(fs) =>
          val m = fs.toMap
          m.get("type") match
            case Some(JStr("connection_init")) =>
              if initialised then shut(Close(4429, "Too many initialisation requests"))
              else
                initialised = true
                if accept(m.getOrElse("payload", JNull)) then
                  acknowledged = true
                  out("connection_ack")
                else shut(Close(4403, "Forbidden"))
            case Some(JStr("ping")) => out("pong")
            case Some(JStr("pong")) => ()
            case Some(JStr("subscribe")) => (m.get("id"), m.get("payload")) match
              case (Some(JStr(id)), Some(JObj(p))) => subscribe(id, p.toMap)
              case _ => shut(Close(4400, "Invalid subscribe message"))
            case Some(JStr("complete")) => m.get("id") match
              case Some(JStr(id)) => active.remove(id).flatten.foreach(_.cancel())
              case _ => shut(Close(4400, "Invalid complete message"))
            case other => shut(Close(4400, s"Invalid message type ${other.fold("(none)")(Json.print)}"))
        case _ => shut(Close(4400, "Invalid message: not a JSON object"))
    }

    /** the socket closed: every running operation cancelled */
    def closedBySocket(): Unit = synchronized {
      closed = true
      active.values.flatten.foreach(_.cancel())
      active.clear()
    }

    def running: Set[String] = synchronized(active.keySet.toSet)

    private def subscribe(id: String, p: Map[String, Json]): Unit =
      if !acknowledged then shut(Close(4401, "Unauthorized"))
      else if active.contains(id) then shut(Close(4409, s"Subscriber for $id already exists"))
      else p.get("query") match
        case Some(JStr(query)) =>
          val vars = p.getOrElse("variables", JObj(Vector.empty)) match
            case JNull => JObj(Vector.empty)
            case v => v
          val op = p.get("operationName").collect { case JStr(n) => n }
          Executor.prepare(schema, query, op, limits) match
            case Left(refused) => error(id, refused)
            case Right(prepared) if prepared.operation.kind != OperationKind.Subscription =>
              active(id) = None
              AsyncCont.runAsync(prepared.executeAsync(vars)).onComplete { result =>
                synchronized {
                  if active.remove(id).isDefined && !closed then result match
                    case Success(j) => next(id, j); out("complete", id)
                    case Failure(e) => error(id, Executor.errorsJson(List(GraphQLError(e.toString))))
                }
              }(using scala.concurrent.ExecutionContext.parasitic)
            case Right(prepared) => prepared.subscribe(vars) match
              case Left(refused) => error(id, refused)
              case Right(events) =>
                active(id) = None
                val running = AsyncCont.runAsyncCancellable(events.runForeach { j =>
                  synchronized(if active.contains(id) && !closed then next(id, j))
                  pure(())
                })
                running.future.onComplete { result =>
                  synchronized {
                    if active.remove(id).isDefined && !closed then result match
                      case Success(_) => out("complete", id)
                      case Failure(e) => error(id, Executor.errorsJson(List(GraphQLError(e.toString))))
                  }
                }(using scala.concurrent.ExecutionContext.parasitic)
                if active.contains(id) then active(id) = Some(running)
        case _ => shut(Close(4400, "Invalid subscribe message: no query"))

    private def out(tpe: String, id: String = ""): Unit =
      send(Json.print(JObj(Vector("type" -> JStr(tpe)) ++ (if id.isEmpty then Vector.empty else Vector("id" -> JStr(id))))))

    private def next(id: String, payload: Json): Unit =
      send(Json.print(JObj(Vector("id" -> JStr(id), "type" -> JStr("next"), "payload" -> payload))))

    /** a refused operation: its errors, the `errors` of a response */
    private def error(id: String, response: Json): Unit =
      val errors = response match
        case JObj(fs) => fs.collectFirst { case ("errors", e) => e }.getOrElse(JArr(Vector.empty))
        case _ => JArr(Vector.empty)
      send(Json.print(JObj(Vector("id" -> JStr(id), "type" -> JStr("error"), "payload" -> errors))))

    private def shut(c: Close): Unit =
      closed = true
      active.values.flatten.foreach(_.cancel())
      active.clear()
      close(c)
