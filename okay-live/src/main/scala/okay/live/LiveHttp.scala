package okay.live

import java.nio.charset.StandardCharsets.UTF_8
import okay.*
import okay.codec.Json
import okay.http.{Http, Method, Query, Request, Response, Route, Router}

/**
 * A `Watched` document over HTTP (typescript-types T11): what a browser —
 * or any fetch — subscribes to, with no protocol of its own.
 *
 *  - `GET <prefix>/watch?key=K` is a server-sent-event stream: the focused
 *    part NOW, then each time it changes, one `data: <json>` event each —
 *    so a subscriber never waits for a first change to have a value. A key
 *    the schema has no place for is one `event: refused` naming why, and
 *    the end of the stream: SSE has no status after the first byte, so the
 *    refusal travels as an event;
 *  - `POST <prefix>/set` with `{"key": K, "value": V}` is `Watched.set`:
 *    204; 400 with the reason for a key the schema has no place for; 409
 *    when the key's place is ABSENT now (an index past the end, a field of
 *    another case) — the lens would change nothing there, and a client
 *    told 204 would believe it had written.
 *
 * The keys are `JsonOptic.path`'s, and `Stubs.typescriptPaths` declares
 * every one of them for TypeScript, with the type of what it is told.
 */
object LiveHttp:

  private def resource(name: String): String =
    val in = getClass.getResourceAsStream(s"/okay/live/$name")
    try String(in.readAllBytes(), UTF_8) finally in.close()

  /** the TypeScript client, `live.ts`: `live<Paths>(options)` with a typed
   * `watch` and `set`, and the `<okay-live>` custom element */
  def client: String = resource("live.ts")

  /** `live-react.ts`: `useWatch(client, key)`, for a React frontend */
  def react: String = resource("live-react.ts")

  private def event(text: String): Source[Chunk[Byte]] = Http.one(text.getBytes(UTF_8))

  private def data(j: Json): Source[Chunk[Byte]] = event(s"data: ${Json.print(j)}\n\n")

  /** the channel's values, as events, until it closes */
  private def pushes(c: Channel[Json]): Source[Chunk[Byte]] =
    effect[Writer % Chunk[Byte] + Async, Option[Json]](Async.Await[Option[Json]] { k =>
      c.receiveAsync(end => k(end))
      () => ()
    }).flatMap {
      case Some(j) => data(j).flatMap(_ => pushes(c))
      case None => pure(())
    }

  def routes[A](w: Watched[A], prefix: String = "live"): Router =
    Router.empty
      .events[String *: EmptyTuple](Method.Get, Route / prefix / "watch" :? Query[String]("key")) { key =>
        (w.focus(key), w.subscribe(key)) match
          case (Right(now), Right(c)) => pure(data(now.getOrElse(Json.JNull)).flatMap(_ => pushes(c)))
          case (Left(why), _) => pure(event(s"event: refused\ndata: ${Json.print(Json.JStr(why))}\n\n"))
          case (_, Left(why)) => pure(event(s"event: refused\ndata: ${Json.print(Json.JStr(why))}\n\n"))
      }
      .at[EmptyTuple](Method.Post, Route / prefix / "set") { (_, r: Request) =>
        val answer = Json.parse(String(r.body.bytes, UTF_8)) match
          case Json.JObj(fields) =>
            val m = fields.toMap
            (m.get("key"), m.get("value")) match
              case (Some(Json.JStr(k)), Some(v)) => w.focus(k) match
                case Right(Some(_)) => w.set(k, v).left.map(400 -> _)
                case Right(None) => Left(409 -> s"`$k` is absent in the document now: nothing is there to set")
                case Left(why) => Left(400 -> why)
              case _ => Left(400 -> "the body is {\"key\": string, \"value\": any}")
          case _ => Left(400 -> "the body is {\"key\": string, \"value\": any}")
        pure(answer match
          case Right(()) => Response(204, Nil, Http.one(Array.empty[Byte]))
          case Left((status, why)) => Response(status, Seq("content-type" -> "text/plain"), Http.one(why.getBytes(UTF_8))))
      }
