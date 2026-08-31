package okay.llm

import okay.{!, %, +, Async, Writer, effect, pure}

/**
 * The JVM transport: java.net.http, streaming the response line by
 * line, with the virtual thread parking on the wire. The seam itself
 * is platform-free — this is one implementation of it, and the JS
 * side has its own over fetch.
 */
object Transports:
  /** java.net.http, streaming lines; the virtual thread parks on the wire */
  def http(client: java.net.http.HttpClient = java.net.http.HttpClient.newHttpClient())
  : Transport = new Transport:
    def post(url: String, headers: Map[String, String], body: String)
    : Unit ! (Writer % String + Async) =
      type F = Writer % String + Async
      effect[F, java.util.Iterator[String]](Async.Run { () =>
        val b = java.net.http.HttpRequest.newBuilder(java.net.URI.create(url))
          .POST(java.net.http.HttpRequest.BodyPublishers.ofString(body))
        headers.foreach((k, v) => b.header(k, v))
        client.send(b.build(),
          java.net.http.HttpResponse.BodyHandlers.ofLines()).body().iterator()
      }).flatMap { it =>
        def go(): Unit ! F =
          effect[F, Boolean](Async.Run(() => it.hasNext)).flatMap { has =>
            if !has then pure(())
            else effect[F, String](Async.Run(() => it.next()))
              .flatMap(line => effect[F, Unit](Writer(line)).flatMap(_ => go()))
          }
        go()
      }

