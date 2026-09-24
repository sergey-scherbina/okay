package okay.pool

import okay.*
import okay.cluster.{Checkpoint, Jobs, Lease}
import okay.codec.{Codecs, Json}
import okay.http.{Http, Method, Request, Response, Route, Router}
import okay.http.syntax.*
import okay.resilience.Discovery
import java.nio.charset.StandardCharsets.UTF_8

/** the HTTP door (specs/cluster-pool.md, stage 1): the four routes
 * plus the two probes okay-ops's shape already established, kept
 * here rather than reusing `okay.ops.Ops.routes` because that one is
 * built over an okay-persist `Store` — a concrete kind of journal
 * this module's own `store` seam is deliberately more general than */
object Routes:

  val healthz: Route[EmptyTuple] = Route / "healthz"
  val readyz: Route[EmptyTuple] = Route / "readyz"
  val jobsPath: Route[EmptyTuple] = Route / "pool" / "jobs"
  val submitPath: Route[String *: EmptyTuple] = Route / "pool" / "jobs" / "name".as[String]
  val runsPath: Route[String *: EmptyTuple] = Route / "pool" / "runs" / "id".as[String]
  val peersPath: Route[EmptyTuple] = Route / "pool" / "peers"

  private def text(status: Int, body: String): Response =
    Response(status, Seq("content-type" -> "text/plain; charset=utf-8"), Http.one(body.getBytes(UTF_8)))
  private def json(status: Int, body: String): Response =
    Response(status, Seq("content-type" -> "application/json"), Http.one(body.getBytes(UTF_8)))
  private def err(status: Int, why: String): Response = json(status, Codecs.writeJson(ErrorBody(why)))

  /**
   * `ready` is a callback rather than a fixed value so a test can flip
   * it — `Pool.run` sets it once, after the registrars have loaded and
   * BEFORE the port ever accepts a connection, which is why the FALSE
   * state is real but not reachable through a live process: nothing
   * can ask before the socket exists. A unit test constructs the
   * router directly to exercise both answers.
   */
  def router(conf: PoolConf, discovery: Discovery, store: String => (Checkpoint, Lease), ready: () => Boolean)
            (using Scheduler): Router =
    Router.empty
      .on(Method.Get, healthz) { _ => pure(text(200, "live=true")) }
      .on(Method.Get, readyz) { _ =>
        pure(if ready() then text(200, "ready=true") else text(503, "ready=false"))
      }
      .on(Method.Get, jobsPath) { _ =>
        pure(json(200, Codecs.writeJson(Jobs.names)))
      }
      .on(Method.Get, peersPath) { _ =>
        Pool.resolve(conf, discovery).map(es => json(200, Codecs.writeJson(es)))
      }
      .at(Method.Post, submitPath) { (name, req) =>
        Json.parse(new String(req.body.bytes, UTF_8)) match
          case o: Json.JObj =>
            val fs = o.fs.toMap
            val params = fs.getOrElse("params", Json.JObj(Vector.empty))
            val parts = fs.get("parts").collect { case Json.JNum(n) => n.toInt }.getOrElse(0)
            val take = fs.get("take").collect { case Json.JNum(n) => n.toInt }.getOrElse(0)
            val journal = fs.get("journal").collect { case Json.JStr(s) => s }.getOrElse("")
            Pool.submit(name, params, parts, take, journal, conf, discovery, store).map {
              case Right(s) => json(202, Codecs.writeJson(s))
              case Left((status, why)) => err(status, why)
            }
          case _ => pure(err(400, "a submission body must be a JSON object"))
      }
      .at(Method.Get, runsPath) { (id, _) =>
        Pool.statusOf(id, conf, discovery, store).map {
          case None => err(404, s"no run named '$id'")
          case Some(s) => json(200, Codecs.writeJson(s))
        }
      }

  def routes(conf: PoolConf, discovery: Discovery, store: String => (Checkpoint, Lease), ready: () => Boolean)
            (using Scheduler): PartialFunction[Request, Response ! Async] =
    router(conf, discovery, store, ready).routes
