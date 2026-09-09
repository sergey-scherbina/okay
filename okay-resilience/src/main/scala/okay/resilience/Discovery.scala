package okay.resilience

import okay.*
import okay.codec.Schema
import okay.http.{Http, Request, Response}

/** where a service instance answers */
final case class Endpoint(host: String, port: Int) derives Schema:
  def authority: String = s"$host:$port"

/**
 * A name to the endpoints behind it (specs/discovery.md). Three
 * sources cover where a service actually runs: a static table, the
 * environment (Kubernetes writes `<NAME>_SERVICE_HOST`/`_PORT` into
 * every pod), and DNS (JVM, `Discovery.dns`). A registry of our own
 * is not here.
 */
trait Discovery:
  def resolve(service: String): Vector[Endpoint] ! Async

object Discovery:

  def static(table: Map[String, Vector[Endpoint]]): Discovery = new Discovery:
    def resolve(service: String): Vector[Endpoint] ! Async = pure(table.getOrElse(service, Vector.empty))

  /** the environment variable a name maps to: upper-cased, `-` → `_` */
  def varName(service: String): String = service.toUpperCase.replace('-', '_').replace('.', '_')

  /** `<NAME>_SERVICE_HOST`/`_PORT` first (Kubernetes), then
    * `OKAY_SERVICE_<NAME>` as "host:port,host:port" */
  def env(lookup: String => Option[String]): Discovery = new Discovery:
    def resolve(service: String): Vector[Endpoint] ! Async = okay.async {
      val n = varName(service)
      val k8s = for
        host <- lookup(s"${n}_SERVICE_HOST").map(_.trim).filter(_.nonEmpty)
        port <- lookup(s"${n}_SERVICE_PORT").flatMap(_.trim.toIntOption)
      yield Vector(Endpoint(host, port))
      k8s.getOrElse(lookup(s"OKAY_SERVICE_$n").map(parse(_)).getOrElse(Vector.empty))
    }

  /** the first source with an answer wins */
  def chain(sources: Discovery*): Discovery = new Discovery:
    def resolve(service: String): Vector[Endpoint] ! Async =
      def go(rest: List[Discovery]): Vector[Endpoint] ! Async = rest match
        case Nil => pure(Vector.empty)
        case d :: more => d.resolve(service).flatMap(es => if es.nonEmpty then pure(es) else go(more))
      go(sources.toList)

  /** answers remembered for `ttlMillis` per name — DNS's shape */
  def cached(inner: Discovery, ttlMillis: Long,
             clock: () => Long = () => System.currentTimeMillis): Discovery = new Discovery:
    private val cell = TRef(Map.empty[String, (Long, Vector[Endpoint])])
    def resolve(service: String): Vector[Endpoint] ! Async =
      val now = clock()
      cell.get.get(service) match
        case Some((at, es)) if now - at < ttlMillis => pure(es)
        case _ => inner.resolve(service).map { es =>
          cell.modify(m => (m.updated(service, (now, es)), ()))
          es
        }

  /** "h1:p1,h2:p2"; a bare host takes the default port; a damaged
    * entry is dropped, not guessed */
  def parse(list: String, defaultPort: Int = 80): Vector[Endpoint] =
    list.split(',').toVector.map(_.trim).filter(_.nonEmpty).flatMap { e =>
      e.lastIndexOf(':') match
        case -1 => Some(Endpoint(e, defaultPort))
        case i =>
          val host = e.substring(0, i)
          e.substring(i + 1).toIntOption.filter(p => p > 0 && p <= 65535 && host.nonEmpty).map(Endpoint(host, _))
    }

/**
 * Client-side balancing as a URL rewrite: `http://orders/v1/x` asks
 * the discovery for `orders`, picks an endpoint round-robin among
 * those not cooling down, and sends `http://host:port/v1/x`. A host
 * the discovery does not know passes through unchanged. A THROWN
 * wire error marks its endpoint down for the cool-down; an answered
 * status does not — the far end spoke, and the breaker decides.
 */
final class Balanced(discovery: Discovery, cooldownMillis: Long = 5_000,
                     clock: () => Long = () => System.currentTimeMillis)
  extends Reporting[Balanced.Stats]:
  import Balanced.*

  val name: String = "balanced"

  private val cell = TRef(St(Map.empty, Map.empty, 0L, 0L, 0L))

  def stats: Stats =
    val s = cell.get
    val now = clock()
    Stats(s.picked, s.failed, s.refused,
      s.downUntil.collect { case (e, until) if until > now => e.authority }.toVector.sorted)

  def http(inner: Http): Http = new Http:
    def send(r: Request): Response ! Async = Url.split(r.url) match
      case None => inner.send(r)
      case Some(u) =>
        discovery.resolve(u.host).flatMap { es =>
          if es.isEmpty then
            if u.explicitPort || u.host.contains('.') || u.host == "localhost" then inner.send(r)   // not a service name
            else pick(u.host, es).flatMap(_ => inner.send(r))   // refuses
          else pick(u.host, es).flatMap { e =>
            Attempt(inner.send(r.copy(url = u.rewrite(e)))).map {
              case Right(res) => res
              case Left(t) => markDown(e); throw t
            }
          }
        }

  /** round-robin over the endpoints not cooling down; all cooling
    * down → the least recently failed; none at all → refused */
  private def pick(service: String, es: Vector[Endpoint]): Endpoint ! Async =
    okay.async {
      val now = clock()
      cell.modify { s =>
        if es.isEmpty then (s.copy(refused = s.refused + 1), None)
        else
          val alive = es.filter(e => s.downUntil.get(e).forall(_ <= now))
          val pool = if alive.nonEmpty then alive else es.sortBy(e => s.downUntil.getOrElse(e, 0L))
          val i = s.cursor.getOrElse(service, 0)
          val chosen = if alive.nonEmpty then pool(i % pool.size) else pool.head
          (s.copy(cursor = s.cursor.updated(service, (i + 1) % Int.MaxValue), picked = s.picked + 1), Some(chosen))
      }
    }.map {
      case Some(e) => e
      case None => throw Refused.NoEndpoint(service)
    }

  private def markDown(e: Endpoint): Unit =
    val until = clock() + cooldownMillis
    cell.modify(s => (s.copy(downUntil = s.downUntil.updated(e, until), failed = s.failed + 1), ()))

object Balanced:
  final case class Stats(picked: Long, failed: Long, refused: Long,
                         down: Vector[String]) derives Schema

  private final case class St(cursor: Map[String, Int], downUntil: Map[Endpoint, Long],
                              picked: Long, failed: Long, refused: Long)

/** the pieces of a URL a rewrite touches */
private[resilience] final case class Url(scheme: String, host: String, port: Option[Int], rest: String):
  def explicitPort: Boolean = port.isDefined
  def rewrite(e: Endpoint): String = s"$scheme://${e.host}:${e.port}$rest"

private[resilience] object Url:
  private val shape = "^(https?)://([^/:?#]+)(?::(\\d+))?(.*)$".r
  def split(url: String): Option[Url] = url match
    case shape(scheme, host, port, rest) =>
      Some(Url(scheme, host, Option(port).flatMap(_.toIntOption), if rest == null then "" else rest))
    case _ => None
