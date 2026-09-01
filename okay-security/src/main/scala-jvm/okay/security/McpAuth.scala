package okay.security

import okay.{!, Async, pure}
import okay.codec.Json
import okay.http.{Http, McpHttp, Request, Response}

/**
 * MCP authorization (specs/security.md stage 1; MCP's own auth spec):
 * the server is an OAuth2 RESOURCE SERVER — it verifies bearers and
 * tells strangers where to learn to authenticate — and the client
 * walks that chain: 401 → resource metadata (RFC 9728) → AS metadata
 * (RFC 8414) → token → bearer on every request.
 *
 * `discover` ANSWERS what it found rather than following it: the
 * Discovered value names the authorization server BEFORE any secret
 * goes to it — that is the trust boundary, held by making the caller
 * look at it.
 */
object McpAuth {

  val WellKnown = "/.well-known/oauth-protected-resource"

  // ---------------------------------------------------------------- server

  /** the RFC 9728 document: who this resource is, and who may issue
   * tokens for it — servable WITHOUT a token, because it is how a
   * stranger learns to stop being one */
  def metadata(resource: String, authorizationServers: Seq[String])
  : PartialFunction[Request, Response ! Async] = {
    case r if r.url.startsWith(WellKnown) =>
      val doc = Json.JObj(Vector(
        "resource" -> Json.JStr(resource),
        "authorization_servers" -> Json.JArr(
          authorizationServers.map(Json.JStr(_)).toVector)))
      pure(Response(200, Seq(("content-type", "application/json")),
        okay.http.Http.one(Json.print(doc).getBytes("UTF-8"))))
  }

  /**
   * The MCP route, protected. Total routes (which is what
   * `McpHttp.route` is) rather than partial ones, and the challenge
   * carries `resource_metadata` — the pointer that makes the 401 the
   * first step of the dance instead of a dead end.
   */
  def protect(verify: String => Verified, metadataUrl: String,
              policy: Policy = Policy.allowAll)
             (route: Request => Response ! Async): Request => Response ! Async =
    guard(verify, metadataUrl, policy)((_, r) => route(r))

  /** the capability form (specs/context-functions.md,
   * ctx-everywhere): the principal AMBIENT in the protected route —
   * closes the route-wrapper family beside Secure.granted and
   * Traced.route; the same ladder through the same private core */
  def granted(verify: String => Verified, metadataUrl: String,
              policy: Policy = Policy.allowAll)
             (route: Principal ?=> Request => Response ! Async): Request => Response ! Async =
    guard(verify, metadataUrl, policy)((p, r) => route(using p)(r))

  /** the one ladder both forms share */
  private def guard(verify: String => Verified, metadataUrl: String, policy: Policy)
                   (k: (Principal, Request) => Response ! Async): Request => Response ! Async =
    r =>
      def challenge(status: Int, error: String): Response ! Async =
        pure(Response(status, Seq(("www-authenticate",
          s"""Bearer resource_metadata="$metadataUrl", error="$error"""")),
          okay.http.Http.one(Array.emptyByteArray)))
      Secure.bearerToken(r) match
        case None => challenge(401, "no token")
        case Some(t) => verify(t) match
          case Verified.No(_) => challenge(401, "invalid_token")
          case Verified.Ok(p) => policy(p, r.method.name, r.url) match
            case Decision.Deny(_) => challenge(403, "insufficient_scope")
            case Decision.Permit => k(p, r)

  // ---------------------------------------------------------------- client

  final case class Discovered(resource: String, authServer: String,
                              authEndpoint: String, tokenEndpoint: String)

  /**
   * From an MCP url to its authorization server's endpoints: probe
   * (expect the 401), read `resource_metadata` off the challenge,
   * fetch the RFC 9728 document, fetch the AS's RFC 8414 metadata.
   * Every missing link is a named Left — a server that is simply OPEN
   * is one of them, and the caller then needs no token at all.
   */
  def discover(http: Http, mcpUrl: String): Either[String, Discovered] ! Async =
    http.send(Request.post(mcpUrl, okay.http.Body.Text("{}"),
      Seq(("content-type", "application/json")))).flatMap { probe =>
      if probe.status != 401 then
        pure(Left(s"the server did not challenge (HTTP ${probe.status}) — it may be open"))
      else probe.header("www-authenticate").flatMap(resourceMetadataUrl) match
        case None => pure(Left("the 401 carried no resource_metadata"))
        case Some(metaUrl) =>
          fetchJson(http, metaUrl).flatMap {
            case Left(e) => pure(Left(s"resource metadata: $e"))
            case Right(doc) =>
              val resource = str(doc, "resource").getOrElse(mcpUrl)
              firstAuthServer(doc) match
                case None => pure(Left("the resource metadata names no authorization server"))
                case Some(as) =>
                  fetchJson(http, asMetadataUrl(as)).map {
                    case Left(e) => Left(s"authorization server metadata: $e")
                    case Right(asDoc) =>
                      (str(asDoc, "authorization_endpoint"), str(asDoc, "token_endpoint")) match
                        case (Some(a), Some(t)) => Right(Discovered(resource, as, a, t))
                        case _ => Left("the AS metadata lacks endpoints")
                  }
          }
    }

  /**
   * The machine-to-machine dance, whole: discover, obtain by client
   * credentials, hand back a bearer-carrying link. The interactive
   * code+PKCE path is stage 0's `authorizationUrl`/`exchange` with
   * the browser the caller owns — a library cannot click consent.
   */
  def connect(http: Http, mcpUrl: String, clientId: String,
              secret: Option[String], scopes: Seq[String] = Nil)
             (using okay.Scheduler): Either[String, McpHttp.McpLink] ! Async =
    discover(http, mcpUrl).flatMap {
      case Left(e) => pure(Left(e))
      case Right(d) =>
        val client = OAuth2.Client(clientId, secret, d.authEndpoint,
          d.tokenEndpoint, "urn:ietf:wg:oauth:2.0:oob", scopes)
        OAuth2.clientCredentials(http, client).map {
          case Left(e) => Left(s"token endpoint: $e")
          case Right(tokens) =>
            // the supplier holds the token; refresh rotation can
            // swap it without rebuilding the link
            val current = java.util.concurrent.atomic.AtomicReference(tokens.access)
            Right(McpHttp.link(http, mcpUrl,
              bearer = Some(() => Option(current.get()))))
        }
    }

  // ---------------------------------------------------------------- small parts

  /** resource_metadata="..." off a WWW-Authenticate header */
  private[security] def resourceMetadataUrl(header: String): Option[String] =
    val marker = "resource_metadata=\""
    val at = header.indexOf(marker)
    if at < 0 then None
    else
      val rest = header.drop(at + marker.length)
      val end = rest.indexOf('"')
      if end < 0 then None else Some(rest.take(end))

  /** RFC 8414: the well-known path goes BETWEEN host and any AS path */
  private[security] def asMetadataUrl(as: String): String =
    val u = java.net.URI.create(as)
    val base = s"${u.getScheme}://${u.getAuthority}"
    val path = Option(u.getPath).filter(p => p.nonEmpty && p != "/").getOrElse("")
    s"$base/.well-known/oauth-authorization-server$path"

  private def firstAuthServer(doc: Json): Option[String] =
    Claims.field(doc, "authorization_servers") match
      case Some(Json.JArr(vs)) => vs.collectFirst { case Json.JStr(s) => s }
      case _ => None

  private def str(j: Json, n: String): Option[String] = Claims.str(j, n)

  private def fetchJson(http: Http, url: String): Either[String, Json] ! Async =
    http.send(Request.get(url)).flatMap { r =>
      okay.http.Http.text(r).map { t =>
        if !r.ok then Left(s"HTTP ${r.status} at $url")
        else Json.parse(t) match
          case Json.JErr(m) => Left(s"not JSON at $url: $m")
          case j => Right(j)
      }
    }

  // ── the ambient-Http doors (ctx-everywhere): pure delegation
  def discover(mcpUrl: String)(using http: Http): Either[String, Discovered] ! Async =
    discover(http, mcpUrl)
  def connect(mcpUrl: String, clientId: String, secret: Option[String],
              scopes: Seq[String])(using http: Http, s: okay.Scheduler)
  : Either[String, McpHttp.McpLink] ! Async =
    connect(http, mcpUrl, clientId, secret, scopes)
}
