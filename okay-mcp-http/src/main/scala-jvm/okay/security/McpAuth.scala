package okay.security

import okay.{!, +, %, Async, Throws, pure, raise, runEither}
import okay.Row.at
import okay.given_Effects_Free
import okay.codec.Json
import okay.http.{Body, Http, McpHttp, Method, Request, Response}
import okay.mcp.{Mcp, Rpc}

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

  /**
   * The MCP route with the policy asked per TOOL rather than per URL
   * (specs/security.md stage 7).
   *
   * `protect` above can only say "this caller may POST /mcp": its
   * question is `(principal, method, url)` and every tool on the
   * server sits behind that one answer. Here the question is
   * `policy(p, Mcp.ToolsCall, <tool name>)`, asked once per tool, and
   * a tool a caller may not use is ABSENT — missing from
   * `tools/list`, and answering "no such tool" when named, which is
   * what a misspelling answers. Compose the two when a caller needs
   * both doors; neither implies the other.
   */
  def tools(verify: String => Verified, metadataUrl: String,
            policy: Policy = Policy.allowAll)
           (route: Request => Response ! Async): Request => Response ! Async =
    authenticated(verify, metadataUrl) { (p, r) =>
      gate(name => policy(p, Mcp.ToolsCall, name) match
        case Decision.Permit => true
        case Decision.Deny(_) => false)(route)(r)
    }

  /**
   * The same door, where the credential is a CAPABILITY and the
   * question is the capability's OWN.
   *
   * A `Policy` is the SERVER's table; a `Capability` is the HOLDER's.
   * `Capability.checking` with the tool as the scope is the module's
   * existing question — so whoever holds a capability narrows the
   * tool set for the agent it hands it to, with no issuer, no
   * registry and no round trip, and the authorization it was itself
   * granted is untouched because the root capability never moved.
   * Nothing new is spelled here: `Caveat.Scope` already says a tool.
   *
   * THE LIVENESS CHECK, and why it is shaped like that. A door must
   * tell "not yours" (401, which is also how a client DISCOVERS where
   * to authenticate) from "not for this tool" (absent). Asking for
   * the signature alone would do it and `Capability.verify` does not
   * offer that, on purpose. So the probe verifies the capability
   * against the scopes IT ITSELF names: that asks the chain and the
   * clock, refuses a caveat kind this verifier cannot enforce, and
   * is never the authorization decision — which stays per tool,
   * below.
   */
  def capabilities(rootKey: Array[Byte], metadataUrl: String,
                   now: () => Long = () => System.currentTimeMillis(),
                   scopeOf: String => String = (n: String) => "tool:" + n,
                   /**
                    * WHAT IS OFF, and the caller keeps the list.
                    *
                    * Asked about every identifier the capability
                    * carries: its root `id`, which voids the whole
                    * tree, and each `Agent` caveat, which voids one
                    * branch and everything attenuated from it. One
                    * predicate, because an operator holds one list of
                    * things that are off — and a function rather than
                    * a set, so a database or a cache fits too.
                    *
                    * NOT the tag: it is new at every attenuation, so a
                    * deny-list of tokens is escaped by one more
                    * `attenuate` (specs/security.md stage 7).
                    */
                   revoked: String => Boolean = _ => false)
                  (route: Request => Response ! Async)
                  (using Crypto): Request => Response ! Async =
    r =>
      Secure.bearerToken(r).flatMap(Capability.decode) match
        case None => challenge(metadataUrl, 401, "no token")
        case Some(cap) if !alive(cap, rootKey, now(), revoked) =>
          challenge(metadataUrl, 401, "invalid_token")
        case Some(cap) =>
          gate(name => cap.verify(rootKey,
            Capability.checking(now(), Set(scopeOf(name)), revoked)))(route)(r)

  /** well-formed, unexpired, not revoked and ours: the capability
   * against its own scopes, so an unknown caveat kind refuses here
   * rather than silently emptying the tool set */
  private def alive(cap: Capability, rootKey: Array[Byte], now: Long,
                    revoked: String => Boolean)(using Crypto): Boolean =
    !revoked(cap.id) && cap.verify(rootKey, Capability.checking(now,
      cap.caveats.flatMap(Caveat.parse).collect { case Caveat.Scope(n) => n }.toSet,
      revoked))

  /**
   * The narrowing itself, per REQUEST — a bearer arrives on every
   * request, so a permission withdrawn between two calls is refused
   * on the NEXT CALL rather than whenever the client reconnects. It
   * holds no state, which is the other half of that: a route cached
   * per caller would key on the allowed SET, and a holder can
   * attenuate into arbitrarily many distinct subsets, each minting a
   * session table, a channel and a fan-out fiber.
   *
   * IT FILTERS THE ANSWER TO `tools/list` AND DOES NOT COMPOSE ONE.
   * The stage owns the protocol: a `tools/list` before `initialize`
   * is `InvalidRequest`, a server with no tools answers
   * `MethodNotFound`. A guard that serves its own list re-implements
   * those branches and drifts from them.
   */
  private def gate(allowed: String => Boolean)
                  (route: Request => Response ! Async): Request => Response ! Async =
    r =>
      // the SSE stream is never read and never rewritten: reading a
      // body to filter it would consume the very thing being streamed
      if r.method == Method.Get then route(r)
      else Rpc.decode(bodyOf(r)) match
        case Rpc.Request(id, Mcp.ToolsCall, params) =>
          Mcp.callOf(params, Json.print(id)) match
            // stopped BEFORE the table runs, and answering exactly
            // what an unknown name answers
            case Some(c) if !allowed(c.name) =>
              pure(jsonRpc(200, Nil, Rpc.Answer(id,
                Mcp.contentResult(s"no such tool '${c.name}'", isError = true))))
            case _ => route(r)
        case Rpc.Request(_, Mcp.ToolsList, _) =>
          route(r).flatMap(withoutForbidden(allowed))
        case _ => route(r)

  /**
   * The tools this caller may not use, out of a `tools/list` answer.
   *
   * FAIL CLOSED: an entry whose name cannot be read is dropped, not
   * kept — this is an authorization boundary, and the shape we did
   * not understand is the one we cannot judge. Anything that is not
   * a tools answer at all (a session error, a `Failed`) travels
   * unchanged.
   */
  private def withoutForbidden(allowed: String => Boolean)(resp: Response)
  : Response ! Async =
    Http.text(resp).map { body =>
      val filtered = Rpc.decode(body) match
        case Rpc.Answer(id, result) => Rpc.encode(Rpc.Answer(id, result match
          case Json.JObj(fs) => Json.JObj(fs.map {
            case ("tools", Json.JArr(items)) => "tools" -> Json.JArr(items.filter {
              case Json.JObj(t) =>
                t.collectFirst { case ("name", Json.JStr(n)) => n }.exists(allowed)
              case _ => false
            })
            case kept => kept
          })
          case other => other))
        case _ => body
      Response(resp.status, resp.headers,
        Http.one(filtered.getBytes("UTF-8")))
    }

  private def bodyOf(r: Request): String = r.body match
    case Body.Text(s) => s
    case Body.Bytes(b) => String(b.toArray, java.nio.charset.StandardCharsets.UTF_8)
    case Body.Empty => ""

  private def jsonRpc(status: Int, extra: Seq[(String, String)], m: Rpc): Response =
    Response(status, extra :+ ("content-type", "application/json"),
      Http.one(Rpc.encode(m).getBytes("UTF-8")))

  /** the 401/403 that is the first step of the dance rather than a
   * dead end: `resource_metadata` says where to learn to authenticate */
  private def challenge(metadataUrl: String, status: Int, error: String)
  : Response ! Async =
    pure(Response(status, Seq(("www-authenticate",
      s"""Bearer resource_metadata="$metadataUrl", error="$error"""")),
      okay.http.Http.one(Array.emptyByteArray)))

  /** the bearer, verified — the half both `guard` and the tool doors
   * share, with no policy question of its own */
  private def authenticated(verify: String => Verified, metadataUrl: String)
                           (k: (Principal, Request) => Response ! Async)
  : Request => Response ! Async =
    r =>
      Secure.bearerToken(r) match
        case None => challenge(metadataUrl, 401, "no token")
        case Some(t) => verify(t) match
          case Verified.No(_) => challenge(metadataUrl, 401, "invalid_token")
          case Verified.Ok(p) => k(p, r)

  /** the one ladder `protect` and `granted` share */
  private def guard(verify: String => Verified, metadataUrl: String, policy: Policy)
                   (k: (Principal, Request) => Response ! Async): Request => Response ! Async =
    authenticated(verify, metadataUrl) { (p, r) =>
      policy(p, r.method.name, r.url) match
        case Decision.Deny(_) => challenge(metadataUrl, 403, "insufficient_scope")
        case Decision.Permit => k(p, r)
    }

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
    runEither(discovering(http, mcpUrl))

  /** the same walk with every missing link a `raise`: one flat chain
   * where each step is a line, instead of a pyramid of `Left` cases */
  private def discovering(http: Http, mcpUrl: String): Discovered ! Failing =
    for
      probe <- http.send(Request.post(mcpUrl, okay.http.Body.Text("{}"),
        Seq(("content-type", "application/json")))).at[Failing]
      _ <- when(probe.status != 401, s"the server did not challenge (HTTP ${probe.status}) — it may be open")
      metaUrl <- present(probe.header("www-authenticate").flatMap(resourceMetadataUrl),
        "the 401 carried no resource_metadata")
      doc <- fetchJson(http, metaUrl).at[Failing].flatMap(right(_, "resource metadata"))
      as <- present(firstAuthServer(doc), "the resource metadata names no authorization server")
      asDoc <- fetchJson(http, asMetadataUrl(as)).at[Failing].flatMap(right(_, "authorization server metadata"))
      ends <- present(str(asDoc, "authorization_endpoint").zip(str(asDoc, "token_endpoint")),
        "the AS metadata lacks endpoints")
    yield Discovered(str(doc, "resource").getOrElse(mcpUrl), as, ends._1, ends._2)

  /**
   * The machine-to-machine dance, whole: discover, obtain by client
   * credentials, hand back a bearer-carrying link. The interactive
   * code+PKCE path is stage 0's `authorizationUrl`/`exchange` with
   * the browser the caller owns — a library cannot click consent.
   */
  def connect(http: Http, mcpUrl: String, clientId: String,
              secret: Option[String], scopes: Seq[String] = Nil)
             (using okay.Scheduler): Either[String, McpHttp.McpLink] ! Async =
    runEither(for
      d <- discovering(http, mcpUrl)
      client = OAuth2.Client(clientId, secret, d.authEndpoint,
        d.tokenEndpoint, "urn:ietf:wg:oauth:2.0:oob", scopes)
      tokens <- OAuth2.clientCredentials(http, client).at[Failing].flatMap(right(_, "token endpoint"))
    yield
      // the supplier holds the token; refresh rotation can swap it
      // without rebuilding the link
      val current = java.util.concurrent.atomic.AtomicReference(tokens.access)
      McpHttp.link(http, mcpUrl, bearer = Some(() => Option(current.get()))))

  /** the row the flat chains run in: a named failure, and the network */
  private type Failing = Throws % String + Async

  private def when(bad: Boolean, why: => String): Unit ! Failing =
    if bad then raise[String, Unit](why).at[Failing] else pure(())

  private def present[A](o: Option[A], why: => String): A ! Failing =
    o.fold(raise[String, A](why).at[Failing])(pure)

  private def right[A](e: Either[String, A], label: String): A ! Failing =
    e.fold(m => raise[String, A](s"$label: $m").at[Failing], pure)

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
