package okay.admin

import okay.{!, Async, pure}
import okay.http.{Http, Method, Request, Response}
import okay.security.{Secure, SessionIssuer, Verified}
import java.nio.charset.StandardCharsets.UTF_8

/**
 * Protected admin routes (specs/admin.md): a small named ACTION
 * (today: replay a projection from its log) that DECLARES what it
 * requires, so the router can refuse and a document can say so —
 * specs/route-headers.md, stage B.
 *
 * It used to wrap `Secure.granted` around the finished
 * `PartialFunction`. That worked and could not be described: the
 * requirement was applied AFTER the table was built, never reached
 * the entry, and `OpenApi.document` rendered `POST /admin/replay` as
 * an open door. A caller who believed the document got a 401 the
 * document never mentioned.
 *
 * `Secure.granted` is not deprecated by this. A rule that reads the
 * ACTION or the RESOURCE — `Policy.role`, anything richer than "these
 * scopes" — still belongs there; what a route can DECLARE is a
 * scheme and its scopes, because that is what a document can render
 * and a table can enforce.
 *
 * This module holds no opinion about what "replay" means for a
 * consumer; `replay`/`onReplayed` are its own closures.
 */
object Admin {

  /** `replay` answers how many turns it replayed; `onReplayed` is a
   * side effect to run after (e.g. a market-feed ping) — both the
   * caller's own concern, unknown to this module */
  /**
   * The TABLE, declaring what it requires — what a renderer reads.
   *
   * Nothing enforces it yet, and nothing serves it either: a secured
   * entry with no verifier FAILS CLOSED, so this value answers 401 to
   * everyone until `routes` installs one. That is deliberate —
   * forgetting to enforce must be a loud mistake, not a silent hole.
   */
  def router(scopes: Set[String] = Set("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit): okay.http.Router =
    // declared (docs/declaring-an-api.md): the route used to compare
    // the WHOLE request target, so a cache-buster or a tracking
    // parameter turned an authorised replay into a miss. A route cuts
    // the query before it matches
    okay.http.Router.on(Method.Post,
      (okay.http.Route / "admin" / "replay")
        .securedBy(okay.http.Route.Security(scopes = scopes, realm = realm))) { (_, _) =>
      val n = replay()
      onReplayed()
      pure(Response(200, Seq("content-type" -> "text/html; charset=utf-8"),
        Http.one(htmlFor(n).getBytes(UTF_8))))
    }

  /** the same table with a deployment's verifier installed — what a
   * consumer mounts. `Secure.verifier` is the adapter across the
   * module boundary: okay-http may not name `Verified`. */
  def routes(verify: String => Verified,
             scopes: Set[String] = Set("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async] =
    router(scopes, realm)(replay, onReplayed)
      .enforcing(Secure.verifier(verify)).routes

  private def htmlFor(n: Long): String =
    "<!doctype html><meta charset=\"utf-8\"><title>replay</title>" +
      "<style>body{font:15px system-ui;background:#10141a;color:#e6e9ef;padding:2rem}</style>" +
      s"<p>проекция перестроена из журнала: $n ходов</p>" +
      "<p><a style=\"color:#6b9fff\" href=\"/market\">→ /market</a></p>"

  /**
   * A minimal in-process admin credential — okay-security's
   * `SessionIssuer` (specs/security.md, security-sessions), scoped
   * "admin". Exists so a consumer has SOMETHING to test/use `routes`
   * with; a deployment with a real identity provider supplies its
   * own `verify` instead — `routes` only ever needs
   * `String => Verified`.
   */
  object Issuer:
    private val issuer = SessionIssuer()

    /** a long-lived admin-scoped token */
    def issue(now: Long = System.currentTimeMillis()): String =
      issuer.issue("admin", scopes = Set("admin"), now = now)

    val verify: String => Verified = t => issuer.verify(t)
}
