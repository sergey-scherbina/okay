package okay.admin

import okay.{!, Async, pure}
import okay.http.{Http, Method, Request, Response}
import okay.security.{Policy, SessionIssuer, Verified}
import java.nio.charset.StandardCharsets.UTF_8

/**
 * Protected admin routes (specs/admin.md): a small named ACTION
 * (today: replay a projection from its log) wrapped in
 * `Secure.granted` + `Policy.scoped("admin")` — the same 401/403
 * ladder every protected route in this stack already uses. This
 * module holds no opinion about what "replay" means for a consumer;
 * `replay`/`onReplayed` are its own closures.
 */
object Admin {

  /** `replay` answers how many turns it replayed; `onReplayed` is a
   * side effect to run after (e.g. a market-feed ping) — both the
   * caller's own concern, unknown to this module */
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async] =
    okay.security.Secure.granted(verify, policy, realm) {
      case r if r.method == Method.Post && r.url == "/admin/replay" =>
        val n = replay()
        onReplayed()
        pure(Response(200, Seq("content-type" -> "text/html; charset=utf-8"),
          Http.one(htmlFor(n).getBytes(UTF_8))))
    }

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
