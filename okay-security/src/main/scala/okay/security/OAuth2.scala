package okay.security

import okay.{!, Async}
import okay.codec.Json
import okay.http.{Body, Http, Request}

/**
 * The OAuth2 CLIENT flows over the Http seam: authorization code
 * with PKCE (S256 — required, not optional, per current practice and
 * MCP's own authorization spec), refresh, and client credentials.
 * This module is not an authorization server; the stub AS in the
 * tests is a test.
 *
 * A token-endpoint failure is a Left with the server's error, never
 * a throw — the endpoint's whole job is to refuse strangers, and a
 * refusal is not exceptional.
 */
object OAuth2 {

  final case class Client(id: String, secret: Option[String],
                          authEndpoint: String, tokenEndpoint: String,
                          redirectUri: String, scopes: Seq[String] = Nil)

  final case class Tokens(access: String, refresh: Option[String],
                          expiresIn: Option[Long], idToken: Option[String])

  private val enc = java.util.Base64.getUrlEncoder.withoutPadding

  /** a PKCE pair: the verifier stays, the S256 challenge travels */
  def pkce()(using c: Crypto): (String, String) =
    val verifier = enc.encodeToString(c.randomBytes(32))
    (verifier, enc.encodeToString(c.sha256(verifier.getBytes("US-ASCII"))))

  /** where to send the browser */
  def authorizationUrl(c: Client, state: String, challenge: String): String =
    c.authEndpoint + "?" + query(
      "response_type" -> "code",
      "client_id" -> c.id,
      "redirect_uri" -> c.redirectUri,
      "scope" -> c.scopes.mkString(" "),
      "state" -> state,
      "code_challenge" -> challenge,
      "code_challenge_method" -> "S256")

  /** the code for tokens, with the PKCE verifier */
  def exchange(http: Http, c: Client, code: String, verifier: String)
  : Either[String, Tokens] ! Async =
    token(http, c,
      "grant_type" -> "authorization_code",
      "code" -> code,
      "redirect_uri" -> c.redirectUri,
      "code_verifier" -> verifier)

  def refresh(http: Http, c: Client, refreshToken: String)
  : Either[String, Tokens] ! Async =
    token(http, c, "grant_type" -> "refresh_token", "refresh_token" -> refreshToken)

  def clientCredentials(http: Http, c: Client): Either[String, Tokens] ! Async =
    token(http, c, "grant_type" -> "client_credentials",
      "scope" -> c.scopes.mkString(" "))

  private def token(http: Http, c: Client, params: (String, String)*)
  : Either[String, Tokens] ! Async =
    val form = query((("client_id" -> c.id) +:
      c.secret.map("client_secret" -> _).toSeq ++: params.toSeq)*)
    http.send(Request.post(c.tokenEndpoint, Body.Text(form),
      Seq(("content-type", "application/x-www-form-urlencoded"))))
      .flatMap { r =>
        okay.http.Http.text(r).map { body =>
          val j = Json.parse(body)
          if !r.ok then
            Left(Claims.str(j, "error").getOrElse(s"HTTP ${r.status}"))
          else Claims.str(j, "access_token") match
            case None => Left("no access_token in the answer")
            case Some(at) => Right(Tokens(at,
              Claims.str(j, "refresh_token"),
              Claims.num(j, "expires_in"),
              Claims.str(j, "id_token")))
        }
      }

  private def query(params: (String, String)*): String =
    params.map((k, v) => k + "=" + java.net.URLEncoder.encode(v, "UTF-8"))
      .mkString("&")

  /** the other side of the form, for the stub AS in tests and for
   * anyone serving a token endpoint later: decode a urlencoded body */
  def form(body: String): Map[String, String] =
    body.split('&').toSeq.flatMap { kv =>
      kv.split("=", 2) match
        case Array(k, v) => Some(k -> java.net.URLDecoder.decode(v, "UTF-8"))
        case _ => None
    }.toMap

  // ── the ambient-Http doors (ctx-everywhere): pure delegation —
  // the one recurring environment of these flows
  def exchange(c: Client, code: String, verifier: String)(using http: Http)
  : Either[String, Tokens] ! okay.Async = exchange(http, c, code, verifier)
  def refresh(c: Client, refreshToken: String)(using http: Http)
  : Either[String, Tokens] ! okay.Async = refresh(http, c, refreshToken)
  def clientCredentials(c: Client)(using http: Http)
  : Either[String, Tokens] ! okay.Async = clientCredentials(http, c)
}
