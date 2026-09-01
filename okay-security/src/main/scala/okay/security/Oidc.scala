package okay.security

import okay.{!, Async, pure}
import okay.codec.Json
import okay.http.{Http, Request}

/**
 * OpenID Connect (specs/security.md stage 3) — user login, assembled
 * from pieces this module already had: discovery is one GET, the
 * login URL is OAuth2's with `openid` and a nonce, the tokens come
 * from OAuth2.exchange, and the id_token is a JWT whose validation
 * adds exactly four OIDC-specific checks to Jwt.verify's — issuer,
 * audience-is-the-client, nonce, and at_hash.
 *
 * Every refusal is a named `No`; a stolen or spliced id_token fails
 * on the check that catches it, and the caller can log which.
 */
object Oidc {

  /** what discovery answered — the caller SEES the issuer's endpoints
   * before any secret or credential travels (the McpAuth rule) */
  final case class Provider(issuer: String, authEndpoint: String,
                            tokenEndpoint: String, jwksUri: String)

  /** {issuer}/.well-known/openid-configuration */
  def discover(http: Http, issuer: String): Either[String, Provider] ! Async =
    val url = issuer.stripSuffix("/") + "/.well-known/openid-configuration"
    http.send(Request.get(url)).flatMap { r =>
      okay.http.Http.text(r).map { t =>
        if !r.ok then Left(s"HTTP ${r.status} at $url")
        else
          val j = Json.parse(t)
          (Claims.str(j, "issuer"), Claims.str(j, "authorization_endpoint"),
            Claims.str(j, "token_endpoint"), Claims.str(j, "jwks_uri")) match
            case (Some(i), Some(a), Some(tk), Some(k)) => Right(Provider(i, a, tk, k))
            case _ => Left("the discovery document lacks required fields")
      }
    }

  /** one login attempt's client-held state: send the url, keep this */
  final case class Attempt(url: String, state: String, nonce: String, verifier: String)

  /** the login URL: OAuth2's code+PKCE with `openid` and a nonce */
  def login(p: Provider, clientId: String, redirectUri: String,
            scopes: Seq[String] = Seq("openid"))(using c: Crypto): Attempt =
    val (verifier, challenge) = OAuth2.pkce()
    val state = java.util.Base64.getUrlEncoder.withoutPadding
      .encodeToString(c.randomBytes(16))
    val nonce = java.util.Base64.getUrlEncoder.withoutPadding
      .encodeToString(c.randomBytes(16))
    val client = OAuth2.Client(clientId, None, p.authEndpoint, p.tokenEndpoint,
      redirectUri, if scopes.contains("openid") then scopes else "openid" +: scopes)
    Attempt(OAuth2.authorizationUrl(client, state, challenge) + "&nonce=" + nonce,
      state, nonce, verifier)

  /**
   * The callback: exchange the code, then validate the id_token —
   * signature by the issuer's JWKS, issuer, audience-is-the-client,
   * nonce, expiry with skew, and at_hash when present (the left half
   * of sha256(access_token), base64url — RFC-checked so a spliced
   * access token does not ride a genuine id_token).
   */
  def callback(http: Http, p: Provider, clientId: String, redirectUri: String,
               attempt: Attempt, code: String, now: Long, skew: Long = 60)
              (using Crypto): Either[String, (Principal, OAuth2.Tokens)] ! Async =
    val client = OAuth2.Client(clientId, None, p.authEndpoint, p.tokenEndpoint,
      redirectUri, Seq("openid"))
    OAuth2.exchange(http, client, code, attempt.verifier).flatMap {
      case Left(e) => pure(Left(s"token endpoint: $e"))
      case Right(tokens) => tokens.idToken match
        case None => pure(Left("no id_token in the answer"))
        case Some(idt) =>
          Jwks.fetch(http, p.jwksUri).map { keys =>
            validate(idt, keys.get, p.issuer, clientId, attempt.nonce,
              tokens.access, now, skew).map(principal => (principal, tokens))
          }
    }

  /** the id_token checks alone — for tokens that arrive by other
   * roads (a front channel, a test) */
  def validate(idToken: String, keys: String => Option[Jwt.Key],
               issuer: String, clientId: String, nonce: String,
               accessToken: String, now: Long, skew: Long = 60)
              (using c: Crypto): Either[String, Principal] =
    Jwt.verify(idToken, keys, audience = Some(clientId), now, skew) match
      case Verified.No(why) => Left(why)
      case Verified.Ok(p) =>
        val j = p.claims.json
        if !p.claims.issuer.contains(issuer) then
          Left(s"issuer is ${p.claims.issuer.getOrElse("absent")}, expected $issuer")
        else if !Claims.str(j, "nonce").contains(nonce) then
          Left("nonce does not match this attempt")
        else Claims.str(j, "at_hash") match
          case Some(ah) if atHash(accessToken) != ah =>
            Left("at_hash does not match the access token")
          case _ => Right(p)

  /** base64url of the LEFT HALF of sha256(access_token) — OIDC core 3.1.3.6 */
  def atHash(accessToken: String)(using c: Crypto): String =
    java.util.Base64.getUrlEncoder.withoutPadding.encodeToString(
      c.sha256(accessToken.getBytes("US-ASCII")).take(16))
}
