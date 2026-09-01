package okay.security

import okay.{!, Async, pure}
import okay.http.{Http, Request, Response}

/**
 * Protection as a ROUTE WRAPPER: the protected route is a
 * `Principal => PartialFunction[...]`, so it cannot be reached
 * without a principal in scope — the type system holds the door, not
 * the call order of a middleware chain. A refusal is a response with
 * WWW-Authenticate, never an exception; 401 for "who are you", 403
 * for "not you".
 */
object Secure {

  /** the bearer token of a request, if one is presented */
  def bearerToken(r: Request): Option[String] =
    r.headers.collectFirst {
      case (k, v) if k.equalsIgnoreCase("authorization") && v.startsWith("Bearer ") =>
        v.drop(7)
    }

  def bearer(verify: String => Verified,
             policy: Policy = Policy.allowAll,
             realm: String = "okay",
             action: Request => String = _.method.name,
             resource: Request => String = _.url)
            (route: Principal => PartialFunction[Request, Response ! Async])
  : PartialFunction[Request, Response ! Async] = {
    // defined wherever the underlying route is — protection must not
    // change WHICH requests a route answers, only who gets through
    case r if route(Principal("", "", Claims())).isDefinedAt(r) =>
      bearerToken(r) match
        case None => challenge(401, realm, "no token")
        case Some(t) => verify(t) match
          case Verified.No(_) =>
            // the WHY stays server-side: a uniform refusal tells an
            // attacker nothing about how close the token was
            challenge(401, realm, "invalid_token")
          case Verified.Ok(p) => policy(p, action(r), resource(r)) match
            case Decision.Deny(_) => challenge(403, realm, "insufficient_scope")
            case Decision.Permit => route(p)(r)
  }

  private def challenge(status: Int, realm: String, error: String): Response ! Async =
    pure(Response(status,
      Seq(("www-authenticate", s"""Bearer realm="$realm", error="$error"""")),
      okay.http.Http.one(Array.emptyByteArray)))
}
