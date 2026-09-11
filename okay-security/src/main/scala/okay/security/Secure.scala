package okay.security

import okay.{!, Async, pure}
import okay.http.{Request, Response}

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

  /**
   * The capability form (specs/context-functions.md, ctx-principal):
   * the principal is AMBIENT in the handler — `granted { ... }`
   * reads `summon[Principal]` (or takes `using`) instead of a
   * lambda parameter. Delegation, not reimplementation: the
   * 401/403 ladder is bearer's, byte for byte. ADDITIVE.
   */
  def granted(verify: String => Verified,
              policy: Policy = Policy.allowAll,
              realm: String = "okay",
              action: Request => String = _.method.name,
              resource: Request => String = _.url)
             (route: Principal ?=> PartialFunction[Request, Response ! Async])
  : PartialFunction[Request, Response ! Async] =
    bearer(verify, policy, realm, action, resource)(p => route(using p))

  /**
   * THE ADAPTER FOR A DECLARED ROUTE (specs/route-headers.md, stage B).
   *
   * `okay.http.Router.enforcing` reads what a route DECLARES and
   * refuses accordingly, but okay-http cannot name `Verified` — it is
   * the module this one depends on, and it must not grow an identity
   * model of its own. So its seam is a function: a bearer token in,
   * and either the scopes it grants or a refusal. This is that
   * function, made from the verifier a deployment already has.
   *
   * The refusal's WHY crosses the seam and is then dropped by
   * `enforcing`, deliberately: a caller gets a uniform 401 either way,
   * and the reason is the deployment's to log.
   *
   * What does NOT cross is a `Policy`. A declared route says "these
   * scopes"; a rule that reads the action or the resource stays with
   * `granted` below, which is why both roads exist.
   */
  def verifier(verify: String => Verified): okay.http.Router.Verify =
    t => verify(t) match
      case Verified.Ok(p) => Right(p.claims.scopes)
      case Verified.No(why) => Left(why)

  private def challenge(status: Int, realm: String, error: String): Response ! Async =
    pure(Response(status,
      Seq(("www-authenticate", s"""Bearer realm="$realm", error="$error"""")),
      okay.http.Http.one(Array.emptyByteArray)))
}
