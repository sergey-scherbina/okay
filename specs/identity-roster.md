# identity-roster — a channel address bound to a principal, and who may do what

## Overview

okay-security has the primitives — `Principal`, `Claims`, `Policy` as
`(principal, action, resource) => Decision`, `Capability`, JWT/OIDC — and
**no store**: no users, no roles, and no way to say that Telegram user
`123456` *is* a principal. okay-chat needed all three and wrote them as
application code (`okaychat.Identity`: `Channel`, `Binding`, a topic,
`bind`/`profileFor`/`request`/`verify`); okay-watch's bots have the
allowlist form. The first consumer that needs it as a library is
`../nadia`'s okay implementation (nadia `docs/specs/app.md` "Access"): an
owner by Telegram id, invited users with a role, a policy the screen
consults so a viewer's screen has no button they may not press.

The operator's rule (2026-09-29): what has no product in it is okay's.
A roster has none. This spec lifts the shape okay-chat proved, minus its
product (profiles, e-mail codes, the marketplace), into okay-security.

## Interface

```scala
package okay.security

/** where a message came from — total: a channel this module does not
 * name is `Other(kind)` */
enum Channel:
  case Telegram, Web, Console, Mcp, Mail
  case Other(kind: String)

/** an address on a channel, bound to a principal at a time */
final case class Binding(channel: Channel, address: String, principal: String, at: Long)

/** a role is a fact about a principal, read by a Policy */
final case class Grant(principal: String, role: String, scope: Option[String], at: Long)

final class Roster(topic: okay.persist.Topic):
  def load(): Int                                              // fold the topic; how many records
  def bind(channel: Channel, address: String, principal: String): Binding
  def unbind(channel: Channel, address: String): Boolean
  def whoIs(channel: Channel, address: String): Option[Principal]
  def addressesOf(principal: String): Vector[(Channel, String)]
  def grant(principal: String, role: String, scope: Option[String] = None): Grant
  def revoke(principal: String, role: String, scope: Option[String] = None): Boolean
  def rolesOf(principal: String): Vector[Grant]
  def principals: Vector[Principal]

object Roster:
  /** a Policy that reads roles from the roster: Permit when the principal
   * holds `role`, scoped to `resource` when the grant has a scope */
  def role(r: Roster, role: String): Policy
  /** the owner: one principal granted `owner` at construction, from
   * configuration — never "whoever wrote first" */
  def owned(topic: Topic, channel: Channel, address: String): Roster
```

The records on the topic, keyed by principal:

```
Bound(channel, address, principal, at)
Unbound(channel, address, at)
Granted(principal, role, scope, at)
Revoked(principal, role, scope, at)
```

`Principal` is the existing one: `id` is the roster's principal string,
`name` the last bound address, `claims.roles` the roles held.

## Behavior

- [ ] `owned(topic, Telegram, "123")` yields a roster whose `whoIs(Telegram, "123")` is a
      principal with role `owner`; `whoIs(Telegram, "124")` is `None`
- [ ] `bind` then `whoIs` round-trips; a second `bind` of the same address to another
      principal replaces the first (`addressesOf` the first no longer lists it)
- [ ] `grant(p, "operator", Some("/work/x"))` then `Roster.role(r, "operator")(p, "run",
      "/work/x")` is `Permit`, `(p, "run", "/work/y")` is `Deny`; an unscoped grant permits
      every resource
- [ ] `revoke` of a role a principal does not hold is `false` and appends nothing
- [ ] a roster reloaded from its topic (`load()`) equals the one that wrote it: same
      bindings, same grants, in a property over random sequences of operations
- [ ] `Roster.role` composes with the existing `Policy.allOf`/`anyOf`: `anyOf(role(r,
      "owner"), role(r, "operator"))` behaves as the disjunction
- [ ] the console channel: `whoIs(Console, "")` is the owner — the process is the owner's
- [ ] no record ever carries a secret: the topic's bytes for every test contain no token

## Out of scope

- Proving an address (a one-time code to an e-mail, a Telegram deep link). okay-chat's
  `request`/`verify` stays there until a second consumer needs it; `bind` here is the
  owner's act (an invite), already trusted.
- Sessions and tokens for the web channel. `SessionIssuer` exists; binding a session's
  principal is `bind(Web, sessionId, principal)` by the caller.
- Groups, teams, inheritance. A scope on a grant is the only structure.
- Sharing a resource by link. It is the next consumer's spec (nadia P7); the roster is
  its hook, not its implementation.

## Design

- **A roster is a fold of a topic**, exactly like okay-chat's `Identity` and
  agent-fleet's `Status`: append, then apply; `load()` replays. Cross-platform (the
  topic is okay-persist's, which is), so the JS host draws the same access screen.
- **Roles are `Grant`s, not fields on `Principal`.** `Principal` is a value handed
  around; the roster is where it changes. `rolesOf` builds the `Claims.roles` view.
- **`owned` is the only constructor that grants without a grantor**, and it takes the
  owner from configuration. This is the rule nadia's spec states for the bot — the
  owner is never "whoever finds it" — made structural.

## Decisions

- **Lifted from okay-chat rather than designed fresh** — chosen because that code has
  run against real users for months and its shape (bind / whoIs / roles-as-facts) is
  the part with no product in it. Rejected: okay-watch's allowlist file — no roles,
  no scope, no history.
- **Scope is a string prefix, matched by the policy** — chosen as the smallest thing
  that lets "operator of project X" be said. Rejected: a resource algebra — nothing
  needs it yet; the string is upgradable without a record change.

## Implementation lane

`identity-roster` — okay-security, additive (`Roster.scala`, `TestRoster`);
okay-chat may later replace its `Identity` internals with it, as its own lane.

## Results

Not implemented yet.
