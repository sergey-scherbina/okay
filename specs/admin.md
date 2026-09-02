# okay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.- [x]ay-admin: protected admin routes

## Overview

Second of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md landed first). The
demo's `POST /admin/replay` was, until this lands, completely
UNAUTHENTICATED — anyone who could reach the route could drop and
rebuild the marketplace projection. That was a real gap found while
planning the extraction, not a hypothetical: this module fixes it as
part of moving the route out, rather than moving an insecure route
verbatim and leaving the fix for later.

The fix is delegation, not invention: `okay-security`'s `Secure.
granted` already gives every protected route in this stack the same
401/403 ladder (`Secure.scala`), and `Policy.scoped(scope)` already
exists — proven in `okay-security`'s own test suite (`TestGranted`,
`TestMcpAuth`) — as the convention for "this route needs a named
scope," just never wired into a shipped route until now. `okay-admin`
is that wiring, plus a minimal credential story so a demo (or any
small app) doesn't have to hand-roll ES256 to get one admin token.

## The model

Two pieces:

- **`Admin.routes`** — the route wrapper. Takes the app's own
  `replay`/`onReplayed` actions as closures (the marketplace-specific
  `replayProjections(chatLog)` / `marketChanged("replay")` stay in
  the demo; this module never learns `MatchStore` or `ChatLog`
  exist), and a `verify`/`policy` pair — `Policy.scoped("admin")` by
  default, matching the convention `okay-security`'s tests already
  established.
- **`Admin.Issuer`** — a minimal in-process admin credential, the
  SAME shape as `okay.demo.Login` (an ES256 keypair, one per process
  — a restart signs the admin out too, stated not hidden): `issue()`
  mints a long-lived admin-scoped token, `verify` checks it. This
  exists so a consumer has SOMETHING to test/use the protected route
  with out of the box; a deployment with a real identity provider
  supplies its own `verify: String => Verified` instead — the route
  wrapper only ever needs that one function type, never the Issuer
  specifically.

## Interface

```scala
package okay.admin

object Admin:
  def routes(verify: String => Verified,
             policy: Policy = Policy.scoped("admin"),
             realm: String = "okay-admin")
            (replay: () => Long, onReplayed: () => Unit)
  : PartialFunction[Request, Response ! Async]

  object Issuer:
    def issue(now: Long = System.currentTimeMillis()): String
    val verify: String => Verified
```

- `routes` is `Secure.granted(verify, policy, realm) { case ... }` —
  delegation, so the 401/403 behavior is byte-identical to every
  other protected route in this stack; this module adds no new
  refusal shape.
- `replay: () => Long` answers how many turns were replayed (the
  demo's `replayProjections` already returns this); the route's HTML
  response names the count, same copy as today's unauthenticated
  version.
- `Issuer.issue`/`verify` are one ES256 keypair per process — the
  SAME limitation `Login` already states about itself: a restart
  signs everyone (including the admin) out. A deployment wanting
  persistent admin credentials swaps `Issuer.verify` for its own
  `String => Verified` (e.g. a real IdP's JWKS-backed verifier,
  `okay-security`'s `Jwks.fetch` already exists for that) — `routes`
  never cares which `verify` it was handed.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `handler`/`routes` composes
  `Admin.routes(Admin.Issuer.verify)(() => replayProjections(chatLog),
  () => marketChanged("replay"))` via `orElse`, replacing the old
  inline unauthenticated `case`. The admin token is printed to the
  server console at startup — the same "no delivery channel yet, so
  the credential rides the console" precedent `Login.start`'s
  one-time code already set (specs/demo-chat.md, Sessions).

- [x] an unauthenticated request to `/admin/replay` gets 401 with
      `WWW-Authenticate`, naming "no token" — the route no longer
      answers to anyone who merely reaches it
- [x] a token without the `admin` scope gets 403, `insufficient_scope`
      — `Policy.scoped("admin")` enforced, not just "any valid token"
- [x] a token WITH the `admin` scope succeeds: the projection is
      rebuilt, the response names how many turns replayed
- [x] `Admin.Issuer.issue()` produces a token `Admin.Issuer.verify`
      accepts, carrying the `admin` scope
- [x] through the real demo route: the old unauthenticated behavior
      is GONE (a plain POST with no token now 401s) and the
      authenticated path reaches `replayProjections`/`marketChanged`
      exactly as before

## Filed (BACKLOG slugs, not built this pass)

- **okay-chat** — third of the three extractions; API sketch already
  recorded in specs/subscription.md's "Filed" section and
  BACKLOG.md's "Reusable modules" — unchanged by this landing.

## Decisions

- **Delegation over reimplementation** — `Admin.routes` is `Secure.
  granted` plus one case; no new auth primitive, no new refusal
  shape. The 401/403 ladder some future reviewer audits is
  `okay-security`'s ladder, audited once.
- **`Issuer` ships because "protected route, no way to get a token"
  is not a finished feature** — a consumer needs a credential story
  to even exercise the route; `Issuer` is the smallest one
  (`Login`'s own shape, copied) that does not force a real IdP
  integration on a demo that does not have one.
- **`replay`/`onReplayed` stay as closures, not a capability
  parameter** — `okay-admin` has zero opinion about what an admin
  action DOES; today it is one action (replay), the shape leaves
  room for more without this module knowing their names.
