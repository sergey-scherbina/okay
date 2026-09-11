# okay-admin

> Protected admin routes (specs/admin.md): a named action that
> DECLARES what it requires, and a table that refuses accordingly —
> the same bearer-token 401/403 ladder, now visible to a document.
> Extracted 2026-09-02 from `okay-demo`, fixing a real gap found while
> extracting it — `POST /admin/replay` had shipped with no
> authentication at all.

Depends on: `okay-security` (`Secure`, `Jwt`, `Verified`),
`okay-http`. JVM-only.

## Guide

**The route.** `Admin.routes(verify, scopes = Set("admin"), realm =
"okay-admin")(replay, onReplayed)` answers `POST /admin/replay` — a
missing or invalid token is 401, a token without the `admin` scope is
403, both with `WWW-Authenticate`.

**The requirement is DECLARED, not wrapped** (specs/route-headers.md,
stage B). `Admin.router(...)` is the table saying what it needs;
`routes` is that table with a deployment's verifier installed, through
`Secure.verifier`. The difference is that a renderer can now see it:
`POST /admin/replay` used to appear in an OpenAPI document as an open
door, because `Secure.granted` wrapped the finished `PartialFunction`
and the requirement never reached the entry.

`Admin.router(...)` on its own **fails closed** — a secured entry with
no verifier answers 401 `no_verifier` rather than serving. Forgetting
to enforce is a route that refuses everyone, not a hole.

A policy richer than "these scopes" — one that reads the action or the
resource — still belongs in `Secure.granted`; what a route can declare
is what a document can render and a table can enforce. `replay: () => Long` and `onReplayed: () => Unit`
are the caller's own concern; this module has no opinion on what
"replay" means — `okay-demo` passes its `ChatDemo.replayProjections`
and a market-feed ping.

**Getting a token to test with.** `Admin.Issuer` is a minimal
in-process ES256 credential — the same shape as `okay.demo.Login`
(one key pair per process; a restart signs the admin out too, stated
not hidden). `Issuer.issue()` mints a long-lived admin-scoped token,
`Issuer.verify` checks one. A deployment with a real identity
provider supplies its own `verify: String => Verified` instead —
`routes` asks for nothing more than that function.

**Wiring it in**, `okay-demo`'s way:

```scala
core.orElse(Admin.routes(Admin.Issuer.verify)(
  () => replayProjections(chatLog), () => marketChanged("replay")))
```

with the token printed once at startup (`println(s"admin token
(okay-admin, /admin/replay): ${Admin.Issuer.issue()}")`) and the
`/market` page's replay button sending it as `Authorization: Bearer`
via `fetch` rather than a plain form POST.

| | |
|---|---|
| `Admin.routes(verify, policy, realm)(replay, onReplayed)` | `POST /admin/replay`, protected |
| `Admin.Issuer.issue()` / `.verify` | the in-process ES256 credential |
