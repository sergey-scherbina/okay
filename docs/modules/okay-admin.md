# okay-admin

> Protected admin routes (specs/admin.md): a named action wrapped in
> the same bearer-token 401/403 ladder every other protected route in
> this stack uses. Extracted 2026-09-02 from `okay-demo`, fixing a
> real gap found while extracting it — `POST /admin/replay` had
> shipped with no authentication at all.

Depends on: `okay-security` (`Secure`, `Jwt`, `Policy`, `Verified`),
`okay-http`. JVM-only.

## Guide

**The route.** `Admin.routes(verify, policy = Policy.scoped("admin"),
realm = "okay-admin")(replay, onReplayed)` answers `POST
/admin/replay` behind `Secure.granted` — a missing or invalid token is
401, a token without the `admin` scope is 403, both with
`WWW-Authenticate`. `replay: () => Long` and `onReplayed: () => Unit`
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
