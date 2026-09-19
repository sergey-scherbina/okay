## script-scoped-state — ThreadLocal-with-public-setter closed in okay-script/api
Landed: 2026-09-19

`Scoped[A]` (`okay-script/.../api/Scoped.scala`): a `ThreadLocal` with
no public `set`, `where(value)(body)` binds for `body`'s extent only
and restores whatever was bound before on every exit, exception
included — the Scala/JDK21 answer to `java.lang.ScopedValue` (still
preview on JDK 21; JEP 506 finalizes it only in JDK 25).

Migrated: `Web`, `Response` (x2), `Session`, `Error`, `Container` (x4),
`Principal`, `Lang`, `Application`, `Content.roots` — 13 fields, all of
which had a public `setCurrent`/`setX` before this. `Site.servePage`'s
hand-matched setup (12 statements) + `finally` (11, already drifted —
`Content`'s root/problems were never in the reset list) collapses into
one `api.Requested.run(...)` call. `Principal`, previously reset to
`None` only by that `finally` surviving intact, now reverts by
construction — closing a real fragility: a pooled request thread's
*previous* principal was one dropped `finally` line away from leaking
into the next request.

Deliberately NOT `java.lang.ScopedValue` (preview) and NOT context
functions/`provide`/`wire` — the latter was tried and measured for
this exact subsystem already (specs/okay-script.md "Metadata as
context", 2026-09-03) and rejected: a re-declared `given` doesn't
shadow the way `val` does, and a `given` is memoized once, not per
read; `run`/`render` compile a page's script blocks as one flat body
with no nested lexical scopes for `?=>` to thread through.

See specs/script-scoped-state.md.
