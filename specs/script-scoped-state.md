# Scoped request state (script-scoped-state, 2026-09-19)

Prompted by a Habr article on `java.lang.ScopedValue` replacing
`ThreadLocal` for security-context propagation: no public `set()` so
a callee cannot mutate the binding, install-for-a-block only, cleanup
automatic even on exception. `okay-script/src/main/scala/okay/script/
api/{Api,Application,Content}.scala` has the exact ThreadLocal shape
the article calls risky — a `ThreadLocal` plus a public `setCurrent`/
`setX`, readable and writable from anywhere with a reference.

## Why not `java.lang.ScopedValue` itself

Still preview on JDK 21 (this repo's JDK) — JEP 429/481, finalized
only in JDK 25 (JEP 506). Would need `--enable-preview` across the
whole build for one subsystem. Not worth it when option two below
costs nothing extra.

## Why not context functions (`?=>` / `provide` / `wire`)

Tried and measured for this exact subsystem already —
`specs/okay-script.md` "Metadata as context" (okay-script-meta,
2026-09-03). Two facts killed it there, and they apply here
unchanged:

1. A re-declared `given` at the same flat scope is a compile error
   (unlike `val`, no local-shadowing leniency).
2. A `given` is evaluated ONCE (memoized), not per read.

`run`/`render` compile a `.md` file's script blocks as ONE FLAT BODY
(deliberately — a later block sees an earlier block's `val`/`def`,
same as a REPL session), so there are no nested lexical scopes for
`?=>` to thread through, and a script reads `Web.current`/
`Principal.current`/etc. from an arbitrary point in that flat body,
same as `Meta.current`. The article's `ScopedValue` propagates
across arbitrary call depth without any change to a function's
signature; `A ?=> B` propagates only where every function on the
path is itself typed `?=>` (or takes `using A`) — it is Reader, not
dynamic scope. Migrating to it would mean threading `Web ?=>` through
every function transitively reachable from a page's flat body,
including page-authored code the codegen does not control. Out of
proportion to the actual defect, which is the missing "no set()"
guarantee, not the reader's shape.

## The fix: `Scoped[A]`

`ThreadLocal`, kept a plain always-fresh read (same as `Meta.current`
today), with the mutation hole closed:

```scala
package okay.script.api

/** ScopedValue's shape, ported to where `java.lang.ScopedValue` is
 * still preview: a `ThreadLocal` with no public `set`. `where` binds
 * `value` for `body`'s extent only, restores whatever was bound
 * before (nesting resolves to the nearest `where`) however `body`
 * exits, exception included. */
final class Scoped[A] private (default: () => A):
  private val local: ThreadLocal[A] = ThreadLocal.withInitial(() => default())
  def current: A = local.get()
  def where[B](value: A)(body: => B): B =
    val prior = local.get()
    local.set(value)
    try body finally local.set(prior)

object Scoped:
  def apply[A](default: => A): Scoped[A] = new Scoped(() => default)
```

No `set`/`setCurrent` escapes `Scoped` itself. Each of the 13 fields
below keeps its existing `current`-style getter and gains a `where`
in place of its setter — same public read API (so nothing that only
reads `Web.current` etc. changes), no public write API at all.

## What moves, file by file

**`okay-script/src/main/scala/okay/script/api/Api.scala`** (11
fields, 7 objects) — `Web.scoped`, `Response.secureScoped`,
`Response.scoped`, `Session.scoped`, `Error.scoped`, `Container`'s
four (`includerScoped`, `livesScoped`, `translatorsScoped`,
`issuersScoped`), `Principal.scoped`, `Lang.scoped`. Each object's
`current` reads `.current`; each drops its `setCurrent`/`setX` for a
`where`.

**`Application.scala`** (1 field) — same shape, `Application.scoped`.

**`Content.scala`** (1 field: `roots`) — `Content.setRoot` becomes
`Content.where`. `problems` is UNCHANGED and out of scope: it has no
public setter today (`clearProblems` is `private[script]`, the only
mutator besides the private `read`-internal accumulation) — it
already meets the article's bar.

**`api.Requested`** (new, in Api.scala) — one call that nests every
per-request `where` and collapses `Site.servePage`'s hand-matched
setup (12 statements) + `finally` (11 statements, already drifted —
see the sprint item) into a single composed scope:

```scala
object Requested:
  def run[B](web: Web, resp: Response, sess: Session, lang: String,
             translator: Option[String => Option[String]],
             includer: Option[String => String],
             liveRegistrar: Option[(String, Live[?]) => Unit],
             issuer: Option[(String, Set[String]) => String],
             secure: Boolean, application: Application,
             contentRoot: Option[java.nio.file.Path])(body: => B): B =
    Web.scoped.where(web):
      Response.scoped.where(resp):
        Response.secureScoped.where(secure):
          Session.scoped.where(sess):
            Error.scoped.where(None):
              Lang.scoped.where(lang):
                Container.includerScoped.where(includer):
                  Container.livesScoped.where(liveRegistrar):
                    Container.translatorsScoped.where(translator):
                      Container.issuersScoped.where(issuer):
                        Application.scoped.where(application):
                          Content.scoped.where(contentRoot):
                            body
```

`Principal` is deliberately NOT a `Requested.run` parameter — it was
never set at the top of `servePage` either; it is bound only around
the `render` call in `dispatch`'s `Access.Granted` branch, same as
today, via `Principal.scoped.where(Some(p)) { render(...) }`. The
difference from today: reverting to "no principal" is no longer a
`finally` line that must not be dropped — it is the unwind of the
`where` call, impossible to skip.

**`Site.scala`** — `servePage`'s setup+try+finally becomes one
`Requested.run(...) { ...cookie logic...; dispatch(...); ...caching... }`
call; `dispatch`'s `api.Principal.setCurrent(Some(p))` becomes
`api.Principal.scoped.where(Some(p)) { render(f, web, resp, forwards) }`.

**`Page.scala`**, **`ScalaScript.scala`** — the two bare-render entry
points (`Page.render`, `ScalaScript.render`) wrap their own invoke
call in `api.Web.scoped.where(web) { ... }` instead of
`api.Web.setCurrent(web)` followed by nothing. Strictly better than
today: today's bare path leaves `Web.current` set on that thread
forever after the call returns (acceptable per the existing spec's
"bare render, single-threaded caveat", but still cheap to fix for
free) — `where` restores whatever was there before.

**Tests** — `TestContent.scala`'s three `Content.setRoot(...)` calls
become `Content.scoped.where(...) { ... }`; `TestInlineI18n.scala`'s
five `Lang.setCurrent(...)` become `Lang.scoped.where(...) { ... }`.

## Done when

No public `setCurrent`/`setSecureByDefault`/`setIncluder`/
`setLiveRegistrar`/`setTranslator`/`setIssuer`/`setRoot` remains on
any of the touched objects; `Site.servePage` has no hand-written
`finally` for this state; `scripts/gate.sh "affected master"` green.
