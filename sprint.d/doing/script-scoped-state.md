- [ ] script-scoped-state — replace the ThreadLocal-with-public-setter
      pattern in okay-script/src/main/scala/okay/script/api/{Api,
      Application,Content}.scala with a `Scoped[A]` combinator (no
      public `set`, install-for-a-block-only, auto-restore even on
      exception) — the Scala/JDK21 answer to the Habr article's
      `ScopedValue` pitch (java.lang.ScopedValue is still preview on
      JDK 21; JEP 506 finalizes it only in JDK 25).

      WHY: Site.servePage (Site.scala ~430-480) sets ~12 ThreadLocals
      by hand before `dispatch` and resets 11 of them in a matching
      `finally` — already drifted (Content.setRoot/clearProblems
      aren't in the finally list, harmless only because every request
      re-sets them before reading). Principal specifically is NEVER
      set at the top of servePage, only reset to None in the finally
      — so a pooled thread's PREVIOUS request is the only thing
      keeping this request from inheriting a stale Principal if that
      one finally line is ever dropped or reordered. That is exactly
      the risk the article's ScopedValue section warns about.

      NOT context functions (`?=>`/`provide`/`wire`): tried and
      rejected for this exact subsystem already, empirically —
      specs/okay-script.md "Metadata as context" (okay-script-meta,
      2026-09-03) has the measured reasons (a re-declared `given`
      doesn't shadow the way `val` does; a `given` is memoized once,
      not per read) and lands on "a plain always-fresh method + a
      mutable var". `Scoped[A]` keeps that shape — `current` stays a
      plain method callable from anywhere in a flat-compiled script
      body — and only closes the mutation hole.

      HOW: see specs/script-scoped-state.md for the full design
      (`Scoped[A]`'s API, the touched-object list, `api.Requested.run`
      that collapses servePage's setup+finally into one call). Files:
      new okay-script/src/main/scala/okay/script/api/Scoped.scala;
      Api.scala (Web, Response x2, Session, Error, Container x4,
      Principal, Lang), Application.scala, Content.scala (roots only
      — `problems` already has no public setter, out of scope);
      call sites Site.scala (servePage, dispatch's Principal.setCurrent),
      Page.scala, ScalaScript.scala; tests TestContent.scala,
      TestInlineI18n.scala switch their direct `setX` calls to
      `X.where(v) { ... }`.

      DONE WHEN: no public `setCurrent`/`setSecureByDefault`/
      `setIncluder`/`setLiveRegistrar`/`setTranslator`/`setIssuer`/
      `setRoot` remains on any of these objects; Site.servePage's
      finally block is gone (replaced by `Requested.run`'s nesting);
      `scripts/gate.sh "affected master"` green.
