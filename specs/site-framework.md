# The site framework — okay-script and okay-ui as a closed, open, orthogonal whole

## Overview

Operator ask (2026-09-18): finish okay-script and okay-ui into a
framework of scalascript's standing, so that the two sites this
operator actually runs — szykownia.pl (an atelier's storefront) and
it.szykownia.pl (an IT consulting line) — could be built HERE.

That ask has a measurable criterion, which is why it is worth taking
literally: both sites are rendered today by busi's declarative-site
system out of `src/v2/http/storefront.ssc`, 818 lines of scalascript.
A page of that file is the acceptance test. If the same storefront
can be written as okay-script pages, with no feature simulated by
hand, the framework is at that level; if it cannot, the missing piece
is named and staged rather than argued about.

## What the two sites actually need

Read out of `storefront.ssc` and its neighbours, in the order the gap
matters:

1. **Modules.** The file begins with a front-matter `exports:` list
   and four import lines written as markdown links —
   `[i18nAttrs, phAttrs](i18n.ssc)`, `[Site, Service](../domain/site.ssc)`,
   `[money, compareMoney](std/money.ssc)`. Definitions cross files.
   okay-script has `include` (which inlines another page's OUTPUT)
   and `declare` (which is per page); a `def` in one page is
   unreachable from another. This is the structural gap and stage 1.
2. **Content as data, edited in the app.** The sites' text lives in a
   store the owner edits at `/sites`, with a FILE able to override any
   key (`fileI18nAttrs(slug, "name.hem", fb)`) and a baked default
   under that. okay-script has `Application` attributes and a
   persisted store, so the pieces exist; the seam does not.
3. **Per-element i18n.** Every translatable element carries
   `data-ru/pl/en/uk` and a few lines of JavaScript swap
   `textContent` on load. okay-script's i18n is per REQUEST (a
   variant file, or `t(key)`) — one language per response. Both are
   legitimate and they answer different questions: a per-request
   language needs no script and is indexable; a per-element one
   switches with no round trip. The sites use the second.
4. **The ordinary furniture of a storefront**: service cards with
   prices, an intake form and an offer form, a courier flow, a RODO
   consent, a portfolio section. Every one of these is a `def`
   returning HTML — which is to say, they are what stage 1 makes
   possible and nothing more.

## Stages

**Stage 1 — modules (`script-modules`).** A page exports names; a page
imports them by a markdown link. The unit of reuse becomes a
definition rather than a rendered fragment.

**Stage 2 — content (`script-content`).** A typed content store with a
file override and a baked default, plus the editor page that writes
it. The three-layer lookup the sites use, made a seam rather than a
convention.

### Stage 2, LANDED 2026-09-18

`okay.script.api.Content`: `read[A](path, default)` answers the file
under the site root or the baked default, `write[A]` puts one there,
`clear` takes it away so what shipped answers again, and `problem`
says whether a file was damaged. Three properties are the design:

- **The default is by-name and is not built when the file answers.**
  A baked default is usually everything a site ships with; building
  it per request to discard it is waste that only shows under load.
- **A damaged file is the DEFAULT, not a 500.** An editor that writes
  half a value must not take the site down; `problem` lets a page say
  so without the visitor seeing an error page.
- **The write is atomic where the filesystem allows it.** The reader
  is the same site serving requests while the editor saves, so half a
  file is a state not worth creating on purpose.

Content cannot leave the site root, reading or writing — the router's
rule, for the router's reason.

The editor is a PAGE, not a feature: `Forms.html[Shop]` renders the
form from the same `Schema` the storefront renders from, `Forms.read`
reads the post back, `Content.write` stores it. The fixture has it at
`/edit`, and the test drives the whole loop — what ships, the form,
an edit, the storefront showing the edited words, a reset.

- [x] with no file the baked default answers; with a file the default
      is not even built
- [x] a write round-trips through the Schema and creates its
      directories; `clear` puts the shipped default back
- [x] a damaged file is the default and is reported in `problem`; an
      empty file is the same without alarm
- [x] content cannot leave the site root, reading or writing
- [x] outside a `Site` there is no root and every read is its default
- [x] the loop, in the storefront: the editor's form comes from the
      Schema, a post changes what the storefront shows, a reset
      restores it

**Stage 3 — per-element i18n (`script-i18n-inline`).** Every language
on the element, the client picking. The per-request road stays; a
page chooses, and the two COMPOSE.

### Stage 3, LANDED 2026-09-18

`okay.script.api.Inline`: `attrs` and `placeholder` carry the
languages, `span`/`element` render one translated element whole,
`switcher` draws the buttons and `script` is the ~20 lines that pick
a language and apply it. The element's own content is the REQUEST's
language, so the page is right before any script runs and right
without one; the attributes only let the visitor change their mind.
The client reads the same `OKAYLANG` cookie the per-request road
writes, so the two roads cannot disagree about what was chosen.

The storefront fixture uses it: the title is one translated element,
the switcher and the script are on the page, and `?lang=uk` renders
Ukrainian text with Polish still on the element.

**The trap this stage removed, measured rather than reasoned:** a
page holds `val langs = Langs.of(...)` in a declare block, and a
`val` there is evaluated once per COMPILE. With `default` a FIELD,
the first visitor's language froze for everyone after them — a
`?lang=en` request answered Ukrainian in the test. `default` is a
method reading `Lang.current` now, so the value is
request-independent and the request-dependent part is a function.
A page that wants one language on purpose passes `pinned`.

- [x] the attributes carry every language given and skip an empty one
- [x] the element renders the request's language as its text, falls
      back to what there is when the request's is missing, and never
      carries a language the page does not offer
- [x] a placeholder gets its own carrier, being an attribute
- [x] the switcher marks the current language; the script offers
      exactly the page's languages and reads the same cookie
- [x] a held `Langs` answers a NEW request's language, which is what
      the freeze above was
- [x] in the storefront: `?lang=uk` renders Ukrainian, `?lang=pl`
      Polish, and both carry the other languages

**Stage 4 — the worked example (`script-storefront`).** The szykownia
storefront as okay-script pages, rendered and asserted. This stage is
the arc's verdict: whatever it cannot express is the next stage.

### Stage 4's verdict (2026-09-18, a slice)

A real slice is ported and passing —
`okay-script/src/test/resources/storefront/`, five library pages
under `lib/` (escaping, the domain, the i18n attribute seam, money,
the cards and the theme) and two storefronts that import them, one
warm-and-clothing, one dark-and-technical, exactly as the two sites
differ. TestStorefront, 3.

What it proves, and the first item was the open question:

- **A TYPE crosses a module boundary.** `[Service, Shop](/lib/domain.md)`
  puts a case class and its companion in scope on the importing page,
  which is what makes every other page ordinary Scala over a domain
  rather than string-slinging. Without it the port would have been a
  rewrite; with it the storefront's own shape survives.
- **One library, two sites.** The same five modules render both,
  parameterised by a value (`clothing`, `accent`) rather than copied
  — the thing the 818-line single file could not do.
- **`route: false` keeps the library off the URL space** while every
  importer renders, and `warm()` compiles the whole tree with no
  page broken.

What the slice did NOT cover, honestly: the intake and offer FORMS
(okay-script has two roads for those already — `Forms.html`/`read`
and the Live app — so this is porting, not building), the courier
flow and the RODO consent (forms again), the portfolio and product
showcase (more cards), and the two things that are genuinely missing
and remain staged: content edited in the app (stage 2) and the
client-side language switch that makes the `data-*` attributes do
something (stage 3).

## Stage 1 — modules

```
---
name: money
exports:
  - money
  - compareMoney
---

[t](/i18n/helpers.md)

```scala declare
def money(cents: Long): String = ...
```
```

- A page (any page) may declare `exports:` in its front matter: the
  names its declare blocks make available to others.
- A page imports with a markdown link whose TEXT is the names and
  whose TARGET is the file: `[money, compareMoney](std/money.md)`.
  Relative to the importing page's directory, or absolute from the
  site root with a leading `/` — `include`'s own rule.
- The imported names are in scope in every block of the importing
  page, declare and body alike.
- A module is compiled ONCE per site load, to a stable object name
  derived from its path; the importing page gets that object's
  output directory on its classpath and an `import` of exactly the
  named members. A name that is imported but not exported is a
  compile error naming both files.
- Cycles are a hard error naming the ring, as `include`'s depth cap
  is: a module graph that cannot be ordered cannot be compiled.
- A module page is still a page: it can be routed and rendered. A
  library that should not be served says `route: false` in its front
  matter (stage 1 adds that flag), and the `i18n/` precedent — a
  directory the router never serves — stays as it is.
- Hot reload: a module's file changing invalidates every page that
  imports it, transitively. The dependency edges are known, because
  the imports were parsed to build them.

### Behavior

- [x] a page imports a `def` from another page and calls it in a
      `${}` marker, in a body block and in its own declare block
- [x] the import is by NAME: an exported name not asked for is not in
      scope, and asking for a name the module does not export is a
      compile error naming the module and the name
- [x] a relative target resolves against the importing page's
      directory; a leading `/` resolves from the site root
- [x] a module imports a module: the chain compiles in dependency
      order, and a diamond compiles the shared module ONCE
- [x] a cycle is refused with a message naming the ring, not a stack
      overflow
- [x] `route: false` makes a module unroutable: a GET of its path is
      a 404 while an importing page still renders
- [x] editing a module re-renders the pages that import it (the
      dependents are invalidated, not just the file that changed)
- [x] a module's compile ERROR is reported against the MODULE's own
      file and line, while the importing page says which import
      failed
- [x] an import line is a DEPENDENCY and never reaches the output,
      while an ordinary markdown link inside a SENTENCE stays prose —
      found by RUNNING the storefront, because every test until then
      asserted what a page contains and none what it does not

## The arc's verdict (2026-09-18)

All four stages are landed, and the criterion the arc set itself —
"could the two sites be built here" — is answered for what those
sites DO: modules carry definitions and types across files, one
library renders both storefronts parameterised by a value, every
language rides on the element with the client picking and the server
still rendering one, and the words are content a person edits through
a form generated from the same Schema the page renders from.

What is honestly NOT claimed: the 818-line file has more in it than
the slice ported here — the intake and offer forms, the courier flow,
the RODO consent, the portfolio and product showcase. Every one of
those is a `def` over the domain or a form, which is to say more of
what stage 1 and okay-script's existing form roads already do. They
are porting work, not missing machinery, and until someone does that
porting this sentence is the honest statement of where the line is.

Distribution is the one piece of scalascript's own library story left
out on purpose: `.ssclib` packages, manifests, transitive versioned
deps. A site's own files answer composition, which is what these
sites needed; a SECOND site wanting the first's library is when that
stage earns itself.

## Decisions

- **A module is a page, not a new kind of file.** scalascript's
  `.ssc` is one format for both, and the reasons carry: one parser,
  one front matter, one line map, and a library that wants to
  document itself renders. `route: false` is the only addition, and
  it exists because a library usually should not answer a URL.
- **Imports are markdown links, as scalascript writes them.** They
  read as prose, they survive a markdown renderer, and the file is
  still a document. The alternative — a front-matter `imports:` list
  — puts the dependency where a reader is not looking.
- **Names, not wildcards.** `[money](std/money.md)` says what crosses
  the boundary; a wildcard would make a module's every private helper
  part of its surface by accident.
- **No versions, no manifests, no packages in stage 1.**
  scalascript's `.ssclib` (a ZIP with a manifest and transitive
  deps) answers DISTRIBUTION; a site's own files answer composition.
  This stage is composition. Distribution is a later stage if a
  second site wants the first's library.

## Results

Stage 1 LANDED 2026-09-18. `Modules.scala` (200 lines) parses the
front matter and the import links, resolves targets, orders the
graph and compiles each module once; `ScalaScript.compileAs` gained
the three parameters a module needs (its object name, its package,
the import lines above it) and `compileRender`/`compileModule` are
its two callers. `Page` keys its cache on `(mtime, module stamp)`,
which is how editing a module re-renders its importers without a
dependents map. `Site` owns one `Loader` and refuses to route a
`route: false` page. TestModules, 8; the okay-script suite passes
unchanged.

Two things the tests found, both the kind a green first run would
have hidden:

- **A module's classpath must be TRANSITIVE.** An importer that
  carried only the modules it names compiles — and then fails at run
  time with `NoClassDefFoundError`, because a module's class
  references the classes of the modules IT imports. The diamond test
  is what caught it, one level deeper than the obvious case.
- **An unexported name is caught HERE, not by the compiler.** Asking
  for a name the module does not export says which file exports what,
  where dotty would have said only "value secret is not a member of
  M_lib_money_md_f99ca5cf" — a name no author wrote.
