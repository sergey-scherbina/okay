## script-scoped-state-followup — doc drift fixed, leak regression test added
Landed: 2026-09-19

Follow-up to script-scoped-state (175a2993): its own doc comment
(Api.scala) and specs/okay-script.md still showed the old
`ThreadLocal`/public-`setCurrent` shape after the migration to
`Scoped[A]`. Fixed the "current API summary" blocks and the
`Page.render` concurrency note; the 2026-09-03 design narrative's own
code sample stays as historical record, with a dated addendum
pointing forward to specs/script-scoped-state.md.

Added a regression test (TestSecure.scala): an admin-granted request
followed by a plain open one on the same calling thread asserts
`Principal.current` reads `None` on the second — the exact fragility
the original migration closed (previously provable only by
construction, now asserted directly).

See specs/script-scoped-state.md, specs/okay-script.md "Site — the
container".
