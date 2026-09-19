## wroclaw-feed-shadowed-fix — compare/OkayLane.scala's shadowed Feed
Landed: 2026-09-19

`OkayLane.scala`'s `import okay.*` was winning over the same-package
`okay.wroclaw.Feed` (Gtfs.scala), resolving to core's
`type Feed[W] = Unit ! Writer % W` instead — 30 cascading errors.
Fixed: `import okay.{Feed as _, *}` excludes the one name from the
wildcard.

Found while verifying a Spark JDK25 upgrade (okaySpark depends on
`compare` for its own test compile). See compare/BUGS.md.
