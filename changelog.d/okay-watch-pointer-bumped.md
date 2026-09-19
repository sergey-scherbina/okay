## okay-watch-pointer-bumped - the private consumer follows the arc

okay-watch (private, okay as a submodule) moved its pointer from
9909ce22 — before producer-to-writer-carrier — to 73f459a8, this
repository's pushed tip. Exactly the two seams the backlog entry
predicted needed attention: `feed/Book.scala` drained `Typed.rows`
with `Producer.concat` and now calls `Source.concat` (one line, plus
its comment), and `Backup.scala`'s note on `pure(chunk)` was history
and stayed. Its gate: `sbt test`, 914 tests, 0 failures, 0 warnings;
landed and pushed there as ac66e4a. The entry
`okay-watch-pointer-bump-check` retires with this.

Files: backlog.d/okay-core/okay-watch-pointer-bump-check.md (deleted).
