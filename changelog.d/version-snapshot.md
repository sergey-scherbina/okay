## version-snapshot - the build is 0.2.0-SNAPSHOT, and a gate test keeps it from claiming a release it is not

The operator noticed that the docs told users to depend on `0.1.1`
("разве не 0.2.0-SNAPSHOT?"). The cause was in the build: `v0.1.1` was
tagged and pushed on 2026-09-14, and master went on saying
`ThisBuild / version := "0.1.1"` for 1387 commits. So every
`publishLocal` from master produced artifacts named like the release
that were not the release.

- `ThisBuild / version := "0.2.0-SNAPSHOT"`. The next version is a
  minor one, because the scheme is `early-semver` and 0.x has had
  breaking changes since 0.1.1 (`Async` left the core, among others).
- `TestVersionIsNotAReleasedTag` (okay-deploy) turns the forgotten step
  into a check. A version without `-SNAPSHOT` must be the tagged commit
  itself, and a `-SNAPSHOT` must not be of a version already tagged.
  Watched failing on `0.1.1` first ("version 0.1.1 is the release
  v0.1.1, but HEAD is not that commit"), then green. Where no tags are
  visible, it skips.
- The docs that quote the version were re-run, not just re-typed.
  `publishLocal`: about a minute, 138 modules. The chat guide's app:
  tests 2 + 2, `fastLinkJS`, the three `curl`s answered, and `app.js`
  is 1 605 800 bytes. The Scala 2 consumer: `run` and an unforked
  `test`. The SNAPSHOT caching note is back where it applies.
- A scaladoc warning surfaced by okay-spring's first `publishLocal`
  (it joined the aggregate earlier today) is escaped (`\$Bind`).
