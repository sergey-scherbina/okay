## release-first-wave - the build can publish the core and the main modules to Maven Central

- `project/ReleaseWave.scala` names the first wave: okay, okay-async,
  okay-platform, okay-stream, okay-direct, okay-optics, okay-diagnose,
  okay-test, okay-lex, okay-parse, okay-codec, okay-http, on every
  platform each builds for. Every other module is `publish / skip` and
  keeps `publishLocal`. `releaseWaveCheck` fails when a wave module
  depends at compile scope on one of ours outside the wave; it passes,
  and the wave has no third-party compile dependency beyond the Scala,
  Scala.js and Scala Native runtimes.
- `build.sbt`: SCM, developer, `publishTo` (Central snapshots, or sbt's
  own `localStaging` for `sonaRelease`), and no resolver in the POM (a
  local mirror had leaked into it as `<repositories>`). `sbt-pgp` signs.
- `docs/releasing.md`: the one-time Central account, token and key, and
  the steps of a release. A workflow that publishes on a tag push is left
  to the maintainer and not added.
