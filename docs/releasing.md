# Releasing

How a version of okay reaches Maven Central. The build side is done:
coordinates, POM metadata, signing and the Central Portal upload are in
`build.sbt`, `project/plugins.sbt` and `project/ReleaseWave.scala`. What is
left is account setup, done once, and a short procedure per release.

## What is published

Two places can serve it: Maven Central (below) and a Maven repository on
this repository's GitHub Pages (the next section). The build is the same
for both.

The **first wave**, named in `project/ReleaseWave.scala`: `okay`,
`okay-async`, `okay-platform`, `okay-stream`, `okay-direct`,
`okay-optics`, `okay-diagnose`, `okay-test`, `okay-lex`, `okay-parse`,
`okay-codec` and `okay-http`, each for every platform it builds for (JVM,
Scala.js, Scala Native). Every other module is skipped by `publish`,
`publishSigned` and the upload. It keeps `publishLocal`, which the guides
use to build apps against the whole family.

The wave has to be closed: a published module may depend at compile
scope only on published modules. `releaseWaveCheck` fails otherwise, and
it lists any third-party compile dependency a wave module has. Today
there is none beyond Scala's, Scala.js's and Scala Native's own runtime.

Coordinates: groupId `io.github.sergey-scherbina`, artifact `okay` for the
core, so a user writes:

```
libraryDependencies += "io.github.sergey-scherbina" %% "okay" % "0.2.0"
```

The groupId is decided in specs/modules-infra.md, "Publishing". Central
verifies an `io.github.<user>` namespace through the GitHub account, so
no domain is needed. `dev.okay` stays possible later, at the price of a
relocation POM per artifact once something is public.

## GitHub Pages: a Maven repository without Central

The same wave can be served as a Maven repository from the `gh-pages`
branch of this repository. No account, token or signing key is needed, on
either side. A user adds one resolver line:

```
resolvers += "okay" at "https://sergey-scherbina.github.io/okay/maven"
libraryDependencies += "io.github.sergey-scherbina" %% "okay" % "0.2.0"
```

`scripts/pages-publish.sh` builds it:

1. It keeps a worktree of `gh-pages` at `.work/pages/`, and makes the
   branch on the first run if origin has none.
2. It runs `releaseWaveCheck` and `publish` with `OKAY_PAGES_REPO`
   pointing at that worktree's `maven/` folder, so the jars, sources,
   javadoc, POMs and checksums land there in Maven layout.
3. It writes an `index.html` naming each artifact and its versions, and
   commits on `gh-pages`.
4. It pushes only with `--push`. Without it, look first:

```
sh scripts/pages-publish.sh
git -C .work/pages show --stat
sh scripts/pages-publish.sh --push
```

Two refusals are deliberate. A release already in the repository is never
overwritten: move the version on. A `-SNAPSHOT` version is refused unless
`--snapshot` is passed, because each publish is a commit of about 64 MB of
jars and the branch keeps every one.

Once, in the repository's Settings, Pages: serve the `gh-pages` branch
from its root. The URL above works a minute or two after the first push.

Verified before the first push (2026-09-29): the repository built this way
and served over plain HTTP was resolved by a separate sbt project that saw
only that server and a Maven Central mirror. It compiled the tutorial's
State example against `okay` and printed `(40,42)`.

## Once: accounts and keys

1. Sign in at central.sonatype.com with the GitHub account
   `sergey-scherbina`. The namespace `io.github.sergey-scherbina` is
   verified by that sign-in.
2. In the Portal, generate a user token (Account, "Generate User Token").
   It is a username and password pair for uploads, not the login.
3. Make a signing key and publish its public half:

```
gpg --full-generate-key
gpg --list-secret-keys --keyid-format LONG
gpg --keyserver keyserver.ubuntu.com --send-keys <KEY-ID>
```

4. Give sbt the token. Either export it where the release runs, or keep
   it in `~/.sbt/1.0/credentials.sbt` outside the repository:

```
export SONATYPE_USERNAME=<token username>
export SONATYPE_PASSWORD=<token password>
export PGP_PASSPHRASE=<key passphrase>
```

## Each release

1. **The tree is green.** The whole family, on every platform, through
   the gate (`scripts/gate.sh`, or the ci-runner's `family all`). A
   release does not re-run tests; it trusts this.
2. **Set the version.** `ThisBuild / version` in `build.sbt` goes from
   `0.2.0-SNAPSHOT` to `0.2.0`. Commit that alone.
3. **Check the wave and the POMs:**

```
scripts/gate.sh "releaseWaveCheck; publishLocal"
```

4. **Sign, stage and upload.** From that commit, on a machine holding the
   key and the token:

```
sbt "releaseWaveCheck" "publishSigned" "sonaRelease"
```

   `publishSigned` stages the signed wave under `target/sona-staging`,
   and `sonaRelease` uploads it to the Portal and publishes it. Use
   `sonaUpload` instead to upload and review the deployment in the Portal
   before pressing Publish there. The first time, that is the safer
   choice.
5. **Tag the commit** `v0.2.0` and push the tag.
6. **Move to the next snapshot** in the same push: `0.3.0-SNAPSHOT`.
   `TestVersionIsNotAReleasedTag` fails the gate when this is forgotten.

Central takes from a few minutes to about an hour to show a new version.

## Automating it

A workflow that runs step 4 on a pushed tag needs the key and the token
as repository secrets. It is not in the repository yet: whether a tag
push alone may publish is the maintainer's decision, not a default.

## Not yet

- **Binary compatibility.** The scheme is `early-semver`, so `0.2.0` may
  break `0.1.x`, and does. After `0.2.0` is out, MiMa against it keeps
  `0.2.x` from breaking silently.
- **The rest of the family.** A module joins a later wave by being named
  in `project/ReleaseWave.scala`, with `releaseWaveCheck` green.
