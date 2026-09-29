# Releasing

How a version of okay reaches Maven Central. The build side is done:
coordinates, POM metadata, signing and the Central Portal upload are in
`build.sbt`, `project/plugins.sbt` and `project/ReleaseWave.scala`. What is
left is account setup, done once, and a short procedure per release.

## What is published

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
