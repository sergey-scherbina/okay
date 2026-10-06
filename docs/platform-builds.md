# Building and testing each platform

The root build defaults to JVM: plain `sbt compile` and `sbt test` aggregate
only JVM modules. JS and Native have independent aggregate entry projects
sharing the same source modules, with no dependencies between build roots.

```sh
sbt test
sbt 'project jsBuild' test
sbt 'project nativeBuild' 'set Global / concurrentRestrictions := Seq(Tags.limitAll(1))' test
```

Use the managed entry point for watchdogs, logs and bounded Native tasks:
`sh scripts/build.sh` defaults to JVM.

```sh
sh scripts/build.sh jvm
sh scripts/build.sh js
sh scripts/build.sh native
```

Each command tests the root aggregate's projects for that platform.
JVM includes JVM-only modules whose names have no platform suffix.
Native runs one sbt task at a time, preventing multiple modules' Native
compilers from competing for the same heap. Code generation inside one
module may still use multiple threads.

To compile instead of running tests:

```sh
sh scripts/build.sh jvm compile
sh scripts/build.sh js compile
sh scripts/build.sh native compile
```

For all platforms:

```sh
sh scripts/build.sh all
```

This runs JVM, then JS, then Native in three fresh sbt processes. It stops
at the first failed platform and retains all output in the gate log.
A fresh process releases compiler analyses and classloaders before the
next platform. The whole-build CI lock covers the complete sequence.
The local CI runner and GitHub CI use this managed platform path.

For a change and its dependent modules:

```sh
sh scripts/gate.sh "affected master staged"
```

The same platform separation applies, with changed projects before their
dependents within each platform. Explicit command chains keep one sbt
session so `set` settings remain available to following commands.

Bare `sbt test` now runs JVM only. JS/Native are explicitly selected with
their build projects or the managed commands. Individual project tasks
remain available for scoped development.
The shared gate retains ordinary `test` when run from okay2's separate
build, which does not expose the root build's `family` command.

These changes bound concurrent work; they do not prove that every single
Native compilation fits in the configured heap. An OOM needs its own
log and module identification if it persists in a separated run.
