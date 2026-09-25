# Scoped as a Multi-Release JAR (script-scoped-state-mrjar, 2026-09-19)

**Since moved to core (scoped-to-core, 2026-09-19), then to
okay-platform (scoped-cross-platform):** every
`okay.script.api.Scoped` / `okay-script/jdk25/...` /
`okayScript` settings reference below is what this looked like on
first landing, in `okay-script`. The class is now `okay.Scoped`
(`okay-platform/src/main/scala/Scoped.scala`), its JDK25 variant is
`jdk25/Scoped.scala` (repo root). **The script, the "if the directory
exists" packaging and the manual probe below are HISTORY since
mrjar-jdk25-ci-gap (2026-09-25, the closing section at the end):**
the variant is an sbt project, packaged always, and proved by a test
in the gate on both sides of 25.

`Scoped[A]` (specs/script-scoped-state.md) was built with a thin
public facade -- `current`, `where` -- specifically so its backend
could change without touching any of the 13 call sites. This ships
that swap: on JDK 25+ (JEP 506 finalized `java.lang.ScopedValue`),
load a `ScopedValue`-backed implementation instead of the
`ThreadLocal` one, automatically, via the JVM's own Multi-Release
JAR mechanism (JEP 238) -- one artifact, no runtime `if` anywhere.

## Why this needs a genuinely separate compile

`java.lang.ScopedValue` does not exist in JDK 21's own runtime
classes. Scala's (and Java's) `-release N` flag restricts a compiler
to an OLDER API surface than the JVM it runs on -- it cannot grant
access to a NEWER one. There is no flag that makes a compiler running
on a JDK 21 JVM see a JDK 25 class. The only way to compile code that
references `ScopedValue` is to run the compiler itself on a JDK 25 (or
newer) JVM.

Proven this session (scratch, not committed): resolved the
`scala3-compiler_3:3.9.0` classpath with `cs fetch --classpath`
(Coursier, already on this machine), then ran

```
$JDK25/bin/java -cp $CP dotty.tools.dotc.Main -classpath $CP -d out Scoped.scala
```

using a Temurin 25.0.4.1 JVM fetched from Adoptium's API and installed
at `~/.sdkman/candidates/java/25.0.4.1-tem` this session (this box
only had JDK 21 before). `javap` on the result:

```
public final class okay.script.api.Scoped<A> {
  public okay.script.api.Scoped(scala.Function0<A>);
  public A current();
  public <B> B where(A, scala.Function0<B>);
  public java.lang.String backend();
  public static <A> okay.script.api.Scoped<A> apply(scala.Function0<A>);
}
```

Identical public descriptors to the existing (JDK21) class, modulo
the diagnostic `backend()` method added to both variants for
verification (below). This is the binary contract Multi-Release JARs
depend on: whichever variant the JVM loads, every OTHER class in
`okay-script` -- compiled exactly once, against the base variant --
links against the same shapes either way.

sbt cannot be asked to run part of one session's compile on a
different JDK than the session itself is running on -- that is a
different JVM process, not a setting. So this is not a normal sbt
sub-project wired into the dependency graph; it is a standalone
compile step, run by a shell script, whose OUTPUT an existing sbt
task picks up if present.

## Shape

**`okay-script/jdk25/Scoped.scala`** -- the `ScopedValue` variant,
`package okay.script.api`, living OUTSIDE `okayScript`'s own source
sets (`src/main/scala/...`) so the ordinary JDK21 build never tries
to compile it and never needs JDK 25 to exist:

```scala
package okay.script.api

final class Scoped[A] private (default: () => A):
  private val sv: ScopedValue[A] = ScopedValue.newInstance[A]()

  def current: A = if sv.isBound() then sv.get() else default()

  def where[B](value: A)(body: => B): B =
    ScopedValue.where(sv, value).call(() => body)

  private[script] def backend: String = "ScopedValue"

object Scoped:
  def apply[A](default: => A): Scoped[A] = new Scoped(() => default)
```

`ScopedValue.where(key, value).call(op)` already IS a `where`-shaped
combinator -- binds for `op`'s extent, restores on any exit including
an exception, nests correctly (nearest-wins) -- so this is not an
adaptation, it is the native primitive the article that started this
whole thread was describing. The one behavioral seam: raw
`ScopedValue.get()` throws when unbound; `current` covers that with
`isBound()` first, so a `Web.current` called completely outside any
`Requested.run`/`Page.render`/`ScalaScript.render` still answers its
default, exactly like the JDK21 path.

**`okay-script/src/main/scala/okay/script/api/Scoped.scala`** (the
existing base) gains only the matching diagnostic:

```scala
private[script] def backend: String = "ThreadLocal"
```

Nothing else in the base changes.

**`scripts/build-mrjar-jdk25.sh`** -- locates a JDK 25+ toolchain
(`OKAY_JDK25_HOME` env var, else scans
`~/.sdkman/candidates/java/*` for the highest `25.*`/`2[6-9].*`
directory), resolves the compiler classpath with
`cs fetch --classpath org.scala-lang:scala3-compiler_3:3.9.0`,
compiles `okay-script/jdk25/Scoped.scala` with that JDK's `java`
binary running `dotty.tools.dotc.Main`, writes classfiles to
`okay-script/jdk25/target/classes`. **Exits 0 and explains itself if
no JDK 25 is found** -- this must never fail a build that has not
opted in.

**`build.sbt`** (`okayScript` settings) -- `Compile / packageBin /
mappings` appends every `.class` under
`okay-script/jdk25/target/classes`, IF that directory exists, mapped
to `META-INF/versions/25/<same relative path>`; `packageOptions`
adds `Multi-Release: true` to the manifest in that same case. A
machine that never ran the script -- every other agent today, CI as
it stands -- packages the exact byte-identical jar it always has.
This is additive infrastructure, not a new hard dependency.

## Verification -- and its structural gap

`scripts/gate.sh` runs on this box's own JDK, which is 21. It cannot
exercise the JDK 25 path at all, by construction -- a green gate here
proves the base path is unchanged, nothing about the MR variant.

So the actual proof is a manual probe, run once by hand this session
and recorded here rather than wired into CI (there is no JDK 25 in
the gate's environment to wire it into): package `okayScript`'s jar,
run a one-line probe class reading `Scoped.backend` against that SAME
jar under both
`~/.sdkman/candidates/java/21.0.12-tem/bin/java` and
`~/.sdkman/candidates/java/25.0.4.1-tem/bin/java`, and confirm
`"ThreadLocal"` / `"ScopedValue"` respectively.

**Known gap, stated plainly**: nothing currently re-runs this probe
automatically. A future change to `okay-script/jdk25/Scoped.scala`
that broke the JDK25 path would go unnoticed by the gate. Closing
that needs either a JDK25 runner in CI or a project decision to make
JDK 25 a build prerequisite everywhere -- out of scope here; recorded
as a BACKLOG follow-up rather than silently accepted.

## Closed: the variant is built by the build (mrjar-jdk25-ci-gap, 2026-09-25)

The operator, on being told a published jar carried the variant only
when whoever built it had run the script: "это нужно исправить". The
gap the backlog item recorded had two halves, and both are gone.

**Why the script existed, and why it no longer can.** dotc ran on JDK
21 and `java.lang.ScopedValue` was not in that JVM's class library; no
`-release` flag grants a compiler an API newer than the JVM it runs
on. Since java-gatherers (2026-09-23) sbt runs on JDK 25
(`.sdkmanrc`), so the compiler sees the API and the variant is an
ordinary source.

**What replaced it (build.sbt `versioned` / `multiRelease`):**
- `jdk25/` is project `okayPlatformJdk25`: sources compiled with
  `-java-output-version 25` (the API check and bytecode 69 for that
  version) against okay-platform's own compile classpath — the root
  `okay.Scoped` is on it and the source wins, as measured. It has no
  `dependsOn`; okay-platform depends on it `test->compile`, which is
  both the truth (the jar its tests run against carries it) and what
  lets `affected` reach okay-platform from a change under `jdk25/`.
- okay-platform's `packageBin` maps every class of it to
  `META-INF/versions/25/` and the manifest says `Multi-Release: true`
  — unconditionally. The core's copy of that wiring, which had gone
  on packaging a variant into a jar whose root had no `Scoped` since
  the class moved, is deleted.
- okay-platform's tests are FORKED now and run against the packaged
  JAR: `Test / fullClasspath` puts it first and drops both classes
  directories (the host's, which would shadow the jar, and the
  variant's, whose root-path `okay/Scoped.class` would have made
  every JVM load the 25 class). A class loaded from a directory is
  never versioned, so without this a green test proved nothing.
- `TestScopedBackend` asserts that `Scoped` came from a jar, and that
  the backend is the one the running JDK must have picked; it prints
  the JDK so the gate log says which JVM decided.

**Measured 2026-09-25**, both through `scripts/gate.sh`: on the
default `Test / javaHome` (26) — `JDK 26.0.2.1 loaded the ScopedValue
backend`; with `okayPlatform.jvm / Test / javaHome` set to 17.0.19 —
`JDK 17.0.19+10 loaded the ThreadLocal backend`. The jar listing:
`META-INF/versions/25/okay/Scoped.class`, `Scoped$.class`, and
`Multi-Release: true`.

`scripts/build-mrjar-jdk25.sh` is deleted. The same two helpers are
how cont-stack (specs/cont-stack.md, Decision 12) adds a `jdk22/`
variant to the CORE: the FFM stack reader, compiled with
`-java-output-version 22`, in every jar, tested through it.
