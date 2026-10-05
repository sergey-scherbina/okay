# okay2 Named Module Layout

## Overview

Each okay2 artifact owns disjoint JVM packages. Core keeps okay2; the
definitions currently contributed by data, optics, STM and workflow move
to okay2.data, okay2.optics, okay2.stm and okay2.workflow. The remaining
modules already own distinct packages. JVM jars gain real module-info;
JS and Native retain the same logic under the new source namespaces.

## Compatibility

This is an explicit source/binary namespace migration in the experimental
okay2 implementation, not a change to the published Scala 3 okay API.
Hlc/Uid/Sketch, Optic/Plate/Zipper/TypedZipper, Tx/Stm, Proc/Wf change their imports.
All in-repository clients and macro-generated qualified names migrate.
No alias is installed in another artifact's okay2 package: that would
restore the split package or introduce a core/dependent cycle. Recompile
external okay2 callers and use the module-specific imports.

## Interface

JVM packageBin attaches a descriptor after Scala compilation; normal
classpath execution still works. Module names are okay2.core and the
artifact suffix (okay2.data, okay2.async, etc.). Dependencies exposed in
public APIs use requires transitive; scala-reflect is static/optional.
The core's JDK22 implementation stays inside its existing multi-release
jar, never a second module with the same package.

An explicit JVM-only JPMS gate resolves packaged jars on the module path,
checks package ownership, loads the platform's BlockingDefaults provider
and observes actual readability. The existing import-based default stays
unchanged; service discovery is an additional typed JVM entry point.

Third-party interop dependencies may themselves share packages. They are
optional and packaged as one explicit automatic dependency bundle for
the module-path distribution, not copied into okay2's own modules. Spark
is out of the named profile, as decided in specs/okay-audit.md: its
upstream module graph and required JDK opens are not solved by this lane.

## Behavior

- [x] JVM artifacts own no overlapping package, including macro classes.
- [x] Data/optics/STM/workflow clients compile with the new imports.
- [x] Macro expansion produces the new fully qualified optic names.
- [x] Packaged JVM jars contain module-info and resolve as named modules.
- [x] JS and Native consumers compile after namespace migration.
- [x] Platform capabilities are available through uses/provides without
  changing the existing implicit defaults.
- [x] The module-path probe loads real jars, discovers a working provider
  and verifies core has no SQL readability even when SQL is resolved.
- [x] Missing/conflicting optional dependencies refuse by name rather
  than creating split packages silently.

## Decisions

- Keep core namespace: most modules are already correctly separated;
  moving the entire API would add churn without improving ownership.
- Descriptors are generated after Scala compilation, not mixed into the
  Scala compiler's classpath-oriented Java source compilation.
- The macro implementation jar scala-reflect owns subpackages, while
  scala-library owns scala.reflect itself; their actual package inventories
  do not overlap. They can remain separate automatic dependencies.
- This is module structure, not an effect sandbox. java.base capabilities
  still need the scanner; typed service discovery does not replace effect
  handlers or journaled domain evidence.
- Follow direct bytecode references and their external jar closure, not
  every jar on sbt's compile path: sbt-jmh puts benchmark tooling there.
  The first negative profile check exposed an erroneous JMH requirement
  in optics, so descriptors now exclude tooling the library never calls.
- Interop is optional at module selection. A selected adapter requires
  okay2.externals; removing that bundle refuses module resolution by name.
  A profile without Cats/FS2/ZIO needs no external bundle. Scala reflection
  remains a static requirement for compilation-time macro implementations.
- Use jpmsModuleName, not sbt's moduleName: named-module identity must not
  change artifact coordinates or existing packageBin filenames.

## Results

2026-10-05: 21 own JVM jars, Scala library/reflection and one selected
interop bundle resolve as 24 disjoint modules. BoundaryProbe exercises
named type ownership, core/SQL unreadability, typed service discovery and
a real handoff. The same provider also works on the ordinary classpath.
The interop probe links Cats, FS2 and ZIO API signatures. Missing bundle
and conflicting-class fixtures refuse by name; identical class copies
deduplicate. Both interop and dependency-free named profiles pass with
native access denied and no opens/reads overrides.

The final combined gate passed the nine changed JVM modules (866 tests)
and jpmsCheck without compile warnings.
JS and Native Test/compile passes cover data, optics, STM, workflow,
lex, codec, refine, HTTP and persist plus their dependencies. The 58
recursion inventory entries remain valid. Implementation: 488e02e89.
