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
Hlc/Uid/Sketch, Optic/Zipper/TypedZipper, Stm, Proc/Wf change their imports.
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

- [ ] JVM artifacts own no overlapping package, including macro classes.
- [ ] Data/optics/STM/workflow clients compile with the new imports.
- [ ] Macro expansion produces the new fully qualified optic names.
- [ ] Packaged JVM jars contain module-info and resolve as named modules.
- [ ] JS and Native consumers compile after namespace migration.
- [ ] Platform capabilities are available through uses/provides without
  changing the existing implicit defaults.
- [ ] The module-path probe loads real jars, discovers a working provider
  and verifies core has no SQL readability even when SQL is resolved.
- [ ] Missing/conflicting optional dependencies refuse by name rather
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

## Results

Pending implementation and scoped verification.
