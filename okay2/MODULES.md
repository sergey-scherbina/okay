# Named JVM Modules

okay2 JVM artifacts contain explicit `module-info.class` descriptors.
The core module is `okay2.core`; other module names replace the artifact's
hyphens with dots, for example `okay2.optics` and `okay2.platform`.
Normal classpath applications keep working.

## Namespace Migration

Recompile callers after this experimental okay2 source/binary migration.
The published Scala 3 okay API is unchanged.

| Definitions | Previous package | New package |
| --- | --- | --- |
| Hlc, Uid, Sketch | okay2 | okay2.data |
| Optic, OpticMacros, Plate, Zipper, TypedZipper | okay2 | okay2.optics |
| Tx, Stm | okay2 | okay2.stm |
| Proc, Wf | okay2 | okay2.workflow |

Core still owns `okay2`. No compatibility aliases are installed in another
artifact's core package, because they would restore split packages.
Macro expansions use the new fully qualified names. JS and Native use
the same migrated namespaces but do not use JVM descriptors.

## Module-Path Profile

From the `okay2` build directory, run `../scripts/gate.sh "jpmsCheck"`.
This packages the own JVM modules except Spark and assembles a verified
module path under `target/jpms/lib`. It checks package ownership, resolves
the real jars, discovers the platform service and exercises its handoff.
It verifies that core cannot read SQL even when SQL is resolved.

Scala's library and optional reflection jars keep their upstream automatic
module names. Selected Cats, FS2 and ZIO third-party jars are combined in
the optional automatic `okay2.externals` jar: upstream jars can overlap
packages and cannot simply be placed alongside each other on a module path.
Different definitions of the same class refuse assembly by class and jar
name; identical copies deduplicate, and service registrations merge.
Use this bundle, not its component jars, on that module path. Original
classpath dependency resolution is unchanged.
Interop modules are optional at profile selection. When an adapter module
is selected, its `okay2.externals` requirement is mandatory: removing the
bundle refuses resolution by name. A profile without adapters needs no
third-party bundle. JMH tooling is not treated as a library dependency;
the generator follows direct bytecode references and their jar closure.

`BlockingProviders.discover()` is a JVM-only typed discovery entry point.
The async module declares `uses BlockingDefaults`; platform declares
`provides BlockingDefaults` with its JVM provider. Existing explicit
platform imports and implicit defaults do not change.

The JDK22 core implementation remains in the core's multi-release jar.
Spark is excluded: its upstream module graph and required JDK opens are
not addressed here. Named modules are access boundaries, not an effect
sandbox: `java.base` capabilities still require the bytecode audit and
domain/handler classification.
