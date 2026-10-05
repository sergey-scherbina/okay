## okay2 Named Module Layout

Commits: dfaeeb904 (migration specification), 488e02e89 (implementation).

JPMS Stage C: 21 own JVM artifacts now have explicit descriptors and
disjoint packages. Core keeps okay2; data, optics (including Plate),
STM (including Tx) and workflow move into their module namespaces.
In-repository consumers and optic macro expansions migrate. External
okay2 callers must update imports and recompile; Scala 3 okay is unchanged.
Artifact coordinates remain unchanged.

The JVM async module uses BlockingDefaults; platform provides its typed
JVM implementation. Both named and classpath discovery work, including
a handoff. The packaged profile resolves as 24 disjoint modules, verifies
core cannot read SQL, links Cats/FS2/ZIO APIs and exercises dependency-free
and optional-interop profiles. Missing optional bundle and conflicting
classes refuse by name. Spark remains excluded. Named modules do not
replace domain bytecode classification or hermetic replay.

Scoped verification: 866 JVM tests, JS/Native Test/compile for all nine
migrated/consumer modules, jpmsCheck, 58 recursion inventory entries;
no compile warnings. Migration and launch guide: okay2/MODULES.md.
