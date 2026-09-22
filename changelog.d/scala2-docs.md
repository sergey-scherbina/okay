## scala2-docs - Scala 2.13 support documented everywhere it belongs, and checked against a real consumer build

The operator asked for detailed documentation of the Scala 2.13 work
in every relevant place.

- `docs/scala2.md`, a new user guide in 12 sections: the build,
  `Prog`, `Eff` (with a handler-order example), failure, your own
  effect (a store handler and a dry-run handler over the same
  program), `Cont`, `Source`, fibers and channels (a worker pool), a
  Scala 3 / Scala 2 phrasebook, every compiler error the setup
  produces with its cause and fix, and what is not there and why.
  Every snippet is verbatim from `TestScala2Guide`, compiled by scalac
  2.13 with `-Werror`. The claims in the prose that have no snippet
  (which exceptions escape, the effect of handler order, the exact
  unhandled-effect message) are asserted there too.
- `docs/theory/13-rows-without-unions.md`: a union of operations is an
  intersection of requirements, contravariance does the membership
  proof, phantom capabilities, the one cast and why it is sound, and
  three facts about scalac 2.13 that decided the rest. Eight papers
  cited. It is linked from the theory index, the map and ch. 10's
  footer.
- typepedia: a `okay.scala2` section, and `Rows.coerce` and
  `Effect.narrow` added to the cast registry. The module page gains an
  API reference with signatures. Pointers added to README (intro,
  Start here, interop), docs/README (Start here; "Thirteen chapters"),
  guide, tutorial §25, your-own-effect (a new closing section),
  ROADMAP (P3), and okay-scala2/README.
- A DEFECT FOUND BY CHECKING THE DOCS: a separate consumer project
  built against a `publishLocal`, using the documented settings,
  compiled and then failed on `sbt run` with
  `NoClassDefFoundError: scala/reflect/Enum`. `run` reads
  `dependencyClasspathAsJars`. The setup now appends the 3.9 stdlib
  there too, the probe runs unforked (46 tests), and the consumer
  runs forked and unforked and passes an unforked `test`.
- Backlog: `published-pom-carries-jmh` (okay_3's pom lists JMH as
  compile dependencies) and `chat-guide-version-drift` (the chat guide
  says 0.1.0-SNAPSHOT; build.sbt says 0.1.1). Both were seen during
  the consumer check.
