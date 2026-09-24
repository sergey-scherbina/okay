## scala2-roads - two Scala 2 roads, one vocabulary

The operator's decision on `okay2-one-scala2-story`: `okay2` is okay on
pure Scala 2, with no dependency on Scala 3, and carries less than okay
by default — it grows when somebody needs something specific;
`okay-scala2` is a door from Scala 2 code into the Scala 3 world.
Both stay; the facade is not re-based on okay2; okay2's remaining
stage-2 ports are on demand.

To keep the two as close as is reasonable without adding to okay2, the
facade gained okay's names as forwarders beside its own:
`State.handle`/`set`, `Writer.collect`, `Throws.runEither`,
`Choose.choose`/`runChoice`, `Async(a)`, in okay2's type-argument order.
`TestOkayVocabularyFromScala2` (facade probe) and `TestFacadeVocabulary`
(okay2) hold the same program lines, green on both; only the runner
differs.

Docs: docs/scala2.md section 3a, docs/okay2.md overview;
specs/scala2-facade.md stage 20, specs/okay2.md decision.
