package okay

/**
 * The half of the core's `TestReplayable` that is about `Uid`, asked
 * where `Uid` lives (core-modules stage 3). It was never actually
 * asked before: the core's test was TITLED "Resource and Uid are
 * refused" and its body only ever summoned the Resource row. Moving
 * `Uid` out is what made the gap visible.
 */
class TestUidReplayable extends munit.FunSuite {

  test("Uid is refused: a fresh id is not a replay") {
    val e = compileErrors("summon[okay.Replayable[okay.Delim + okay.Uid]]")
    assert(e.nonEmpty, "a Uid row was accepted as replayable")
  }
}
