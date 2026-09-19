package okay

/**
 * The channel half of `TestErrorMessages`: it moved here with the
 * channels (core-modules stage 1). It names `okay.Channel` inside a
 * `compileErrors` STRING, which is why the compiler could not flag it
 * when the core lost the type and only the gate did.
 */
class TestChannelErrorMessages extends munit.FunSuite {

  test("the sanctioned channel spellings compile (the discard lint itself lives in build.sbt)") {
    // `c.send(1)` in statement position, as a Unit def's body, or
    // eta-expanded into a Unit function is a compile ERROR — a -Wconf
    // escalation in build.sbt, verified with a probe file (the three
    // shapes error, Sim's nested-`!` Queue does not). compileErrors
    // cannot see lints (they are reported after typer), so the
    // negative half is not testable here; the positive half is.
    assertEquals(compileErrors("val c = okay.Channel[Int](); def f(): Unit = c.offer(1): Unit"), "")
    assertEquals(compileErrors("val c = okay.Channel[Int](); val p: Unit ! okay.Async = c.send(1).map(_ => ())"), "")
  }
}
