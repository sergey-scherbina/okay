package okay

/**
 * THE DISCIPLINE AS A CONSTRAINT (dialogue-replay-discipline,
 * 2026-09-17). Replay is exact only while everything the outside
 * world tells the program enters through `pause`; until now that was
 * a sentence in a document and a test that measured what breaking it
 * costs. These are the compile errors that make it a rule.
 *
 * The spike that chose the encoding is in the spec's Results: the
 * inductive instance over the row does NOT resolve, and the subtyping
 * form does.
 */
class TestReplayable extends munit.FunSuite {

  test("a replayable row resolves, however deep the union") {
    assert(summon[Replayable[Delim + Pure]] ne null)
    assert(summon[Replayable[Delim + (State % Int + Pure)]] ne null)
    assert(summon[Replayable[State % Int + (Reader % String + (Throws % String + Pure))]] ne null)
  }

  test("a row with Async is refused, and the message states the discipline") {
    val e = compileErrors("summon[okay.Replayable[okay.Delim + okay.Async]]")
    assert(e.nonEmpty, "an async row was accepted as replayable")
    assert(e.contains("PERFORM AGAIN"), s"the message does not say what is wrong: $e")
    assert(e.contains("pause"), s"the message does not name the rule: $e")
    assert(e.contains("unchecked"), s"the message does not name the deliberate way out: $e")
  }

  test("Writer is refused too — replay tells the log again, which is measured") {
    // TestDelimPersist watches exactly that happen, and now has to say
    // `Replayable.unchecked` to be allowed to
    val e = compileErrors(
      "summon[okay.Replayable[okay.Delim + (okay.Writer % String + okay.Pure)]]")
    assert(e.nonEmpty, "a Writer row was accepted as replayable")
  }

  // The title used to say "Resource and Uid", and the body only ever
  // checked Resource — caught when Uid left for okay-data
  // (core-modules stage 3). The Uid half is a real question and is now
  // asked where Uid lives, in okay-data's TestUidReplayable.
  test("Resource is refused: acquiring twice is not a replay") {
    assert(compileErrors("summon[okay.Replayable[okay.Delim + okay.Resource]]").nonEmpty)
  }

  test("an abstract row PROPAGATES the obligation instead of crashing the compiler") {
    // the shape that kills dotty when written as `Row.In` over an
    // abstract row: a property of a row, searched for at an abstract F
    val e = compileErrors("""
      def helper[F[+_]](j: List[Int])(body: okay.Delim.Asking[Int, Int, Int, okay.Delim + F] ?=>
        Int ! okay.Delim + F)(using okay.Delim.OneMachine[F]) =
          okay.Delim.replay[Int, Int, Int, F](body)(j)""")
    assert(e.nonEmpty, "an abstract row satisfied Replayable out of nowhere")
    assert(!e.contains("Failure to join"),
      s"THE COMPILER CRASHED — the encoding is wrong, see specs/durable-workflow.md: $e")

    // declared, it compiles — which is what a library author does
    val ok = compileErrors("""
      def helper[F[+_]](j: List[Int])(body: okay.Delim.Asking[Int, Int, Int, okay.Delim + F] ?=>
        Int ! okay.Delim + F)(using okay.Delim.OneMachine[F],
                                      okay.Replayable[okay.Delim + F], okay.At) =
          okay.Delim.replay[Int, Int, Int, F](body)(j)""")
    assert(ok.isEmpty, s"declaring the obligation did not help: $ok")
  }

  test("`unchecked` is a method, so it cannot be summoned by accident") {
    // it takes writing the name, which is the point: a reviewer sees it
    val ev = Replayable.unchecked[Delim + Async]
    assert(ev ne null)
    val e = compileErrors("summon[okay.Replayable[okay.Delim + okay.Async]]")
    assert(e.nonEmpty, "unchecked leaked into implicit search")
  }
}
