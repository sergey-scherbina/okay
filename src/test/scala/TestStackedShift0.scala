package okay

import okay.Delim.Stacked.{delimited, dollar, reset, shift, shift0}
import okay.Prog.{flatMap, map}

/**
 * specs/shift0-dollar.md STAGE 2: shift0 and dollar in `Delim.Stacked`,
 * and the body stacks that make them sound. Values worked by hand.
 */
class TestStackedShift0 extends munit.FunSuite:

  type P = okay.Pure

  // ---------------------------------------------------------- the hole this lane closed

  test("CLOSED: a shift from a shift's body to the CAPTURED inner prompt is a compile error (it was NoPrompt at run time)") {
    val e = compileErrors("""
      okay.Delim.Stacked.delimited[Int, okay.Pure] { outer =>
        import outer.given
        okay.Delim.Stacked.reset[Int, okay.Pure] { inner =>
          import inner.given
          okay.Delim.Stacked.shift[Int, Int, okay.Pure](outer.p)(k =>
            okay.Delim.Stacked.shift[Int, Int, okay.Pure](inner.p)(k2 => k2(1)).flatMap(k))
        }
      }""")
    assert(e.contains("not on the prompt stack"), s"compiled, or not our message: $e")
  }

  test("a shift from a shift's body to a prompt BELOW the captured one resolves and runs: 22") {
    // shift(inner): body under ⟨inner⟩, k = λa. ⟨inner a + 1⟩; the body's
    // shift(outer) captures `flatMap(k)`, the inner end and `* 2`:
    // k2(10) = (10 + 1) * 2
    val r = !.run(delimited[Int, P] { outer =>
      import outer.given
      reset[Int, P] { inner =>
        import inner.given
        shift[Int, Int, P](inner.p)(k => shift[Int, Int, P](outer.p)(k2 => k2(10)).flatMap(k)).map(_ + 1)
      }.map(_ * 2)
    })
    assertEquals(r, 22)
  }

  // ---------------------------------------------------------- shift0

  test("shift0: the body runs BELOW the consumed prompt, and a shift from it to the outer one runs: 202") {
    // shift0(inner): k = λa. ⟨inner a + 1⟩, inner consumed; the body's
    // shift(outer) captures `flatMap(k)` and `* 2`: k2(100) = (100 + 1) * 2
    val r = !.run(delimited[Int, P] { outer =>
      import outer.given
      reset[Int, P] { inner =>
        import inner.given
        shift0[Int, Int, P](inner.p)(k => shift[Int, Int, P](outer.p)(k2 => k2(100)).flatMap(k)).map(_ + 1)
      }.map(_ * 2)
    })
    assertEquals(r, 202)
  }

  test("shift0: a shift to the CONSUMED prompt from its body is a compile error") {
    val e = compileErrors("""
      okay.Delim.Stacked.delimited[Int, okay.Pure] { s =>
        import s.given
        okay.Delim.Stacked.shift0[Int, Int, okay.Pure](s.p)(k =>
          okay.Delim.Stacked.shift[Int, Int, okay.Pure](s.p)(k2 => k2(1)).flatMap(k))
      }""")
    assert(e.contains("not on the prompt stack"), s"compiled, or not our message: $e")
  }

  test("shift0 at the root: k resumes, the body answers under the empty stack") {
    val r = !.run(delimited[Int, P] { s =>
      import s.given
      shift0[Int, Int, P](s.p)(k => k(1).flatMap(a => k(2).map(b => a + b))).map(_ * 10)
    })
    assertEquals(r, 30)
  }

  test("CONSERVATIVE, pinned: ICFP 2011's example needs k1 where its prompt is gone, and is refused") {
    // "A cat" ++ k1 (k2 "."): k1 was captured under p1, and is called in
    // the body of the shift0 to p1, where p1 is consumed. The paper types
    // it (k1's segment never captures to p1); the index cannot know that,
    // so it asks for p1 and refuses. Sound, not complete.
    val e = compileErrors("""
      okay.Delim.Stacked.delimited[String, okay.Pure] { p1 =>
        import p1.given
        okay.Delim.Stacked.reset[String, okay.Pure] { p2 =>
          import p2.given
          okay.Delim.Stacked.shift0[String, String, okay.Pure](p2.p)(k1 =>
            okay.Delim.Stacked.shift0[String, String, okay.Pure](p1.p)(k2 =>
              k2(".").flatMap(k1).map("A cat" + _))).map(" has " + _)
        }.map("Alice" + _)
      }""")
    assert(e.contains("Found:") && e.contains("k1"), s"refused for another reason, or accepted: $e")
  }

  // ---------------------------------------------------------- dollar

  test("dollar, stacked: ret runs outside, k carries it, and R0 differs from R: n=10|n=20") {
    val r = !.run(delimited[String, P] { s =>
      import s.given
      dollar[Int, String, P](i => Prog.pure(s"n=$i")) { d =>
        import d.given
        shift0[String, Int, P](d.p)(k => k(1).flatMap(a => k(2).map(b => s"$a|$b"))).map(_ * 10)
      }
    })
    assertEquals(r, "n=10|n=20")
  }

  test("dollar, stacked: the dollar's prompt is gone after it returns") {
    val e = compileErrors("""
      okay.Delim.Stacked.delimited[Int, okay.Pure] { s =>
        import s.given
        var leaked: okay.Prompt[Int] | Null = null
        okay.Delim.Stacked.dollar[Int, Int, okay.Pure](i => okay.Prog.pure(i)) { d =>
          import d.given
          leaked = d.p
          okay.Prog.pure(1)
        }.flatMap(_ => okay.Delim.Stacked.shift[Int, Int, okay.Pure](leaked.nn)(k => k(1)))
      }""")
    assert(e.contains("not on the prompt stack"), s"compiled, or not our message: $e")
  }
