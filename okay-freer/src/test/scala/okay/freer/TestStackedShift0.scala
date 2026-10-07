package okay.freer
import okay.freer.Shift.Stacked.{dollar, reset, shift, shift0}
import okay.freer.Row.at

/**
 * specs/shift0-dollar.md STAGE 2: shift0 and dollar in `Shift.Stacked`, and the body ROWS that make them sound —
 * since shift-prompt-key each prompt is a key in the row (`Shift % p.type`), so a body is typed at the row in
 * force where it runs: a `shift`'s under its delimiter, a `shift0`'s outside it. Values worked by hand.
 */
class TestStackedShift0 extends munit.FunSuite:

  type P = Pure

  // ---------------------------------------------------------- the hole this lane closed

  test("CLOSED: a shift from a shift's body to the CAPTURED inner prompt is a compile error (it was NoPrompt at run time)") {
    val e = compileErrors("""
      okay.freer.Shift.Stacked.reset[Int, Pure] { outer =>
        okay.freer.Shift.Stacked.reset[Int, okay.freer.Shift % outer.type + Pure] { inner =>
          okay.freer.Shift.Stacked.shift(outer)[Int](k =>
            okay.freer.Shift.Stacked.shift(inner)[Int](k2 => k2(1)).flatMap(k))
        }
      }""")
    assert(e.replaceAll("\\s+", " ").contains("% (inner"), s"compiled, or not naming the captured key: $e")
  }

  test("a shift from a shift's body to a prompt BELOW the captured one resolves and runs: 22") {
    // shift(inner): body under ⟨inner⟩, k = λa. ⟨inner a + 1⟩; the body's
    // shift(outer) captures `flatMap(k)`, the inner end and `* 2`:
    // k2(10) = (10 + 1) * 2
    val r = !.run(reset[Int, P] { outer =>
      type F = Shift % outer.type + P
      reset[Int, F] { inner =>
        shift(inner)[Int](k => shift(outer)[Int](k2 => k2(10)).at[Shift % inner.type + F].flatMap(k)).map(_ + 1)
      }.map(_ * 2)
    })
    assertEquals(r, 22)
  }

  // ---------------------------------------------------------- shift0

  test("shift0: the body runs BELOW the consumed prompt, and a shift from it to the outer one runs: 202") {
    // shift0(inner): k = λa. ⟨inner a + 1⟩, inner consumed; the body's
    // shift(outer) captures `flatMap(k)` and `* 2`: k2(100) = (100 + 1) * 2
    val r = !.run(reset[Int, P] { outer =>
      type F = Shift % outer.type + P
      reset[Int, F] { inner =>
        shift0(inner)[Int](k => shift(outer)[Int](k2 => k2(100)).flatMap(k)).map(_ + 1)
      }.map(_ * 2)
    })
    assertEquals(r, 202)
  }

  test("shift0: a shift to the CONSUMED prompt from its body is a compile error") {
    val e = compileErrors("""
      okay.freer.Shift.Stacked.reset[Int, Pure] { s =>
        okay.freer.Shift.Stacked.shift0(s)[Int](k =>
          okay.freer.Shift.Stacked.shift(s)[Int](k2 => k2(1)).flatMap(k))
      }""")
    assert(e.replaceAll("\\s+", " ").contains("% (s :"), s"compiled, or not naming the consumed key: $e")
  }

  test("shift0 at the root: k resumes, the body answers outside the delimiter") {
    val r = !.run(reset[Int, P] { s =>
      shift0(s)[Int](k => k(1).flatMap(a => k(2).map(b => a + b))).map(_ * 10)
    })
    assertEquals(r, 30)
  }

  test("CONSERVATIVE, pinned: ICFP 2011's example needs k1 where its prompt is gone, and is refused") {
    // "A cat" ++ k1 (k2 "."): k1 was captured under p1, and is called in
    // the body of the shift0 to p1, where p1 is consumed. The paper types
    // it (k1's segment never captures to p1); the row cannot know that,
    // so it asks for p1 and refuses. Sound, not complete.
    val e = compileErrors("""
      okay.freer.Shift.Stacked.reset[String, Pure] { p1 =>
        okay.freer.Shift.Stacked.reset[String, okay.freer.Shift % p1.type + Pure] { p2 =>
          okay.freer.Shift.Stacked.shift0(p2)[String](k1 =>
            okay.freer.Shift.Stacked.shift0(p1)[String](k2 =>
              k2(".").flatMap(k1).map("A cat" + _))).map(" has " + _)
        }.map("Alice" + _)
      }""")
    assert(e.contains("Found:") && e.contains("k1"), s"refused for another reason, or accepted: $e")
  }

  // ---------------------------------------------------------- dollar

  test("dollar, keyed: ret runs outside, k carries it, and R0 differs from R: n=10|n=20") {
    val r = !.run(dollar[Int, String, P](i => pure(s"n=$i")) { d =>
      shift0(d)[Int](k => k(1).flatMap(a => k(2).map(b => s"$a|$b"))).map(_ * 10)
    })
    assertEquals(r, "n=10|n=20")
  }

  test("dollar, keyed: the dollar's prompt is gone after it returns") {
    val e = compileErrors("""
      var leaked: okay.freer.Shift.Stacked.Reset[Int, Pure] | Null = null
      okay.freer.!.run(okay.freer.Shift.Stacked.dollar[Int, Int, Pure](i => okay.freer.pure(i)) { d =>
        leaked = d
        okay.freer.pure[okay.freer.Shift % d.type + Pure, Int](1)
      }.flatMap { _ =>
        val l = leaked.nn
        okay.freer.Shift.Stacked.shift(l)[Int](k => k(1))
      })""")
    assert(e.replaceAll("\\s+", " ").contains("% (l :"), s"compiled, or not naming the escaped key: $e")
  }
