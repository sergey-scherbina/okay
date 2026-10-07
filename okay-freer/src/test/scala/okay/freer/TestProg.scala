package okay.freer

import okay.*
import okay.given

import okay.freer.Shift.Stacked.{abort, reset, shift}
import okay.freer.Row.at

/**
 * specs/freer-base.md, stage 2, and specs/shift-merge.md stage 3 (shift-prompt-key): `Shift`'s prompts keyed
 * by their own singleton types, the prompt stack read off the ROW. The five positive shapes of the probe
 * (scripts/stage2-prompt-identity-probe.scala) run through the REAL machine and answer the shift/reset laws'
 * values; the three shapes that throw `NoPrompt` at run time on the dynamic doors are compile errors here —
 * each leaves a `Shift % q.type` that nothing handles.
 */
class TestProg extends munit.FunSuite:

  type P = okay.Pure

  // ---------------------------------------------------------- positives

  test("1. the simple shape: reset { shift(k => k(5) * 2) } == 10") {
    val r = !.run(reset[Int, P] { p =>
      shift(p)[Int](k => k(5).map(_ * 2))
    })
    assertEquals(r, 10)
  }

  test("2. a for-comprehension") {
    val r = !.run(reset[Int, P] { p =>
      for
        a <- shift(p)[Int](k => k(1))
        b <- shift(p)[Int](k => k(a + 1))
      yield b * 10
    })
    assertEquals(r, 20)
  }

  test("3. an ordinary effect in head position, beside the prompt's key") {
    type F = Reader % Int
    val r = !.run(Reader.run[Int, Int, P](41)(reset[Int, F] { p =>
      for
        a <- Reader.ask[Int].at[Shift % p.type + F]
        b <- shift(p)[Int](k => k(a + 1))
      yield b
    }))
    assertEquals(r, 42)
  }

  test("4. nesting: a shift to the OUTER delimiter from inside the inner one — typed at the outer's row, widened to the inner's") {
    val r = !.run(reset[Int, P] { outer =>
      reset[Int, Shift % outer.type + P] { inner =>
        shift(outer)[Int](k => k(1).map(_ + 100)).at[Shift % inner.type + Shift % outer.type + P]
      }.map(_ + 1)
    })
    // k is the rest up to OUTER: (_ + 1) is inside k, (+ 100) is outside
    assertEquals(r, 102)
  }

  test("5. a reset as a step of a larger program, its value from the head") {
    val r = !.run(reset[Int, P] { s =>
      for
        a <- pure[Shift % s.type + P, Int](5)
        b <- reset[Int, Shift % s.type + P] { s2 =>
               shift(s2)[Int](k => k(a))
             }
      yield b
    })
    assertEquals(r, 5)
  }

  test("abort and multi-shot, keyed: the laws are the machine's, the row only checks") {
    val aborted = !.run(reset[Int, P] { p =>
      abort(p)[Int](7).map(_ + 100)
    })
    assertEquals(aborted, 7)
    val twice = !.run(reset[Int, P] { p =>
      shift(p)[Int](k => k(1).flatMap(a => k(2).map(b => a + b))).map(_ * 10)
    })
    assertEquals(twice, 30)
  }

  // ---------------------------------------------------------- negatives

  test("6. a shift with no reset has no delimiter to name: a capture needs a reset's handle") {
    // the dynamic door's NoPrompt here: with a bare prompt there is nothing to call `shift` on
    val e = compileErrors("""
      { val loose = okay.freer.Shift.prompt[Int]
        okay.freer.Shift.Stacked.shift(loose)[Int](k => k(1)) }""")
    assert(e.nonEmpty, "a shift to a bare prompt compiled")
  }

  test("7. a shift to a FOREIGN delimiter is a compile error: its key is not in the body's row") {
    val e = compileErrors("""
      okay.freer.Shift.Stacked.reset[Int, okay.Pure] { s =>
        var stolen: okay.freer.Shift.Stacked.Reset[Int, okay.Pure] | Null = null
        okay.freer.Shift.Stacked.reset[Int, okay.Pure] { other => stolen = other; okay.freer.pure(1) }
        val t = stolen.nn
        okay.freer.Shift.Stacked.shift(t)[Int](k => k(1))
      }""")
    assert(e.replaceAll("\\s+", " ").contains("% (t :"), s"compiled, or not naming the foreign key: $e")
  }

  test("8. a delimiter that ESCAPES its reset and is shifted to afterwards is a compile error") {
    // after the inner reset returns, the row in force is the OUTER one, and the leaked delimiter's key is not in it
    val e = compileErrors("""
      okay.freer.Shift.Stacked.reset[Int, okay.Pure] { outer =>
        var leaked: okay.freer.Shift.Stacked.Reset[Int, okay.freer.Shift % outer.type + okay.Pure] | Null = null
        okay.freer.Shift.Stacked.reset[Int, okay.freer.Shift % outer.type + okay.Pure] { inner =>
          leaked = inner
          okay.freer.Shift.Stacked.shift(inner)[Int](k => k(1))
        }.flatMap { _ =>
          val l = leaked.nn
          okay.freer.Shift.Stacked.shift(l)[Int](k => k(2))
        }
      }""")
    assert(e.replaceAll("\\s+", " ").contains("% (l :"), s"compiled, or not naming the escaped key: $e")
  }
