package okay2

import Delim.Stacked

/**
 * The prompt stack in the type: the positive shapes run through the
 * REAL machine and answer the shift/reset laws' values; the three
 * shapes that throw `NoPrompt` at run time on the unstacked doors are
 * compile errors here.
 */
class TestDelimStacked extends munit.FunSuite {

  type P = Pure

  // ---------------------------------------------------------- positives

  test("1. the simple shape: reset { shift(k => k(5) * 2) } == 10") {
    val r = !.run(Stacked.delimited[Int, P] { s =>
      s.stack.shift[Int, Int, P](s.p)(k => k(5).map(_ * 2))
    })
    assertEquals(r, 10)
  }

  test("2. a for-comprehension over the stack's doors") {
    val r = !.run(Stacked.delimited[Int, P] { s =>
      for {
        a <- s.stack.shift[Int, Int, P](s.p)(k => k(1))
        b <- s.stack.shift[Int, Int, P](s.p)(k => k(a + 1))
      } yield b * 10
    })
    assertEquals(r, 20)
  }

  test("3. an ordinary effect in head position, beside the stack") {
    type F = Reader[Int]
    val r = !.run(Reader.run(41)(Stacked.delimited[Int, F] { s =>
      for {
        a <- Reader.ask[Int].at[Delim + F]
        b <- s.stack.shift[Int, Int, F](s.p)(k => k(a + 1))
      } yield b
    }))
    assertEquals(r, 42)
  }

  test("4. nesting: a shift to the OUTER prompt from inside the inner one — Has.there") {
    val r = !.run(Stacked.delimited[Int, P] { outer =>
      outer.stack.reset[Int, P] { inner =>
        // the outer prompt is second on the inner stack: `there` finds it below `here`
        inner.stack.shift[Int, Int, P](outer.p)(k => k(1).map(_ + 100))
      }.map(_ + 1)
    })
    // k is the rest up to OUTER: (_ + 1) is inside k, (+ 100) is outside
    assertEquals(r, 102)
  }

  test("5. a reset as a step of a larger program, its value from the head") {
    val r = !.run(Stacked.delimited[Int, P] { s =>
      for {
        a <- pure[Delim + P, Int](5)
        b <- s.stack.reset[Int, P] { s2 =>
               s2.stack.shift[Int, Int, P](s2.p)(k => k(a))
             }
      } yield b
    })
    assertEquals(r, 5)
  }

  test("abort and multi-shot, stacked: the laws are the machine's, the stack only checks") {
    val aborted = !.run(Stacked.delimited[Int, P] { s =>
      s.stack.abort[Int, Int, P](s.p)(7).map(_ + 100)
    })
    assertEquals(aborted, 7)
    val twice = !.run(Stacked.delimited[Int, P] { s =>
      s.stack.shift[Int, Int, P](s.p)(k => k(1).flatMap(a => k(2).map(b => a + b))).map(_ * 10)
    })
    assertEquals(twice, 30)
    val bare = !.run(Stacked.delimited[Int, P] { s =>
      s.stack.control[Int, Int, P](s.p)(k => k(1)).map(_ + 10)
    })
    assertEquals(bare, 11)
  }

  // ---------------------------------------------------------- negatives

  test("6. a shift with NO reset is a compile error: there is no stack to call it on") {
    val e = compileErrors("""
      { val loose = okay2.Delim.prompt[Int]
        okay2.Delim.Stacked.shift[Int, Int, okay2.Pure](loose)(k => k(1)) }""")
    assert(e.nonEmpty, "a shift with no reset compiled")
    val e2 = compileErrors("new okay2.Delim.Stacked.Stack[okay2.Delim.Stacked.Empty]()")
    assert(e2.nonEmpty, "a stack was constructible outside the object")
  }

  test("7. a shift to a FOREIGN prompt of the same answer type is a compile error") {
    val e = compileErrors("""
      okay2.Delim.Stacked.delimited[Int, okay2.Pure] { s =>
        val stolen = okay2.Delim.prompt[Int]
        s.stack.shift[Int, Int, okay2.Pure](stolen)(k => k(1))
      }""")
    assert(e.nonEmpty, "a shift to a foreign prompt compiled")
    assert(e.contains("not on the prompt stack"), s"the message is not ours: $e")
  }

  test("8. a prompt that ESCAPES its reset and is shifted to afterwards is a compile error") {
    // the run-time NoPrompt this exists for: after the inner reset
    // returns, the stack in force is the OUTER one, and the leaked
    // prompt is not on it
    val e = compileErrors("""
      okay2.Delim.Stacked.delimited[Int, okay2.Pure] { outer =>
        var leaked: okay2.Prompt[Int] = null
        outer.stack.reset[Int, okay2.Pure] { inner =>
          leaked = inner.p
          inner.stack.shift[Int, Int, okay2.Pure](inner.p)(k => k(1))
        }.flatMap { _ =>
          val l = leaked
          outer.stack.shift[Int, Int, okay2.Pure](l)(k => k(2))
        }
      }""")
    assert(e.nonEmpty, "a shift to an escaped prompt compiled")
    assert(e.contains("not on the prompt stack"), s"the message is not ours: $e")
  }

  test("the hole, said: a PROGRAM built under an inner stack and run after its reset is still a run-time NoPrompt") {
    var leaked: Int ! (Delim + P) = null
    // a `def`: on a Pure row the machine runs while the program is
    // being BUILT, so the throw lands here rather than inside `!.run`
    def prog = Stacked.delimited[Int, P] { outer =>
      outer.stack.reset[Int, P] { inner =>
        leaked = inner.stack.shift[Int, Int, P](inner.p)(k => k(1))
        pure[Delim + P, Int](0)
      }.flatMap(_ => leaked)
    }
    val _ = intercept[NoPrompt](!.run(prog))
  }
}
