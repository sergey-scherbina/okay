package okay

import okay.Delim.Stacked
import okay.Delim.Stacked.{abort, delimited, reset, shift, under}
import okay.Row.at
// NOT for the old `Comonad[Id]` capture (gone, comonad-id-map-capture):
// found through `Prog`'s companion, the extension's `flatMap` leaves the
// lambda's argument an uninferred `A` in the stacked shapes below
// (`a + 1`: "value + is not a member of A"); imported, it infers.
import okay.Prog.{flatMap, map}

/**
 * specs/freer-base.md, stage 2: the indexed facade, and `Delim`'s
 * prompt stack in the index. The five positive shapes of the probe
 * (scripts/stage2-prompt-identity-probe.scala) run through the REAL
 * machine and answer the shift/reset laws' values; the three shapes
 * that throw `NoPrompt` at run time on the unstacked doors are
 * compile errors here.
 */
class TestProg extends munit.FunSuite:

  type P = okay.Pure

  // ---------------------------------------------------------- positives

  test("1. the simple shape: reset { shift(k => k(5) * 2) } == 10") {
    val r = !.run(delimited[Int, P] { s =>
      import s.given
      shift[Int, Int, P](s.p)(k => k(5).map(_ * 2))
    })
    assertEquals(r, 10)
  }

  test("2. a for-comprehension — the head has no expected type, the stack is a given") {
    val r = !.run(delimited[Int, P] { s =>
      import s.given
      for
        a <- shift[Int, Int, P](s.p)(k => k(1))
        b <- shift[Int, Int, P](s.p)(k => k(a + 1))
      yield b * 10
    })
    assertEquals(r, 20)
  }

  test("3. an ordinary effect in head position, under the stack in force") {
    type F = Reader % Int
    val r = !.run(Reader.run[Int, Int, P](41)(delimited[Int, F] { s =>
      import s.given
      for
        a <- under(Reader.ask[Int].at[Delim + F])
        b <- shift[Int, Int, F](s.p)(k => k(a + 1))
      yield b
    }))
    assertEquals(r, 42)
  }

  test("4. nesting: a shift to the OUTER prompt from inside the inner one — Has.there") {
    val r = !.run(delimited[Int, P] { outer =>
      import outer.given
      reset[Int, P] { inner =>
        import inner.given
        // the outer prompt is second on the stack; the inner import
        // wins the ambiguity, `there` finds the outer one below it
        shift[Int, Int, P](outer.p)(k => k(1).map(_ + 100))
      }.map(_ + 1)
    })
    // k is the rest up to OUTER: (_ + 1) is inside k, (+ 100) is outside
    assertEquals(r, 102)
  }

  test("5. a reset as a step of a larger program, its value from the head") {
    val r = !.run(delimited[Int, P] { s =>
      import s.given
      for
        a <- under(pure[Delim + P, Int](5))
        b <- reset[Int, P] { s2 =>
               import s2.given
               shift[Int, Int, P](s2.p)(k => k(a))
             }
      yield b
    })
    assertEquals(r, 5)
  }

  test("abort and multi-shot, stacked: the laws are the machine's, the stack only checks") {
    val aborted = !.run(delimited[Int, P] { s =>
      import s.given
      abort[Int, Int, P](s.p)(7).map(_ + 100)
    })
    assertEquals(aborted, 7)
    val twice = !.run(delimited[Int, P] { s =>
      import s.given
      shift[Int, Int, P](s.p)(k => k(1).flatMap(a => k(2).map(b => a + b))).map(_ * 10)
    })
    assertEquals(twice, 30)
  }

  // ---------------------------------------------------------- negatives

  test("6. a shift with NO reset is a compile error (NoPrompt at run time on the unstacked door)") {
    val e = compileErrors("""
      { val loose = okay.Delim.prompt[Int]
        okay.Delim.Stacked.shift[Int, Int, okay.Pure](loose)(k => k(1)) }""")
    assert(e.nonEmpty, "a shift with no reset compiled")
    assert(e.contains("Stack"), s"the message does not name the stack: $e")
  }

  test("7. a shift to a FOREIGN prompt of the same answer type is a compile error") {
    val e = compileErrors("""
      okay.Delim.Stacked.delimited[Int, okay.Pure] { s =>
        import s.given
        val stolen = okay.Delim.prompt[Int]
        okay.Delim.Stacked.shift[Int, Int, okay.Pure](stolen)(k => k(1))
      }""")
    assert(e.nonEmpty, "a shift to a foreign prompt compiled")
    assert(e.contains("not on the prompt stack"), s"the message is not ours: $e")
  }

  test("8. a prompt that ESCAPES its reset and is shifted to afterwards is a compile error") {
    // the run-time NoPrompt this stage exists for: after the inner
    // reset returns, the stack in force is the OUTER one, and the
    // leaked prompt is not on it
    val e = compileErrors("""
      okay.Delim.Stacked.delimited[Int, okay.Pure] { outer =>
        import outer.given
        var leaked: okay.Prompt[Int] | Null = null
        okay.Delim.Stacked.reset[Int, okay.Pure] { inner =>
          import inner.given
          leaked = inner.p
          okay.Delim.Stacked.shift[Int, Int, okay.Pure](inner.p)(k => k(1))
        }.flatMap { _ =>
          val l = leaked.nn
          okay.Delim.Stacked.shift[Int, Int, okay.Pure](l)(k => k(2))
        }
      }""")
    assert(e.nonEmpty, "a shift to an escaped prompt compiled")
    assert(e.contains("not on the prompt stack"), s"the message is not ours: $e")
  }

  // ---------------------------------------------------------- the facade itself

  sealed trait A
  sealed trait B

  test("the caveat: an abort inside a block promising a transition drops it — the type is not a run-time guarantee") {
    var moved = false
    val move: Prog[Throws % String, Unit, A, B] =
      Prog.transition[A, B, Throws % String, Unit](Free.delay { () => moved = true; pure(()) })
    val p: Prog[Throws % String, Unit, A, B] =
      Prog.diag[A, Throws % String, Unit](raise[String, Unit]("no")).flatMap(_ => move)
    // the type says B is reached; the abort says otherwise
    val closed: Prog[Throws % String, Unit, A, A] = p.flatMap(_ => Prog.transition[B, A, Throws % String, Unit](pure(())))
    assertEquals(!.run(runEither[Unit, okay.Pure, String](closed.free)), Left("no"))
    assert(!moved, "the transition ran after an abort")
  }

  test("zero cost: the facade is the tree — diag/free are identity, flatMap is the same Bind") {
    val p: Int ! Writer % String = Writer.tell("x").map(_ => 1)
    assert(Prog.diag[A, Writer % String, Int](p).free eq p, "diag then free is not the same object")
    val f: Int => Prog[Writer % String, Int, A, A] = n => Prog.pure(n + 1)
    Prog.diag[A, Writer % String, Int](p).flatMap(f).free match
      case Free.Bind(a, _) => assert(a eq p, "flatMap did not build a Bind over the same head")
      case other => fail(s"flatMap built $other")
    // and it runs as the tree it is
    assertEquals(!.run(Writer.run[String, Int, okay.Pure](Prog.diag[A, Writer % String, Int](p).flatMap(f).free)), (Seq("x"), 2))
  }

  test("a move left open has no `free`, and continuations start where the last step ended") {
    assert(compileErrors("""
      okay.Prog.transition[TestProg#A, TestProg#B, okay.Writer % String, Unit](okay.Writer.tell("x")).free""").nonEmpty,
      "an open move unlifted")
    assert(compileErrors("""
      okay.Prog.transition[TestProg#A, TestProg#B, okay.Writer % String, Unit](okay.Writer.tell("x"))
        .flatMap(_ => okay.Prog.transition[TestProg#A, TestProg#B, okay.Writer % String, Unit](okay.Writer.tell("y")))""").nonEmpty,
      "a step starting at A was joined to one ending at B")
  }
