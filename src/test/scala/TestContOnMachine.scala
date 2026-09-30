package okay

import okay.Delim.Stacked.{delimited, contShift, shift0, under}

/**
 * specs/indexed-effects.md, stage 7: Cont's leaf on the one Delim
 * machine. The classic shapes answer what Cont's runner answers; a
 * Cont segment sits beside Delim's own operations; the one thing a
 * synchronous `k` cannot do is refused by name.
 */
class TestContOnMachine extends munit.FunSuite:

  type P = okay.Pure

  test("k(1) + k(10): the body's k is synchronous and multi-shot, and the rest of the body runs per call") {
    val r = !.run(delimited[Int, P] { s =>
      import s.given
      contShift[Int, Int, P](s.p)(k => k(1) + k(10)).map(_ * 2)
    })
    assertEquals(r, 22)
  }

  test("a list reflected through the machine: the continuation called once per element") {
    val r = !.run(delimited[List[Int], P] { s =>
      import s.given
      contShift[List[Int], Int, P](s.p)(k => List(1, 2, 3).flatMap(k)).map(x => List(x * 10))
    })
    assertEquals(r, List(10, 20, 30))
  }

  test("a Cont segment beside Delim's own capture: a shift0 inside the segment k runs") {
    val r = !.run(delimited[Int, P] { s =>
      import s.given
      contShift[Int, Int, P](s.p)(k => k(1) + k(2)).flatMap { x =>
        // the segment k runs contains a typed capture to the same prompt
        shift0[Int, Int, P](s.p)(k2 => k2(x * 100).map(_ + 1))
      }
    })
    // per call of k: shift0 captures up to p with the rest (identity), k2(x*100) answers x*100, +1
    assertEquals(r, (101) + (201))
  }

  test("a synchronous k that meets a foreign operation is refused by name") {
    // `delimited` RUNS the machine when the value is built, so the
    // refusal fires at construction: the whole build sits under intercept
    val e = intercept[UnsupportedOperationException] {
      val p: Int ! Writer % String = delimited[Int, Writer % String] { s =>
        import s.given
        contShift[Int, Int, Writer % String](s.p)(k => k(1)).flatMap(x =>
          under(Row.into[Int, Writer % String, Delim + Writer % String](Writer.tell("x").map(_ => x))))
      }
      !.run(Writer.run[String, Int, P](p))
    }
    assert(e.getMessage.contains("foreign operation"), e.getMessage)
  }
