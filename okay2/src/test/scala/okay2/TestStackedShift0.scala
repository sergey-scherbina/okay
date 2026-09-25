package okay2

import Delim.Stacked

/**
 * okay2-dollar, the stacked half: `Has` carries the stack BELOW a
 * prompt, so a stacked `shift0` runs its body under that stack (ICFP
 * 2011's rule for S0) and a shift from it to the consumed prompt is a
 * compile error. The twin of the Scala 3 core's TestStackedShift0.
 */
class TestStackedShift0 extends munit.FunSuite {

  type P = Pure

  test("a stacked shift0 captures up to its prompt, and k re-installs it") {
    val r = !.run(Stacked.delimited[Int, P] { outer =>
      outer.stack.reset[Int, P] { inner =>
        inner.stack.shift0[Int, Int, P](inner.p).apply(_ => k => k(1).map(_ + 10))
      }.map(_ * 2)
    })
    assertEquals(r, 22)
  }

  test("the body runs BELOW the consumed prompt: a shift to a prompt under it resolves") {
    // shift0 to inner leaves outer in force: the body's shift to outer
    // takes (+ 10) along — k2 is the rest of the outer block
    val r = !.run(Stacked.delimited[Int, P] { outer =>
      outer.stack.reset[Int, P] { inner =>
        inner.stack.shift0[Int, Int, P](inner.p).apply(below => _ => below.shift[Int, Int, P](outer.p)(k2 => k2(5).map(_ + 100)))
      }.map(_ + 10)
    })
    assertEquals(r, 115)
  }

  test("a shift from the body to the CONSUMED prompt is a compile error, and to the one below is not") {
    val consumed = compileErrors("""
      okay2.Delim.Stacked.delimited[Int, okay2.Pure] { outer =>
        outer.stack.reset[Int, okay2.Pure] { inner =>
          inner.stack.shift0[Int, Int, okay2.Pure](inner.p).apply(below => _ =>
            below.shift[Int, Int, okay2.Pure](inner.p)(k2 => k2(1)))
        }
      }""")
    assert(consumed.contains("is not on the prompt stack"), consumed)
    val below = compileErrors("""
      okay2.Delim.Stacked.delimited[Int, okay2.Pure] { outer =>
        outer.stack.reset[Int, okay2.Pure] { inner =>
          inner.stack.shift0[Int, Int, okay2.Pure](inner.p).apply(below => _ =>
            below.shift[Int, Int, okay2.Pure](outer.p)(k2 => k2(1)))
        }
      }""")
    assertEquals(below, "")
  }

  test("a stacked dollar: an Int body leaves through ret as a String, and a shift0 to it takes ret along") {
    val r = !.run(Stacked.delimited[String, P] { s =>
      s.stack.dollar[Int, String, P](i => pure[Delim + P, String](s"n=$i")) { d =>
        d.stack.shift0[String, Int, P](d.p).apply(_ => k => k(1).flatMap(a => k(2).map(b => s"$a|$b"))).map(_ * 10)
      }
    })
    assertEquals(r, "n=10|n=20")
  }
}
