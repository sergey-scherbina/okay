package okay

import okay.Direct.*
import Lexical.State.{get, put}
import Layered.{reify, reflect}

/**
 * specs/direct-layers-instances.md: direct blocks over Lexical instances
 * and Layered layers. Stage 0 found that most of it already worked; these
 * pin it, with stage 1's two additions.
 */
class TestDirectLayersInstances extends munit.FunSuite:

  def run[A](p: A ! Delim + Pure): A = !.run(Delim.run[A, Pure](p))

  // ------------------------------------------------ instances

  test("a Lexical instance in a direct block: `get.?`, and `put(v).?` as a clean statement") {
    val r = run(Lexical.State.deep[Int, Int, Delim + Pure](0) { s =>
      direct {
        val v = s.get.?
        s.put(v + 1).?
        v + 10
      }
    })
    assertEquals(r, (1, 10))
  }

  test("two instances of one effect in one block, each answered by its own installation") {
    val r = run(Lexical.State.deep[Int, Int, Delim + Pure](1) { a =>
      Lexical.State.deep[Int, Int, Delim + Pure](20) { b =>
        direct {
          val x = a.get.?
          val y = b.get.?
          a.put(x + y).?
          x * 100 + y
        }
      }.map(_._2)
    })
    assertEquals(r, (21, 120))
  }

  test("the tail default on the pure row: no Delim, no machine, a plain program") {
    assertEquals(!.run(Lexical.State[Int, Int, Pure](5) { s =>
      direct {
        val v = s.get.?
        s.put(v * 3).?
        v
      }
    }), (15, 5))
  }

  // ------------------------------------------------ layers: the mark on the monad's own value

  test("the mark on a List inside its layer IS its reflect") {
    val r = run(reify[List, Int, Pure] {
      direct {
        val x = List(1, 2, 3).?
        x * 10
      }
    })
    assertEquals(r, List(10, 20, 30))
  }

  test("two layers, marked on their own values: List outside Option") {
    val r = run(reify[List, Option[Int], Pure] {
      reify[Option, Int, Pure] {
        direct {
          val x = List(1, 2, 3).?
          val y = (if x == 2 then None else Some(x * 10)).?
          x + y
        }
      }
    })
    assertEquals(r, List(Some(11), None, Some(33)))
  }

  test("the same block with the explicit reflect gives the same answer (the mark is only spelling)") {
    val r = run(reify[List, Option[Int], Pure] {
      reify[Option, Int, Pure] {
        direct {
          val x = List(1, 2, 3).reflect[Option[Int], Pure].?
          val y = (if x == 2 then None else Some(x * 10)).reflect[Int, Pure].?
          x + y
        }
      }
    })
    assertEquals(r, List(Some(11), None, Some(33)))
  }

  test("a layer in scope but a block row WITHOUT Delim: refused, naming Delim") {
    val e = compileErrors("""
      okay.Layered.reify[List, Int, okay.Pure] {
        val inner: Int ! okay.Writer % String = okay.Direct.direct {
          val x = List(1, 2, 3).?
          x
        }
        okay.Delim.shift0[List[Int], Int, okay.Pure](???)(k => ???)
      }""")
    assert(e.contains("has no Delim"), s"compiled, or refused for another reason: $e")
  }

  test("no layer in scope: the mark on a List is refused as it always was") {
    val e = compileErrors("""
      val p: Int ! okay.Delim + okay.Pure = okay.Direct.direct {
        val x = List(1, 2, 3).?
        x
      }""")
    assert(e.contains("neither this block"), s"compiled, or refused for another reason: $e")
  }
