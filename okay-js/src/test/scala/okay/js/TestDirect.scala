package okay.js

import okay.js.Dyn.global

/**
 * `js { … }` (specs/js.md stage 4).
 *
 * The subset is closed and small. What these hold is that the shapes
 * IN it translate to the obvious JavaScript, and — the part that
 * matters more — that the shapes outside it are refused rather than
 * approximated. An approximation of Scala's semantics in a browser
 * fails by being wrong, and wrong output in a browser is the failure
 * nobody notices.
 */
class TestDirect extends munit.FunSuite:

  def src(p: Vector[Stmt]): String = Js.print(p).trim

  test("a val is a var, and the obvious arithmetic is the same arithmetic") {
    val p = Direct.js {
      val x = 1
      val y = x + 2 * 3
    }
    assertEquals(src(p), "var x = 1;\nvar y = x + 2 * 3;")
  }

  test("precedence comes from the TREE, so Scala's grouping is kept") {
    val p = Direct.js {
      val y = (1 + 2) * 3
    }
    assertEquals(src(p), "var y = (1 + 2) * 3;")
  }

  test("a string, a boolean and null are themselves") {
    val p = Direct.js {
      val a = "hi"
      val b = true
      val c = null
    }
    assertEquals(src(p), "var a = \"hi\";\nvar b = true;\nvar c = null;")
  }

  test("an if is a statement, and an if with a value is a ternary") {
    val statement = Direct.js {
      val x = 1
      if (x > 0) { global.console.log(x) }
    }
    assert(src(statement).contains("if (x > 0) {"), src(statement))
    assert(!src(statement).contains("?"), src(statement))

    val ternary = Direct.js {
      val x = 1
      val y = if (x > 0) 1 else 2
    }
    assertEquals(src(ternary), "var x = 1;\nvar y = x > 0 ? 1 : 2;")
  }

  test("an if with no else writes no else") {
    val p = Direct.js {
      val x = 1
      if (x > 0) { global.f(x) }
    }
    assert(!src(p).contains("else"), src(p))
  }

  test("a while is a while") {
    val p = Direct.js {
      var n = 0
      while (n < 10) { n = n + 1 }
    }
    assert(src(p).contains("while (n < 10) {"), src(p))
    assert(src(p).contains("n = n + 1;"), src(p))
  }

  test("a global reads as a global, and a call as a call") {
    val p = Direct.js {
      global.console.log("hi")
    }
    assertEquals(src(p), """console.log("hi");""")
  }

  test("a field with no call is a field") {
    val p = Direct.js {
      val d = global.document.body
    }
    assertEquals(src(p), "var d = document.body;")
  }

  test("a lambda is a function, which is where the two languages agree exactly") {
    val p = Direct.js {
      val f = (a: Dyn, b: Dyn) => a
    }
    assert(src(p).contains("function (a, b) {"), src(p))
  }

  test("!, && and || carry over") {
    val p = Direct.js {
      val a = true
      val b = false
      val c = a && !b || a
    }
    assertEquals(src(p), "var a = true;\nvar b = false;\nvar c = a && !b || a;")
  }

  // ---- the one deliberate mapping, and its limit --------------------

  test("== becomes ===, because Scala's == is equality and JavaScript's coerces") {
    val p = Direct.js {
      val x = 1
      val same = x == 1
      val other = x != 2
    }
    assert(src(p).contains("x === 1"), src(p))
    assert(src(p).contains("x !== 2"), src(p))
  }

  // ---- a Js value joins a js { } block ------------------------------

  test("a Js value is spliced where it stands, so the two roads share a program") {
    val p = Direct.js {
      val v = Js.Arr(Vector(Js.Str("link"), Js.Str("table")))
    }
    assertEquals(src(p), """var v = ["link", "table"];""")
  }

  // ---- and the whole thing folds to a constant ----------------------

  test("the same block prints into a compile-time constant") {
    inline val s = Direct.source {
      val x = 1
      global.console.log(x)
    }
    assert(s.contains("var x = 1;"), s)
    assert(s.contains("console.log(x);"), s)
  }

  test("what js { } builds is an ordinary value, so it composes") {
    val a = Direct.js { val x = 1 }
    val b = Direct.js { val y = 2 }
    assertEquals(src(a ++ b), "var x = 1;\nvar y = 2;")
    assertEquals(Js.raws(a ++ b), 0)
  }
