package okay.js

import okay.js.Dyn.global

/** typescript-types T8: the okay-js subset printed as TypeScript, typed by Scala */
class TestDirectTs extends munit.FunSuite:

  test("each var and each parameter carries the type the Scala compiler inferred") {
    val p = Direct.ts {
      val n = 1
      val s = "a"
      val b = n > 0
      val d = global.document.body
      val f = (x: Int, y: String) => global.console.log(y + x)
      global.console.log(b, d, f(n, s))
    }
    assertEquals(Js.printTs(p).trim,
      """var n: number = 1;
        |var s: string = "a";
        |var b: boolean = n > 0;
        |var d: any = document.body;
        |var f: (a0: number, a1: string) => void = function (x: number, y: string) {
        |  console.log(y + x);
        |};
        |console.log(b, d, f(n, s));""".stripMargin)
  }

  test("the same tree prints as the same JavaScript, the types dropped") {
    val p = Direct.ts {
      val n = 2
      val g = (x: Double) => global.console.log(x * n)
      global.setTimeout(g, n)
    }
    assertEquals(Js.print(p).trim,
      "var n = 2;\nvar g = function (x) {\n  console.log(x * n);\n};\nsetTimeout(g, n);")
  }

  test("tsSource is a compile-time constant") {
    inline val s = Direct.tsSource {
      val k = "key"
      global.localStorage.removeItem(k)
    }
    assertEquals(s.trim, "var k: string = \"key\";\nlocalStorage.removeItem(k);")
  }

  test("a type TypeScript has no name for here is refused by name") {
    val errors = compileErrors("""
      Direct.ts {
        val xs = List(1, 2)
        okay.js.Dyn.global.f(xs)
      }""")
    assert(errors.contains("as TypeScript"), errors)
  }

  test("calling a function held in a val calls it: f(1), never f.apply(1)") {
    val p = Direct.js {
      val f = (x: Int) => global.console.log(x)
      f(1)
    }
    assertEquals(Js.print(p).trim, "var f = function (x) {\n  console.log(x);\n};\nf(1);")
  }

  test("assigning a member of a Dyn is an assignment, not an updateDynamic call") {
    val p = Direct.js {
      val t = "okay"
      global.document.title = t
    }
    assertEquals(Js.print(p).trim, "var t = \"okay\";\ndocument.title = t;")
  }
