package okay.js

import Js.*

/**
 * The tree and the printer (specs/js.md stage 1).
 *
 * Three properties are tested rather than hoped for, and one of them
 * is a security property: a generator whose output can be ended by
 * its own data is a cross-site scripting hole, and this module exists
 * to put data inside scripts.
 */
class TestJs extends munit.FunSuite:

  test("a literal prints as itself, and a whole number carries no .0") {
    assertEquals(Js.print(Num(3)), "3")
    assertEquals(Js.print(Num(3.5)), "3.5")
    assertEquals(Js.print(Num(-7)), "-7")
    assertEquals(Js.print(Bool(true)), "true")
    assertEquals(Js.print(Null), "null")
    assertEquals(Js.print(Undefined), "undefined")
    assertEquals(Js.print(Name("window")), "window")
  }

  // ---- escaping, which is why this module can be used at all -------

  test("a quote and a backslash cannot end the literal they are in") {
    assertEquals(Js.print(Str("a\"b")), """"a\"b"""")
    assertEquals(Js.print(Str("a\\b")), """"a\\b"""")
  }

  test("</script CANNOT end the document the script is written into") {
    // it ends a script element wherever it appears, INCLUDING inside
    // a string literal — the one escape that is not about readability
    val out = Js.print(Str("</script><img onerror=alert(1)>"))
    assert(!out.contains("</script"), out)
    assert(out.contains("\\u003c"), out)
  }

  test("U+2028 and U+2029 are escaped: JSON allows them and JavaScript does not") {
    // BY CODE, never as a Scala escape: a \\uXXXX in Scala source is
    // processed before the parser sees it, so writing the escape
    // would put a real line separator in this file
    val ls = 0x2028.toChar
    val ps = 0x2029.toChar
    val out = Js.print(Str(s"a${ls}b${ps}c"))
    assert(!out.contains(ls), out)
    assert(!out.contains(ps), out)
    assert(out.contains("\\u2028") && out.contains("\\u2029"), out)
  }

  test("a control character is escaped, because a raw newline does not parse") {
    assertEquals(Js.print(Str("a\nb")), """"a\nb"""")
    val soh = 1.toChar
    assertEquals(Js.print(Str(s"a${soh}b")), """"a\u0001b"""")
  }

  test("an object key is quoted only when it must be") {
    assertEquals(Js.print(obj("a" -> Num(1))), "{a: 1}")
    assertEquals(Js.print(obj("data-key" -> Num(1))), """{"data-key": 1}""")
    // a reserved word is a key an old engine refuses bare
    assertEquals(Js.print(obj("class" -> Num(1))), """{"class": 1}""")
    assertEquals(Js.print(obj("2go" -> Num(1))), """{"2go": 1}""")
  }

  // ---- precedence --------------------------------------------------

  test("precedence parenthesises what must be and nothing else") {
    val a = Name("a"); val b = Name("b"); val c = Name("c")
    assertEquals(Js.print(Bin("*", Bin("+", a, b), c)), "(a + b) * c")
    assertEquals(Js.print(Bin("+", Bin("*", a, b), c)), "a * b + c")
    assertEquals(Js.print(Bin("+", a, Bin("*", b, c))), "a + b * c")
  }

  test("the right side of a left-associative operator keeps its parentheses") {
    val a = Name("a"); val b = Name("b"); val c = Name("c")
    // a - (b - c) is NOT a - b - c
    assertEquals(Js.print(Bin("-", a, Bin("-", b, c))), "a - (b - c)")
    assertEquals(Js.print(Bin("-", Bin("-", a, b), c)), "a - b - c")
  }

  test("a call on a ternary parenthesises the ternary") {
    val f = Ternary(Name("c"), Name("f"), Name("g"))
    assertEquals(Js.print(Call(f, Vector(Num(1)))), "(c ? f : g)(1)")
  }

  test("a word operator keeps its space and a symbol does not") {
    assertEquals(Js.print(Unary("typeof", Name("x"))), "typeof x")
    assertEquals(Js.print(Unary("!", Name("x"))), "!x")
    // -(-x) must not print as --x, which decrements
    assertEquals(Js.print(Unary("-", Unary("-", Name("x")))), "-(-x)")
  }

  test("a field of a call chains without parentheses") {
    val e = Field(Call(Field(Name("document"), "createElement"), Vector(Str("div"))), "style")
    assertEquals(Js.print(e), """document.createElement("div").style""")
  }

  // ---- statements ---------------------------------------------------

  test("semicolons are written, never inferred") {
    val p = Js.print(Vector(
      Stmt.Var("x", Num(1)),
      Stmt.Do(Call(Field(Name("console"), "log"), Vector(Name("x"))))))
    assertEquals(p.trim, "var x = 1;\nconsole.log(x);")
  }

  test("an if with no else writes no else") {
    val p = Js.print(Vector(Stmt.If(Name("c"), Vector(Stmt.Return(None)))))
    assert(!p.contains("else"), p)
    assert(p.contains("if (c) {"), p)
  }

  test("a switch writes its cases and only the default it has") {
    val bare = Js.print(Vector(Stmt.Switch(Name("t"),
      Vector(Str("a") -> Vector(Stmt.Break)))))
    assert(!bare.contains("default"), bare)
    val full = Js.print(Vector(Stmt.Switch(Name("t"),
      Vector(Str("a") -> Vector(Stmt.Break)), Vector(Stmt.Return(None)))))
    assert(full.contains("default:"), full)
    assert(full.contains("""case "a":"""), full)
  }

  test("a for loop writes its three parts, and an absent one as empty") {
    val p = Js.print(Vector(Stmt.For(
      Some(Stmt.Var("i", Num(0))),
      Some(Bin("<", Name("i"), Name("n"))),
      Some(Unary("++", Name("i"))),
      Vector(Stmt.Break))))
    assert(p.contains("for (var i = 0; i < n; ++i) {"), p)
    val bare = Js.print(Vector(Stmt.For(None, None, None, Vector(Stmt.Break))))
    assert(bare.contains("for (; ; ) {"), bare)
  }

  test("a function is an expression and prints its body") {
    val f = fun("a", "b")(Stmt.Return(Some(Bin("+", Name("a"), Name("b")))))
    val p = Js.print(f)
    assert(p.startsWith("function (a, b) {"), p)
    assert(p.contains("return a + b;"), p)
  }

  // ---- the escape hatch is countable --------------------------------

  test("Raw is counted wherever it hides, so a test can hold it to a number") {
    assertEquals(Js.raws(Vector(Stmt.Do(Name("x")))), 0)
    assertEquals(Js.raws(Vector(Stmt.Raw("/re/.test(x);"))), 1)
    // nested inside an expression inside a function inside a call
    val buried = Stmt.Do(Call(fun()(Stmt.Do(Bin("+", Raw("a"), Raw("b")))), Vector.empty))
    assertEquals(Js.raws(Vector(buried)), 2)
  }

  // ---- what it is for: a program built from other data ---------------

  test("a program can be BUILT from Scala data, which a string literal cannot") {
    // the shape `LiveJs` already generates by hand: a set of names
    // that must not drift from its Scala source
    val vocab = Vector("link", "table", "tabs")
    val decl = Stmt.Var("VOCAB", Arr(vocab.map(Str(_))))
    assertEquals(Js.print(Vector(decl)).trim, """var VOCAB = ["link", "table", "tabs"];""")
  }

  test("a name table becomes a switch, which is what the browser client is") {
    val rows = Vector("bold" -> "okay-bold", "dim" -> "okay-dim")
    val sw = Stmt.Switch(Name("token"),
      rows.map((k, v) => Str(k) -> Vector(Stmt.Return(Some(Str(v))))),
      Vector(Stmt.Return(Some(Str("")))))
    val out = Js.print(Vector(sw))
    assert(out.contains("""case "bold":"""), out)
    assert(out.contains("""return "okay-bold";"""), out)
    assert(out.contains("""default:"""), out)
  }

  test("the builders read as JavaScript rather than as constructors") {
    val e = name("document").dot("getElementById").of(str("root"))
    assertEquals(Js.print(e), """document.getElementById("root")""")
    assertEquals(Js.print(name("a") === name("b")), "a === b")
    assertEquals(Js.print(name("a").not), "!a")
  }
