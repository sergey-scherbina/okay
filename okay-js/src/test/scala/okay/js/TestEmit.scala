package okay.js

/**
 * The compile-time road (specs/js.md stage 2).
 *
 * The point of these is not that the strings are equal — `print`
 * already says that. It is that `emit` produced them WHILE THE
 * COMPILER RAN, so the jar carries the script as a constant and a
 * generated program costs a deployment nothing that a hand-written
 * one does not.
 */
class TestEmit extends munit.FunSuite:

  test("an expression becomes a constant at compile time") {
    inline val s = Emit.emit(Js.Call(
      Js.Field(Js.Name("console"), "log"), Vector(Js.Str("hi"))))
    // `inline val` only compiles when the right side IS a constant,
    // so this line is the assertion: it would not build otherwise
    assertEquals(s, """console.log("hi")""")
  }

  test("the constant is the same text the runtime printer writes") {
    val tree = Js.Bin("+", Js.Num(1), Js.Bin("*", Js.Num(2), Js.Num(3)))
    inline val emitted = Emit.emit(Js.Bin("+", Js.Num(1), Js.Bin("*", Js.Num(2), Js.Num(3))))
    assertEquals(emitted, Js.print(tree))
  }

  test("a whole program becomes one constant") {
    inline val program = Emit.program(Vector(
      Stmt.Var("x", Js.Num(1)),
      Stmt.If(Js.Bin(">", Js.Name("x"), Js.Num(0)),
        Vector(Stmt.Return(Some(Js.Str("yes")))),
        Vector(Stmt.Return(Some(Js.Str("no")))))))
    assert(program.contains("var x = 1;"), program)
    assert(program.contains("""return "yes";"""), program)
    assert(program.contains("} else {"), program)
  }

  test("the escaping is the printer's, so a constant is safe the same way") {
    inline val s = Emit.emit(Js.Str("</script>"))
    assert(!s.contains("</script"), s)
  }

  test("an object and an array survive being unlifted whole") {
    inline val s = Emit.emit(Js.Obj(Vector(
      "vocab" -> Js.Arr(Vector(Js.Str("link"), Js.Str("table"))),
      "version" -> Js.Num(1))))
    assertEquals(s, """{vocab: ["link", "table"], version: 1}""")
  }

  test("a function body survives, which is what a real script is") {
    inline val s = Emit.emit(Js.Fun(Vector("u"), Vector(
      Stmt.Return(Some(Js.Field(Js.Name("u"), "key"))))))
    assert(s.startsWith("function (u) {"), s)
    assert(s.contains("return u.key;"), s)
  }

  // A tree the compiler cannot read is a COMPILE ERROR, not a silent
  // fall back to runtime — the whole value of `emit` is that the
  // constant is in the jar, and a quiet fallback would take that away
  // without saying so. There is no runtime test for that: the proof
  // is that the following does not compile, which is checked by
  // `Emit.emit` refusing a non-literal in a scratch build rather than
  // by a suite that cannot express "this must not compile" without a
  // second compiler run.
  test("the runtime road takes what the compile-time road cannot") {
    val fromData = Js.Arr(Vector("a", "b").map(Js.Str(_)))
    assertEquals(Js.print(fromData), """["a", "b"]""")
  }
