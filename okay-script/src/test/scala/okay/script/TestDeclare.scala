package okay.script

/** okay-script-site: ```scala declare fences at object level -- JSP's
 * `<%! %>`. See specs/okay-script.md "Declarations".
 */
class TestDeclare extends munit.FunSuite:

  test("run: a def declared in a LATER declare block is callable from an EARLIER ```scala block") {
    val md =
      """```scala
        |println("twice: " + twice(21))
        |```
        |
        |```scala declare
        |def twice(n: Int): Int = n * 2
        |```
        |""".stripMargin
    val r = ScalaScript.run(md)
    assert(r.ok, r.errors.mkString("\n") + r.thrown.map(_.toString).getOrElse(""))
    assert(r.stdout.contains("twice: 42"), r.stdout)
  }

  test("render: a declare block produces no output of its own and its members are visible to ${expr}") {
    val md =
      """```scala declare
        |val greeting = "hi"
        |```
        |Say ${greeting}!
        |""".stripMargin
    val r = ScalaScript.render(md)
    assert(r.ok, r.errors.mkString("\n"))
    assertEquals(r.stdout.trim, "Say hi!")
  }

  test("a compile error inside a declare block reports the block's own .md line") {
    val md =
      """# Title
        |
        |```scala declare
        |val ok = 1
        |val bad: Int = "no"
        |```
        |""".stripMargin
    val r = ScalaScript.run(md)
    assert(!r.ok)
    assert(r.errors.exists(_.startsWith("L5:")), r.errors.mkString("\n"))
  }

  test("Deps.declared finds a `using dep` inside a declare block") {
    val md = "```scala declare\n//> using dep \"org.example::x:1.0\"\n```\n"
    assertEquals(Deps.declared(md), Vector("org.example::x:1.0"))
  }

  test("blocks: a declare fence counts as a scala block with its own startLine") {
    val md = "prose\n\n```scala declare\nval a = 1\n```\n\n```scala\nprintln(a)\n```\n"
    val bs = ScalaScript.blocks(md)
    assertEquals(bs.map(_.code), Vector("val a = 1", "println(a)"))
    assertEquals(bs(0).startLine, 4)
  }
