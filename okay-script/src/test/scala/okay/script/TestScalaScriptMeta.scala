package okay.script

/** Integration between `run`/`render` and `okay.script.Meta` -- code
 * inside an .md file reading the front-matter/heading-scoped ```yaml
 * metadata defined AROUND it, as its current context (operator,
 * 2026-09-03). See specs/okay-script.md "Metadata as context".
 */
class TestScalaScriptMeta extends munit.FunSuite:

  test("render: ${expr} in prose reads Meta.current -- front-matter, no heading involved") {
    val md =
      """---
        |tagline: Решаю любые IT-проблемы
        |---
        |
        |Слоган: ${okay.script.Meta.current("tagline")}
        |""".stripMargin
    val r = ScalaScript.render(md)
    assert(r.ok, r.errors.mkString("\n") + r.thrown.map(_.toString).getOrElse(""))
    assert(r.stdout.contains("Слоган: Решаю любые IT-проблемы"), r.stdout)
  }

  test("render: code under a deeper heading sees its OWN yaml, the parent's yaml, AND front-matter -- nearest wins on a shared key") {
    val md =
      """---
        |color: blue
        |shared: fm
        |---
        |
        |# Parent
        |
        |```yaml
        |shared: parent
        |onlyParent: p
        |```
        |
        |## Child
        |
        |```yaml
        |shared: child
        |```
        |
        |```scala
        |import okay.script.Meta
        |val ctx = Meta.current
        |println("shared=" + ctx("shared"))
        |println("onlyParent=" + ctx("onlyParent"))
        |println("color=" + ctx("color"))
        |```
        |""".stripMargin
    val r = ScalaScript.run(md)
    assert(r.ok, r.errors.mkString("\n") + r.thrown.map(_.toString).getOrElse(""))
    assert(r.stdout.contains("shared=child"), r.stdout)
    assert(r.stdout.contains("onlyParent=p"), r.stdout)
    assert(r.stdout.contains("color=blue"), r.stdout)
  }

  test("run: a code block under a PARENT heading (before the child's own yaml is declared) does not see it -- document order") {
    val md =
      """# Parent
        |
        |```scala
        |println("early:" + okay.script.Meta.current.get("laterKey"))
        |```
        |
        |## Child
        |
        |```yaml
        |laterKey: v
        |```
        |""".stripMargin
    val r = ScalaScript.run(md)
    assert(r.ok, r.errors.mkString("\n") + r.thrown.map(_.toString).getOrElse(""))
    assert(r.stdout.contains("early:None"), r.stdout)
  }

  test("code outside any heading still sees the front-matter; Context.section is None there") {
    val md =
      """---
        |id: root-level
        |---
        |
        |```scala
        |import okay.script.Meta
        |println("id=" + Meta.current("id"))
        |println("section=" + Meta.current.section)
        |```
        |""".stripMargin
    val r = ScalaScript.run(md)
    assert(r.ok, r.errors.mkString("\n") + r.thrown.map(_.toString).getOrElse(""))
    assert(r.stdout.contains("id=root-level"), r.stdout)
    assert(r.stdout.contains("section=None"), r.stdout)
  }

  test("the typed AST is reachable via Meta.current.doc, independent of position -- ../it-consulting's site.md shape") {
    val md =
      """# Услуги
        |
        |```yaml services
        |- key: audit
        |  name: Развитие
        |  price: 4500.00
        |- key: review
        |  name: Исправление ошибок
        |  price: 1800.00
        |```
        |
        |```scala
        |import okay.script.Meta
        |val services = Meta.current.doc.root.children.head.yaml.head match
        |  case Meta.Value.Arr(items) => items
        |  case _ => Vector.empty
        |println("count=" + services.length)
        |println("first=" + services.head.field("name").flatMap(_.asString).getOrElse("?"))
        |```
        |""".stripMargin
    val r = ScalaScript.run(md)
    assert(r.ok, r.errors.mkString("\n") + r.thrown.map(_.toString).getOrElse(""))
    assert(r.stdout.contains("count=2"), r.stdout)
    assert(r.stdout.contains("first=Развитие"), r.stdout)
  }

  test("a metadata-free document (no front-matter, no yaml, no headings) never references okay.script.Meta -- an empty Classpath (scala runtime only) still compiles it") {
    val md = "```scala\nprintln(\"plain\")\n```\n"
    val onlyScalaRuntime = Classpath(Classpath.ambient.entries.filter { p =>
      val n = p.getFileName.toString
      n.startsWith("scala-library") || n.startsWith("scala3-library")
    })
    val r = ScalaScript.run(md, classpath = onlyScalaRuntime)
    assert(r.ok, r.errors.mkString("\n") + r.thrown.map(_.toString).getOrElse(""))
    assert(r.stdout.contains("plain"), r.stdout)
  }

  test("a locally-declared `given Meta.Context = Meta.current` works for a script that wants given-style ergonomics") {
    val md =
      """---
        |name: okay
        |---
        |
        |```scala
        |import okay.script.Meta
        |def greet()(using ctx: Meta.Context): String = "hi " + ctx("name")
        |given Meta.Context = Meta.current
        |println(greet())
        |```
        |""".stripMargin
    val r = ScalaScript.run(md)
    assert(r.ok, r.errors.mkString("\n") + r.thrown.map(_.toString).getOrElse(""))
    assert(r.stdout.contains("hi okay"), r.stdout)
  }
