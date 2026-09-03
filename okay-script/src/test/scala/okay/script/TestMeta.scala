package okay.script

class TestMeta extends munit.FunSuite:

  test("parse: front-matter is captured as flat key -> value") {
    val md =
      """---
        |id: site-it
        |tagline: Решаю любые IT-проблемы
        |---
        |
        |# Услуги
        |""".stripMargin
    val doc = Meta.parse(md)
    assertEquals(doc.frontMatter.get("id"), Some("site-it"))
    assertEquals(doc.frontMatter.get("tagline"), Some("Решаю любые IT-проблемы"))
  }

  test("parse: a ```yaml list of flat objects under a heading, the site.md shape") {
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
        |""".stripMargin
    val doc = Meta.parse(md)
    assertEquals(doc.root.children.length, 1)
    val services = doc.root.children.head
    assertEquals(services.title, "Услуги")
    assertEquals(services.yaml.length, 1)
    services.yaml.head match
      case Meta.Value.Arr(items) =>
        assertEquals(items.length, 2)
        assertEquals(items(0).field("key"), Some(Meta.Value.Str("audit")))
        assertEquals(items(1).field("name"), Some(Meta.Value.Str("Исправление ошибок")))
      case other => fail(s"expected Arr, got $other")
  }

  test("parse: nested headings build a real tree, and yaml before any heading attaches to root") {
    val md =
      """```yaml
        |site: okay
        |```
        |
        |# Parent
        |
        |## Child
        |
        |```yaml
        |k: v
        |```
        |""".stripMargin
    val doc = Meta.parse(md)
    assertEquals(doc.root.yaml, Vector(Meta.Value.Obj(Vector("site" -> Meta.Value.Str("okay")))))
    assertEquals(doc.root.children.length, 1)
    val parent = doc.root.children.head
    assertEquals(parent.title, "Parent")
    assertEquals(parent.children.length, 1)
    val child = parent.children.head
    assertEquals(child.title, "Child")
    assertEquals(child.yaml, Vector(Meta.Value.Obj(Vector("k" -> Meta.Value.Str("v")))))
  }

  test("Context.get: nearest heading wins over an ancestor's same key, front-matter is the fallback") {
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
        |""".stripMargin
    val doc = Meta.parse(md)
    val parent = doc.root.children.head
    val child = parent.children.head
    val ctxAtChild = Meta.Context(doc, Vector(doc.root, parent, child))
    assertEquals(ctxAtChild.get("shared"), Some("child"))
    assertEquals(ctxAtChild.get("onlyParent"), Some("p"))
    assertEquals(ctxAtChild.get("color"), Some("blue"))
    assertEquals(ctxAtChild.get("nope"), None)

    val ctxAtParent = Meta.Context(doc, Vector(doc.root, parent))
    assertEquals(ctxAtParent.get("shared"), Some("parent"))
  }

  test("Context.apply throws on a missing key; section returns the nearest heading") {
    val doc = Meta.parse("# Only\n")
    val only = doc.root.children.head
    val ctx = Meta.Context(doc, Vector(doc.root, only))
    intercept[NoSuchElementException](ctx("nope")): Unit
    assertEquals(ctx.section, Some(only))
    assertEquals(Meta.Context(doc, Vector(doc.root)).section, None) // root alone -- no REAL heading yet
  }
