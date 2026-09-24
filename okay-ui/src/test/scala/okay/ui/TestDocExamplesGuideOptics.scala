package okay.ui

import okay.*
import okay.given
import okay.codec.Json
import okay.codec.Json.*
import okay.codec.JsonOptic.plate

/**
 * docs/guide.md §10, "Optics: naming a path once", VERBATIM
 * (doc-snippet-debt). Here because this module sees all three of the
 * page's trees: a case-class record, `Json` and `Ui`.
 */
object GuideOptics:
  final case class Address(city: String)
  final case class Person(name: String, address: Option[Address])

class TestDocExamplesGuideOptics extends munit.FunSuite:
  import GuideOptics.*

  test("an optic names the path once: read, write, and absent is a no-op") {
    val p = Person("ada", Some(Address("kyiv")))
    val city = Lens[Person](_.address).andThen(Prism.some).andThen(Lens[Address](_.city))
    val read =
      city.preview(p)                  // the read
    val written =
      city.modify(_.capitalize)(p)     // the write, same path, absent is a no-op
    assertEquals(read, Some("kyiv"))
    assertEquals(written, Person("ada", Some(Address("Kyiv"))))
    val homeless = Person("bob", None)
    assertEquals(city.modify(_.capitalize)(homeless), homeless)
  }

  test("a walk is a cursor: into, next, edit, fold in; a program at the focus; the path back") {
    val doc: Json = JArr(Vector(JStr("ada"), JStr("bob")))
    val edited =
      Zipper(doc).first.flatMap(_.right).map(_.set(JStr("grace")).root)   // into, next, edit, fold in
    assertEquals(edited, Some(JArr(Vector(JStr("ada"), JStr("grace")))))

    val z = Zipper(doc).first.get
    val prog: Int ! State % Json = State.modify[Json](_ => JStr("ADA")).map(_ => 1)
    val (after, n) = !.run(State.handle(z)(
      State.zoom(Zipper.focus)(prog)        // a State % T program run AT the focus
    ))
    assertEquals((n, after.root), (1, JArr(Vector(JStr("ADA"), JStr("bob")))))

    val ui: Ui = Ui.Box(Vector(Ui.Text("a"), Ui.Text("b")), Dir.Vertical, key = "root")
    val path = List(1)
    val back =
      Zipper.at[Ui](path)                   // the path back as an affine — Ui.path
    assertEquals(back.preview(ui), Ui.path(path).preview(ui))
    assertEquals(back.preview(ui), Some(Ui.Text("b")))
  }
