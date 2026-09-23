package okay.ui

import okay.*
import okay.given
import okay.codec.{Json, Schema}

/**
 * specs/form-drill.md — a form one level at a time over the dotted
 * path the router already routes by, and a form over a typed cursor.
 */
class TestFormDrill extends munit.FunSuite {

  import Event.*

  final case class Address(city: String, zip: Int)
  final case class Customer(name: String, address: Address)
  final case class Line(sku: String, qty: Int)
  enum Shape derives Schema:
    case Circle(r: Double)
    case Square(side: Double)
  final case class Order(id: Int, customer: Customer, lines: Vector[Line], shape: Shape)
  given Schema[Address] = Schema.derived
  given Schema[Customer] = Schema.derived
  given Schema[Line] = Schema.derived
  given Schema[Order] = Schema.derived

  val order = Order(7, Customer("ada", Address("Wrocław", 50001)), Vector(Line("a", 2), Line("b", 1)), Shape.Circle(1.5))
  def enc[A](a: A)(using s: Schema[A]): Json = Json.parse(Json.write(a))
  val doc: Json = enc(order)

  /** the interactive widgets of a view, in tab order, as (key, kind) */
  def widgets(ui: Ui): Vector[(String, String)] = Ui.focusable(ui).map {
    case Ui.Input(v, k, _, _, _) => (k, s"input=$v")
    case Ui.Button(l, k, _) => (k, s"button=$l")
    case Ui.Select(_, i, k) => (k, s"select=$i")
    case Ui.Check(on, k, _) => (k, s"check=$on")
    case other => ("?", other.toString)
  }
  def keysOf(ui: Ui): Vector[String] = widgets(ui).map(_._1)
  def texts(ui: Ui): Vector[String] = ui match
    case Ui.Text(s, _) => Vector(s)
    case Ui.Column(cs, _) => cs.flatMap(texts)
    case Ui.Row(cs, _) => cs.flatMap(texts)
    case Ui.Form(fs, _, _) => fs.flatMap(texts)
    case _ => Vector.empty

  test("renderAt at the root: scalars as widgets, composites as into buttons, nothing nested") {
    val ui = Form.renderAt[Order](doc, "")
    assertEquals(keysOf(ui), Vector("id", "customer$into", "lines$into", "shape$into"))
    assertEquals(widgets(ui)(0), ("id", "input=7"))
    assertEquals(widgets(ui)(1), ("customer$into", "button=customer ›"))
  }

  test("renderAt one level down: the focus's scalars keyed by the full path, its composites as into") {
    assertEquals(keysOf(Form.renderAt[Order](doc, "customer")), Vector("customer.name", "customer.address$into"))
    assertEquals(keysOf(Form.renderAt[Order](doc, "customer.address")), Vector("customer.address.city", "customer.address.zip"))
    assertEquals(widgets(Form.renderAt[Order](doc, "customer.address"))(0), ("customer.address.city", "input=Wrocław"))
  }

  test("renderAt at a list: items as into with their del, and add; at an item, its scalars") {
    assertEquals(keysOf(Form.renderAt[Order](doc, "lines")),
      Vector("lines[0]$into", "lines[0]$del", "lines[1]$into", "lines[1]$del", "lines$add"))
    assertEquals(keysOf(Form.renderAt[Order](doc, "lines[1]")), Vector("lines[1].sku", "lines[1].qty"))
    assertEquals(widgets(Form.renderAt[Order](doc, "lines[1]"))(0), ("lines[1].sku", "input=b"))
  }

  test("renderAt at a sum: the case Select and the chosen case's scalars; choosing the other case re-renders") {
    val ui = Form.renderAt[Order](doc, "shape")
    assertEquals(keysOf(ui), Vector("shape.$case", "shape.r"))
    val squared = Form.submitted[Order](doc, Chosen("shape.$case", 1))
    assertEquals(keysOf(Form.renderAt[Order](squared, "shape")), Vector("shape.$case", "shape.side"))
  }

  test("renderAt off the schema or past the end renders the root") {
    val root = keysOf(Form.renderAt[Order](doc, ""))
    assertEquals(keysOf(Form.renderAt[Order](doc, "nosuch")), root)
    assertEquals(keysOf(Form.renderAt[Order](doc, "customer.nosuch")), root)
    assertEquals(keysOf(Form.renderAt[Order](doc, "lines[9]")), root)
    assertEquals(keysOf(Form.renderAt[Order](doc, "shape.$case")), root)
  }

  /** the drill screen, driven through Nav.update as TestScreens drives */
  final class Drive(start: Json = doc):
    var answer: Option[Option[Json]] = None
    var stack: List[Screen] = Nav.state(Form.drill[Order](start) { a => answer = Some(a); Nav.Pop })
    def step(events: Event*): Unit = for e <- events do stack = Nav.update(stack, e)
    def apply(events: Event*): Ui =
      step(events*)
      Nav.view(stack)

  val moves: Set[Event] = Set(Pressed("$out"))
  def isMove(e: Event): Boolean = e match
    case Pressed(k) => k.endsWith("$into") || k == "$out"
    case _ => false

  test("THE LAW: edits through the drill with moves between them leave the value the flat form's fold leaves") {
    val scripts: Vector[Vector[Event]] = Vector(
      Vector(Pressed("customer$into"), Edited("customer.name", "grace"), Pressed("customer.address$into"),
        Edited("customer.address.city", "Kraków"), Pressed("$out"), Pressed("$out"), Edited("id", "8")),
      Vector(Pressed("lines$into"), Pressed("lines$add"), Pressed("lines[2]$into"), Edited("lines[2].sku", "c"),
        Edited("lines[2].qty", "9"), Pressed("$out"), Pressed("lines[0]$del"), Pressed("$out")),
      Vector(Pressed("shape$into"), Chosen("shape.$case", 1), Edited("shape.side", "4"), Pressed("$out"),
        Pressed("$out"), Pressed("$out")),
      Vector(Edited("id", "1"), Pressed("customer$into"), Pressed("$out"), Pressed("customer$into"),
        Pressed("customer.address$into"), Edited("customer.address.zip", "1")))
    for script <- scripts do
      val d = Drive()
      d.step(script*)
      d.step(Pressed("$done"))
      val flat = script.filterNot(isMove).foldLeft(doc)(Form.submitted[Order])
      assertEquals(d.answer, Some(Some(flat)), s"script $script")
  }

  test("drill: into shows the sub-form, out pops, out at the root is a no-op; done/cancel answer") {
    val d = Drive()
    assertEquals(keysOf(d()).take(4), Vector("id", "customer$into", "lines$into", "shape$into"))
    assertEquals(texts(d()).head, "/")
    val in = d(Pressed("customer$into"))
    assertEquals(texts(in).head, "customer")
    assertEquals(keysOf(in).take(2), Vector("customer.name", "customer.address$into"))
    assertEquals(texts(d(Pressed("customer.address$into"))).head, "customer.address")
    assertEquals(texts(d(Pressed("$out"))).head, "customer")
    assertEquals(texts(d(Pressed("$out"))).head, "/")
    assertEquals(d(Pressed("$out")), d())
    d.step(Pressed("$cancel"))
    assertEquals(d.answer, Some(None))
    assertEquals(d.stack, Nil)
  }

  test("drill: done with a field error shows it under its field — or under the way in — and stays") {
    val d = Drive(Form.blank[Order])
    d.step(Pressed("customer$into"), Pressed("$done"))               // name is blank: refused
    val shown = d()
    assertEquals(d.answer, None)
    assert(texts(shown).exists(_.startsWith("! ")), s"an error line at the focus: ${texts(shown)}")
    d.step(Pressed("$out"), Pressed("$done"))
    val atRoot = texts(d())
    assert(atRoot.exists(_.startsWith("! ")), s"the error under customer ›: $atRoot")
    // a sub-record present but a field inside it missing: the error
    // BELOW the way in is shown under it, named by its rest of path
    val partial = Drive(Form.submitted[Order](doc, Edited("customer.address.zip", "x")))
    partial.step(Pressed("$done"))
    val below = texts(partial())
    assert(below.exists(_.startsWith("! address.zip")), s"the nested error under customer ›: $below")
    assertEquals(partial.answer, None)
    // a valid document: done answers
    val fixed = Drive()
    fixed.step(Pressed("$done"))
    assertEquals(fixed.answer, Some(Some(doc)))
  }

  test("drillValue: an Order in, the edited Order out, or None") {
    var got: Option[Option[Order]] = None
    var stack = Nav.state(Form.drillValue[Order](order) { a => got = Some(a); Nav.Pop })
    for e <- Vector(Pressed("customer$into"), Edited("customer.name", "grace"), Pressed("$out"), Pressed("$done")) do
      stack = Nav.update(stack, e)
    assertEquals(got, Some(Some(order.copy(customer = order.customer.copy(name = "grace")))))
    var got2: Option[Option[Order]] = None
    var stack2 = Nav.state(Form.drillValue[Order](order) { a => got2 = Some(a); Nav.Pop })
    stack2 = Nav.update(stack2, Pressed("$cancel"))
    assertEquals(got2, Some(None))
  }

  /** a scripted host, as TestDialog's */
  final class Scripted(script: Seq[Event]) extends Host:
    val frames = scala.collection.mutable.Buffer[Ui]()
    def render(ui: Ui): Unit ! Async = async { frames += ui; () }
    def events: Source[Event] = okay.Source.of(script.toList)

  test("askFrom: the initial value's fields are filled; ok without edits answers it") {
    val host = Scripted(Seq(Pressed("$ok")))
    val out = Dialog.run(host)(Form.askFrom("customer", order.customer)).runWith
    assertEquals(out, Some(Some(order.customer)))
    assertEquals(widgets(host.frames.head).collect { case ("name", v) => v }, Vector("input=ada"))
  }

  test("askAt: a typed cursor's focus asked, edited, put back — up is the order with the new name") {
    val customer = Lens[Order](_.customer)
    val z = TypedZipper(order).down(customer)
    val edited = Dialog.run(Scripted(Seq(Edited("name", "grace"), Pressed("$ok"))))(Form.askAt(z, "customer")).runWith
    assertEquals(edited.flatten.map(_.up.root), Some(order.copy(customer = order.customer.copy(name = "grace"))))
    assertEquals(edited.flatten.map(_.focus.address), Some(order.customer.address))
    val cancelled = Dialog.run(Scripted(Seq(Pressed("$cancel"))))(Form.askAt(z, "customer")).runWith
    assertEquals(cancelled, Some(None))
  }

  test("drill at a key, and from a typed cursor: opened where the code chose, navigated from there") {
    val d = Nav.state(Form.drill[Order](doc, "customer.address")(_ => Nav.Pop))
    assertEquals(texts(Nav.view(d)).head, "customer.address")
    assertEquals(keysOf(Nav.view(d)).take(2), Vector("customer.address.city", "customer.address.zip"))
    // from a cursor: pathKey is the position, root is the document
    var got: Option[Option[Order]] = None
    val cursor = TypedZipper(order).field("customer").field("address")
    var stack = Nav.state(Form.drillAt(cursor) { a => got = Some(a); Nav.Pop }.get)
    assertEquals(texts(Nav.view(stack)).head, "customer.address")
    for e <- Vector(Edited("customer.address.city", "Gdańsk"), Pressed("$out"), Pressed("$out"), Pressed("$done")) do
      stack = Nav.update(stack, e)
    assertEquals(got, Some(Some(order.copy(customer = order.customer.copy(address = Address("Gdańsk", 50001))))))
    // a lens frame has no key, so no screen — the refusal is a None, not a root
    val byLens = Lens[Order](_.customer)
    assertEquals(Form.drillAt(TypedZipper(order).down(byLens))(_ => Nav.Pop).isDefined, false)
  }
}
