package okay2

import okay2.Optic._
import ZipperFixtures._

/** the typed zipper: frames are optics, the parent's type comes back
 * from `up`, a program written against a part runs at the focus — the
 * Scala 3 core's okay-optics TestTypedZipper */
class TestTypedZipper extends munit.FunSuite {

  val customer = Lens[Order](_.customer)
  val address = Lens[Customer](_.address)
  val city = Lens[Address](_.city)
  val name = Lens[Customer](_.name)
  val lines = Lens[Order](_.lines)

  val order = Order(7, Customer("ada", Address("Wrocław", 50001)), Vector(Line.Item("a", 2), Line.Discount(10), Line.Item("b", 1)))

  test("down then up is the input; two ups from two lenses is a Top[Order], and the root is the input itself") {
    val z = TypedZipper(order)
    val c = z.down(customer)
    assertEquals(c.focus, order.customer)
    assertEquals(c.up, z)
    val top: TypedZipper.Top[Order] = c.down(address).up.up     // the type is the claim; the compiler checks it
    assertEquals(top, z)
    assert(c.down(address).root eq order, "a walk without edits rebuilt the whole")
    assertEquals(c.down(address).depth, 2)
  }

  test("set at a two-lens focus then root is the nested copy; the other fields are shared") {
    val edited = TypedZipper(order).down(customer).down(address).set(Address("Kraków", 30001)).root
    assertEquals(edited, order.copy(customer = order.customer.copy(address = Address("Kraków", 30001))))
    assert(edited.lines eq order.lines)
    assertEquals(TypedZipper(order).down(customer).down(address).down(city).modify(_.toUpperCase).root.customer.address.city, "WROCŁAW")
  }

  test("field by name is the field's lens as a frame; a wrong name does not compile") {
    val c = TypedZipper(order).field("customer")
    val cust: Customer = c.focus
    assertEquals(cust, order.customer)
    val z: Int = c.field("address").field("zip").focus
    assertEquals(z, 50001)
    assertEquals(c.field("address").field("city").set("Poznań").root.customer.address.city, "Poznań")
    val errors = compileErrors("""okay2.TypedZipper(okay2.ZipperFixtures.Order(1, null, Vector.empty)).field("customre")""")
    assert(errors.contains("customre is not a case field"), errors)
  }

  test("at(i) on a Vector focus: the element or None; set updates that element only") {
    val ls = TypedZipper(order).down(lines)
    assertEquals(ls.at(1).map(_.focus), Some(Line.Discount(10)))
    assertEquals(ls.at(3), None)
    val edited = ls.at(2).get.set(Line.Item("b", 5)).root
    assertEquals(edited.lines, Vector(Line.Item("a", 2), Line.Discount(10), Line.Item("b", 5)))
    assertEquals(edited.customer, order.customer)
  }

  test("downCase into a sum: the case or None; the edit lands in that case") {
    val second = TypedZipper(order).down(lines).at(1).get
    assertEquals(second.downCase[Line.Item], None)
    val d = second.downCase[Line.Discount].get
    assertEquals(d.focus, Line.Discount(10))
    val edited = d.modify(x => x.copy(pct = x.pct * 2)).root
    assertEquals(edited.lines(1), Line.Discount(20))
    assertEquals(edited.lines(0), order.lines(0))
    val back: TypedZipper.Elem[Order, Line, _] = d.up
    assertEquals(back.focus, Line.Discount(10))
  }

  test("TypedZipper.focus: the lens laws, and a State[Customer] program run while parked in an Order") {
    type C = TypedZipper.Below[Order, Order, Customer, TypedZipper.Top[Order]]
    val l = TypedZipper.focus[Order, Customer, C]
    val c: C = TypedZipper(order).down(customer)
    val v = Customer("v", Address("x", 1)); val w = Customer("w", Address("y", 2))
    def same(x: C, y: C) = x.focus == y.focus && x.root == y.root
    assert(same(l.set(l.get(c))(c), c))
    assertEquals(l.get(l.set(v)(c)), v)
    assert(same(l.set(w)(l.set(v)(c)), l.set(w)(c)))
    val rename: Int ! State[Customer] = State.modify[Customer](cu => cu.copy(name = cu.name.capitalize)).map(_.name.length)
    val (after, n) = State.run(c)(State.zoom[C, Customer, Int, Pure](l)(rename))
    assertEquals(n, 3)
    assertEquals(after.root, order.copy(customer = order.customer.copy(name = "Ada")))
    assertEquals(after.up.focus.id, 7L)
  }

  test("a walk shares its prefix: two branches from one down, each root shows only its own edit") {
    val c = TypedZipper(order).down(customer)
    assertEquals(c.down(name).set("grace").root.customer, Customer("grace", order.customer.address))
    assertEquals(c.down(address).down(city).set("Gdańsk").root.customer, Customer("ada", Address("Gdańsk", 50001)))
    assertEquals(c.root, order)
  }

  test("asAffine: preview is the focus on the cursor's own tree, None where an index or case frame is missing") {
    val z = TypedZipper(order)
    val c = z.down(customer).down(address).down(city)
    assertEquals(c.asAffine.preview(order), Some("Wrocław"))
    assertEquals(c.asAffine.set("Łódź")(order), c.set("Łódź").root)
    val d = z.down(lines).at(1).get.downCase[Line.Discount].get
    assertEquals(d.asAffine.preview(order), Some(Line.Discount(10)))
    assertEquals(d.asAffine.set(Line.Discount(5))(order), d.set(Line.Discount(5)).root)
    val short = order.copy(lines = Vector(Line.Item("only", 1)))
    assertEquals(d.asAffine.preview(short), None)
    assertEquals(d.asAffine.set(Line.Discount(5))(short), short)
    assertEquals(d.asAffine.preview(order.copy(lines = order.lines.updated(1, Line.Item("x", 1)))), None)
    assertEquals(z.asAffine.preview(order), Some(order))
  }

  test("Poly: the whole changes type with the focus — a Box[String] becomes a Box[Int]") {
    val item = Lens[Box[String], Box[Int], String, Int](_.item, (b, i) => Box(i, b.tag))
    val out: Box[Int] = TypedZipper.Poly.of[Box[String], Box[Int]](Box("hello", "t")).down(item).modify(_.length)
    assertEquals(out, Box(5, "t"))
  }

  test("Poly: two lenses and a prism, type-changing; the wrong case builds nothing") {
    val outer = Lens[Box[Box[Option[String]]], Box[Box[Option[Int]]], Box[Option[String]], Box[Option[Int]]](_.item, (b, i) => Box(i, b.tag))
    val inner = Lens[Box[Option[String]], Box[Option[Int]], Option[String], Option[Int]](_.item, (b, i) => Box(i, b.tag))
    val start = Box(Box(Some("four"): Option[String], "in"), "out")
    val walked = TypedZipper.Poly.of[Box[Box[Option[String]]], Box[Box[Option[Int]]]](start).down(outer).down(inner).downCase(Prism.some[String, Int])
    assertEquals(walked.map(_.focus), Some("four"))
    assertEquals(walked.map(_.modify(_.length)), Some(Box(Box(Some(4): Option[Int], "in"), "out")))
    val none = Box(Box(None: Option[String], "in"), "out")
    assertEquals(TypedZipper.Poly.of[Box[Box[Option[String]]], Box[Box[Option[Int]]]](none).down(outer).down(inner).downCase(Prism.some[String, Int]).map(_.focus), None)
  }

  test("Poly shares a prefix: two downs from one cursor, each set a whole with only its own edit") {
    val pa = Lens[Pair, Pair, String, String](_.a, (p, v) => p.copy(a = v))
    val pb = Lens[Pair, Pair, String, String](_.b, (p, v) => p.copy(b = v))
    val c = TypedZipper.Poly.of[Pair, Pair](Pair("a", "b"))
    assertEquals(c.down(pa).set("A"), Pair("A", "b"))
    assertEquals(c.down(pb).set("B"), Pair("a", "B"))
  }

  test("left/right on an element frame: typed siblings, None at the ends, identity both ways") {
    val e1 = TypedZipper(order).down(lines).at(1).get
    assertEquals(e1.right.map(_.focus), Some(order.lines(2)))
    assertEquals(e1.left.map(_.focus), Some(order.lines(0)))
    assertEquals(e1.left.flatMap(_.left), None)
    assertEquals(e1.right.flatMap(_.right), None)
    assertEquals(e1.right.flatMap(_.left).map(z => (z.focus, z.index)), Some((order.lines(1), 1)))
    assert(e1.right.get.root eq order, "a sideways move without edits rebuilt the whole")
  }

  test("an edit survives a sideways move, and up after it carries both edits") {
    val z = TypedZipper(order).down(lines).at(0).get.set(Line.Item("A", 1)).right.get.set(Line.Discount(50))
    assertEquals(z.root.lines, Vector(Line.Item("A", 1), Line.Discount(50), Line.Item("b", 1)))
    assertEquals(z.up.focus, Vector(Line.Item("A", 1), Line.Discount(50), Line.Item("b", 1)))
    assertEquals(z.asAffine.preview(order), Some(order.lines(1)))
    val walked = TypedZipper(order).down(lines).at(0).get.right.flatMap(_.right).map(_.focus)
    assertEquals(walked, TypedZipper(order).down(lines).at(2).map(_.focus))
  }

  test("pathKey: field, element and case frames spell the router's key; a lens frame has none") {
    val c = TypedZipper(order).field("customer").field("address")
    assertEquals(c.pathKey, Some("customer.address"))
    assertEquals(c.field("city").pathKey, Some("customer.address.city"))
    assertEquals(TypedZipper(order).pathKey, Some(""))
    assertEquals(TypedZipper(order).field("lines").at(1).map(_.pathKey), Some(Some("lines[1]")))
    assertEquals(TypedZipper(order).field("lines").at(1).flatMap(_.downCase[Line.Discount]).map(_.pathKey), Some(Some("lines[1]")))
    assertEquals(TypedZipper(order).down(customer).pathKey, None)
    assertEquals(TypedZipper(order).down(customer).field("name").pathKey, None)
    val key = TypedZipper(order).field("lines").at(2).get.downCase[Line.Item].get.field("sku")
    assertEquals(key.pathKey, Some("lines[2].sku"))
    assertEquals(key.asAffine.preview(order), Some("b"))
  }
}
