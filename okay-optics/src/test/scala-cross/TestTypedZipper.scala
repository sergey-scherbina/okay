package okay

/**
 * specs/zipper.md, stage 2 — the typed zipper: frames are optics, the
 * parent's type comes back from `up`, and a program written against
 * a part runs at the focus while the cursor is parked in the whole.
 */
class TestTypedZipper extends munit.FunSuite {

  final case class Address(city: String, zip: Int)
  final case class Customer(name: String, address: Address)
  enum Line:
    case Item(sku: String, qty: Int)
    case Discount(pct: Int)
  final case class Order(id: Long, customer: Customer, lines: Vector[Line])

  val customer = Lens[Order](_.customer)
  val address = Lens[Customer](_.address)
  val city = Lens[Address](_.city)
  val name = Lens[Customer](_.name)
  val lines = Lens[Order](_.lines)

  val order = Order(7, Customer("ada", Address("Wrocław", 50001)),
    Vector(Line.Item("a", 2), Line.Discount(10), Line.Item("b", 1)))

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
    assertEquals(edited.customer.name, "ada")
    // and through modify, at a leaf lens
    val up = TypedZipper(order).down(customer).down(address).down(city).modify(_.toUpperCase).root
    assertEquals(up.customer.address.city, "WROCŁAW")
  }

  test("field by name is the Mirror lens as a frame; a wrong name does not compile") {
    val c = TypedZipper(order).field("customer")
    val cust: Customer = c.focus                                  // the field's declared type
    assertEquals(cust, order.customer)
    val z: Int = c.field("address").field("zip").focus
    assertEquals(z, 50001)
    assertEquals(c.field("address").field("city").set("Poznań").root.customer.address.city, "Poznań")
    val errors = compileErrors("""TypedZipper(order).field("customre")""")
    assert(errors.nonEmpty, "a misspelt field must be refused at compile time")
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
    // and up from the case is the Vector element, typed as the sum
    val back: TypedZipper.Below[Order, Vector[Line], Line, ?] = d.up
    assertEquals(back.focus, Line.Discount(10))
  }

  test("TypedZipper.focus: the lens laws, and a State % Customer program run while parked in an Order") {
    type C = TypedZipper.Below[Order, Order, Customer, TypedZipper.Top[Order]]
    val l = TypedZipper.focus[Order, Customer, C]
    val c: C = TypedZipper(order).down(customer)
    val v = Customer("v", Address("x", 1)); val w = Customer("w", Address("y", 2))
    def same(x: C, y: C) = x.focus == y.focus && x.root == y.root   // observational: the dirty flag is not the concept
    assert(same(l.set(l.get(c))(c), c))                          // GetPut
    assertEquals(l.get(l.set(v)(c)), v)                          // PutGet
    assert(same(l.set(w)(l.set(v)(c)), l.set(w)(c)))             // PutPut
    // the consumer the entry named: a program written against Customer
    val rename: Int ! State % Customer =
      State.modify[Customer](cu => cu.copy(name = cu.name.capitalize)).map(_.name.length)
    val (after, n) = !.run(State.handle(c)(State.zoom[C, Customer, Int, Nothing](l)(rename)))
    assertEquals(n, 3)
    assertEquals(after.root, order.copy(customer = order.customer.copy(name = "Ada")))
    assertEquals(after.up.focus.id, 7L)                           // the frames rode along
  }

  test("a walk shares its prefix: two branches from one down, each root shows only its own edit") {
    val c = TypedZipper(order).down(customer)
    val renamed = c.down(name).set("grace").root
    val moved = c.down(address).down(city).set("Gdańsk").root
    assertEquals(renamed.customer, Customer("grace", order.customer.address))
    assertEquals(moved.customer, Customer("ada", Address("Gdańsk", 50001)))
    assertEquals(c.root, order)
  }
}
