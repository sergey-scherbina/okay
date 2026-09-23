package scala2probe

import okay.scala2._

object OpticsModel {
  final case class Address(city: String, zip: String)
  final case class Person(name: String, address: Address, pet: Option[String])
  final case class Item(name: String, price: Int)
  final case class Order(id: String, items: Vector[Item])

  sealed trait Shape
  final case class Circle(r: Double) extends Shape
  final case class Rect(w: Double, h: Double) extends Shape

  val address = Lens[Person, Address](_.address, (p, a) => p.copy(address = a))
  val city = Lens[Address, String](_.city, (a, c) => a.copy(city = c))
  val pet = Lens[Person, Option[String]](_.pet, (p, x) => p.copy(pet = x))
  val items = Lens[Order, Vector[Item]](_.items, (o, is) => o.copy(items = is))
  val price = Lens[Item, Int](_.price, (i, p) => i.copy(price = p))
  val radius = Lens[Circle, Double](_.r, (c, r) => c.copy(r = r))
}

/** okay-optics from Scala 2.13 (specs/scala2-facade.md, stage 15.6) */
class TestOpticsFromScala2 extends munit.FunSuite {
  import OpticsModel._

  val ada = Person("Ada", Address("London", "N1"), None)

  test("a lens composed with a lens is a lens, and it keeps the laws") {
    val personCity = address.andThen(city)
    assertEquals(personCity.get(ada), "London")
    assertEquals(personCity.set("Paris")(ada).address, Address("Paris", "N1"))
    assertEquals(personCity.modify(_.toUpperCase)(ada).address.city, "LONDON")
    assertEquals(personCity.get(personCity.set("Rome")(ada)), "Rome")
    assertEquals(personCity.set(personCity.get(ada))(ada), ada)
  }

  test("a prism picks one case of a sealed hierarchy; review builds it") {
    val circle = Prism.subtype[Shape, Circle]
    val shapes = Vector[Shape](Circle(1.0), Rect(2.0, 3.0))
    assertEquals(shapes.map(circle.preview), Vector(Some(Circle(1.0)), None))
    assertEquals(shapes.map(circle.andThen(radius).modify(_ * 10)), Vector[Shape](Circle(10.0), Rect(2.0, 3.0)))
    assertEquals(circle.review(Circle(5.0)), Circle(5.0): Shape)
  }

  test("a lens then a prism is an affine: set where the part is, unchanged where it is not") {
    val petName = pet.andThen(Prism.some[String])
    assertEquals(petName.preview(ada), None)
    assertEquals(petName.set("Rex")(ada), ada)
    val withPet = ada.copy(pet = Some("rex"))
    assertEquals(petName.modify(_.capitalize)(withPet).pet, Some("Rex"))
  }

  test("a traversal reaches every part, in order") {
    val order = Order("o1", Vector(Item("a", 10), Item("b", 25)))
    val prices = items.andThen(Traversal.each[Item]).andThen(price)
    assertEquals(prices.toVector(order), Vector(10, 25))
    assertEquals(prices.modify(_ * 2)(order).items.map(_.price), Vector(20, 50))
    val words = Traversal[String, String](_.split(" ").toVector, (_, ws) => ws.mkString(" "))
    assertEquals(words.modify(_.reverse)("ab cd"), "ba dc")
  }

  test("an iso converts both ways, and an iso then a prism reviews through both") {
    val celsius = Iso[Double, Double](f => (f - 32) * 5 / 9, c => c * 9 / 5 + 32)
    assertEquals(celsius.get(212.0), 100.0)
    assertEquals(celsius.reverseGet(0.0), 32.0)
    assertEquals(celsius.modify(_ + 10)(32.0), 50.0)
    val asText = Iso[Option[String], Option[String]](identity, identity).andThen(Prism.some[String])
    assertEquals((asText.review("x"), asText.preview(None)), (Some("x"), None))
  }
}
