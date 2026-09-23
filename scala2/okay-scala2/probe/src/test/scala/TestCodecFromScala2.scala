package scala2probe

import okay.codec.{Cbor, Schema}
import okay.scala2.{Json, JsonSchema, Schemas}

object CodecModel {
  final case class Person(name: String, age: Int, email: Option[String], tags: List[String])
  object Person {
    implicit val schema: Schema[Person] =
      Schemas.product4("Person", "name", "age", "email", "tags")(Person.apply)(p => (p.name, p.age, p.email, p.tags))
  }

  sealed trait Shape
  final case class Circle(r: Double) extends Shape
  final case class Rect(w: Double, h: Double) extends Shape
  case object Empty extends Shape
  object Shape {
    implicit val circle: Schema[Circle] = Schemas.product1("Circle", "r")(Circle.apply)(_.r)
    implicit val rect: Schema[Rect] = Schemas.product2("Rect", "w", "h")(Rect.apply)(r => (r.w, r.h))
    implicit val empty: Schema[Empty.type] = Schemas.constant("Empty", Empty)
    implicit val schema: Schema[Shape] = Schemas.sum[Shape]("Shape")(
      Schemas.variant[Shape, Circle]("Circle"),
      Schemas.variant[Shape, Rect]("Rect"),
      Schemas.variant[Shape, Empty.type]("Empty"))
  }

  final case class Tree(label: String, kids: List[Tree])
  object Tree {
    implicit lazy val schema: Schema[Tree] = Schemas.product2("Tree", "label", "kids")(Tree.apply)(t => (t.label, t.kids))
  }
}

/** okay-codec from Scala 2.13 (specs/scala2-facade.md, stage 6) */
class TestCodecFromScala2 extends munit.FunSuite {
  import CodecModel._

  val ada = Person("ada", 36, Some("ada@example.org"), List("math", "engines"))

  test("a case class round-trips through Json and Cbor") {
    val text = Json.write(ada)
    assertEquals(text, """{"name":"ada","age":36,"email":"ada@example.org","tags":["math","engines"]}""")
    assertEquals(Json.read[Person](text), Right(ada))
    assertEquals(Cbor.read[Person](Cbor.write(ada)), Right(ada))
  }

  test("an absent optional field reads as None") {
    assertEquals(Json.read[Person]("""{"name":"bob","age":1,"tags":[]}"""), Right(Person("bob", 1, None, Nil)))
  }

  test("a sealed hierarchy round-trips, a case object included") {
    val shapes: List[Shape] = List(Circle(1.5), Rect(2, 3), Empty)
    for (s <- shapes) {
      assertEquals(Json.read[Shape](Json.write[Shape](s)), Right(s))
      assertEquals(Cbor.read[Shape](Cbor.write[Shape](s)), Right(s))
    }
    assertEquals(Json.write[Shape](Circle(1.5)), """{"Circle":{"r":1.5}}""")
  }

  test("a recursive type round-trips") {
    val t = Tree("a", List(Tree("b", Nil), Tree("c", List(Tree("d", Nil)))))
    assertEquals(Json.read[Tree](Json.write(t)), Right(t))
  }

  test("JsonSchema renders the declaration") {
    val s = JsonSchema.of(Person.schema)
    assert(s.contains("\"name\""), s)
    assert(s.contains("\"required\""), s)
  }

  test("a decode error is a Left saying what was expected and what came") {
    // okay-codec's own message, the same from Scala 3: it names the
    // expected schema and the value found, not the field's path
    assertEquals(Json.read[Person]("""{"name":"x","age":"old","tags":[]}"""), Left("expected SInt, got JStr(old)"))
    assert(Json.readStrict[Person]("""{"name":"x","age":1,"tags":[]} trailing""").isLeft)
  }
}
