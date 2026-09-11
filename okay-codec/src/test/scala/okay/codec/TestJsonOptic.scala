package okay.codec

import okay.*
import okay.given

/**
 * Optics over Json (specs/optics.md stage 1): the laws of each, and
 * THE DRIFT LAW OF THE SECOND ORDER — for a derived schema, editing
 * the value and editing its JSON are the same edit, seen through the
 * codec. `ui-toolkit` made a form unable to drift from its parser;
 * this makes an EDIT unable to drift from the wire.
 */
class TestJsonOptic extends munit.FunSuite {

  final case class Address(city: String, zip: Int)
  final case class Person(name: String, age: Int, address: Address, tags: Vector[String])
  given Schema[Address] = Schema.derived
  given Schema[Person] = Schema.derived

  enum Shape derives Schema:
    case Circle(r: Double)
    case Square(side: Double)

  /** a value as the Json the codec writes — through the codec's own road */
  def enc[A](a: A)(using s: Schema[A]): Json = Json.parse(Json.write(a))

  val ada = Person("ada", 36, Address("Warszawa", 12345), Vector("a", "b"))
  val rnd = scala.util.Random(20260909)
  def obj(): Json = Json.JObj(Vector("x" -> Json.JNum(rnd.nextInt(99)), "y" -> Json.JStr(rnd.alphanumeric.take(3).mkString)))

  /** object fields sorted, recursively: JSON objects are unordered by
   * RFC 8259, and one law below holds only in that reading */
  def ordered(j: Json): Json = j match
    case Json.JObj(fs) => Json.JObj(fs.map((n, v) => (n, ordered(v))).sortBy(_._1))
    case Json.JArr(vs) => Json.JArr(vs.map(ordered))
    case other => other

  test("at: the lawful lens — GetPut, PutGet, PutPut on objects; absent is None, set(None) removes") {
    val a = JsonOptic.at("x")
    for _ <- 1 to 100 do
      val j = obj()
      val v: Option[Json] = if rnd.nextBoolean() then Some(Json.JNum(rnd.nextInt(9))) else None
      val w: Option[Json] = if rnd.nextBoolean() then Some(Json.JStr("w")) else None
      assertEquals(a.set(a.get(j))(j), j)              // GetPut, exactly
      assertEquals(a.get(a.set(v)(j)), v)              // PutGet, exactly
      // PutPut is exact unless the first put REMOVED the field: a
      // removal loses where it was and the next insert appends, so the
      // law reads under RFC 8259's unordered objects
      assertEquals(ordered(a.set(w)(a.set(v)(j))), ordered(a.set(w)(j)))
      if v.isDefined then assertEquals(a.set(w)(a.set(v)(j)), a.set(w)(j))   // and exactly, when nothing was removed
    // the order the weakening is about, named
    val j0 = Json.JObj(Vector("x" -> Json.JNum(1), "y" -> Json.JNum(2)))
    assertEquals(a.set(Some(Json.JNum(3)))(a.set(None)(j0)),
      Json.JObj(Vector("y" -> Json.JNum(2), "x" -> Json.JNum(3))))
    assertEquals(JsonOptic.at("nope").get(obj()), None)
    assertEquals(JsonOptic.at("x").set(None)(Json.JObj(Vector("x" -> Json.JNull))), Json.JObj(Vector.empty))
    // the field is inserted at the end when it was absent, in place when it was there
    assertEquals(JsonOptic.at("z").set(Some(Json.JNull))(Json.JObj(Vector("x" -> Json.JNum(1)))),
      Json.JObj(Vector("x" -> Json.JNum(1), "z" -> Json.JNull)))
  }

  test("field, index, caseOf: affines — a miss previews nothing and modifies nothing") {
    val j = Json.JObj(Vector("a" -> Json.JNum(1)))
    assertEquals(JsonOptic.field("a").preview(j), Some(Json.JNum(1)))
    assertEquals(JsonOptic.field("b").preview(j), None)
    assertEquals(JsonOptic.field("b").set(Json.JNull)(j), j)                 // the miss changes nothing
    assertEquals(JsonOptic.field("a").set(Json.JNull)(j), Json.JObj(Vector("a" -> Json.JNull)))
    val arr = Json.JArr(Vector(Json.JNum(1), Json.JNum(2)))
    assertEquals(JsonOptic.index(1).preview(arr), Some(Json.JNum(2)))
    assertEquals(JsonOptic.index(9).preview(arr), None)
    assertEquals(JsonOptic.index(9).set(Json.JNull)(arr), arr)
    assertEquals(JsonOptic.index(0).set(Json.JNull)(arr), Json.JArr(Vector(Json.JNull, Json.JNum(2))))
    val sum = Json.JObj(Vector("Circle" -> Json.JObj(Vector("r" -> Json.JNum(2)))))
    assertEquals(JsonOptic.caseOf("Circle").preview(sum).isDefined, true)
    assertEquals(JsonOptic.caseOf("Square").preview(sum), None)
    assertEquals(JsonOptic.caseOf("Square").set(Json.JNull)(sum), sum)
    // the wrong SHAPE is the identity too, which is what totality costs
    assertEquals(JsonOptic.field("a").set(Json.JNull)(Json.JNum(3)), Json.JNum(3))
    assertEquals(JsonOptic.index(0).set(Json.JNull)(j), j)
  }

  test("values and entries: the traversal laws, and the keys are kept") {
    val arr = Json.JArr(Vector(Json.JNum(1), Json.JNum(2), Json.JNum(3)))
    assertEquals(JsonOptic.values.modify(identity)(arr), arr)
    val f = (j: Json) => j match { case Json.JNum(n) => Json.JNum(n + 1); case o => o }
    val g = (j: Json) => j match { case Json.JNum(n) => Json.JNum(n * 2); case o => o }
    assertEquals(JsonOptic.values.modify(f.andThen(g))(arr), JsonOptic.values.modify(f).andThen(JsonOptic.values.modify(g))(arr))
    assertEquals(JsonOptic.values.toVector(arr), Vector(Json.JNum(1), Json.JNum(2), Json.JNum(3)))
    val o = Json.JObj(Vector("a" -> Json.JNum(1), "b" -> Json.JNum(2)))
    assertEquals(JsonOptic.entries.modify(f)(o), Json.JObj(Vector("a" -> Json.JNum(2), "b" -> Json.JNum(3))))
    assertEquals(JsonOptic.entries.toVector(o), Vector(Json.JNum(1), Json.JNum(2)))
    assertEquals(JsonOptic.values.toVector(o), Vector.empty)   // not an array: no foci
  }

  test("THE DRIFT LAW: the value lens and the Json optic commute with the codec") {
    // a field: set on the value, set on its Json, the same value out
    for (name, v) <- Vector(
      ("name", Json.JStr("bob")), ("age", Json.JNum(7))) do
      val onJson = JsonOptic.field(name).set(v)(enc(ada))
      val onValue = name match
        case "name" => enc(Lens[Person](_.name).set("bob")(ada))
        case _ => enc(Lens[Person](_.age).set(7)(ada))
      assertEquals(onJson, onValue, s"the two carriers disagreed on $name")
    // a NESTED field, through the composed optic on both sides
    val jsonCity = JsonOptic.field("address").andThen(JsonOptic.field("city"))
    val valueCity = Lens[Person](_.address).andThen(Lens[Address](_.city))
    assertEquals(jsonCity.set(Json.JStr("Wroclaw"))(enc(ada)), enc(valueCity.set("Wroclaw")(ada)))
    assertEquals(jsonCity.preview(enc(ada)), Some(Json.JStr(ada.address.city)))
    // a LIST, through the traversal on both sides
    val jsonTags = JsonOptic.field("tags").andThen(JsonOptic.values)
    val valueTags = Lens[Person](_.tags).andThen(Traversal.each[String, String])
    assertEquals(jsonTags.modify { case Json.JStr(s) => Json.JStr(s.toUpperCase); case o => o }(enc(ada)),
      enc(valueTags.modify(_.toUpperCase)(ada)))
    assertEquals(jsonTags.toVector(enc(ada)), ada.tags.map(Json.JStr(_)))
  }

  test("THE DRIFT LAW for a sum: caseOf previews exactly when the value prism does") {
    for s <- Vector[Shape](Shape.Circle(2.0), Shape.Square(3.0)) do
      val onValue = Prism.of[Shape, Shape.Circle].preview(s)
      val onJson = JsonOptic.caseOf("Circle").preview(enc(s))
      assertEquals(onJson.isDefined, onValue.isDefined, s"the two carriers disagreed on which case $s is")
      assertEquals(onJson, onValue.map(enc(_)(using summon[Schema[Shape.Circle]])))
    // and setting through the case commutes
    val circle: Shape = Shape.Circle(2.0)
    val jsonR = JsonOptic.caseOf("Circle").andThen(JsonOptic.field("r"))
    assertEquals(jsonR.set(Json.JNum(9))(enc(circle)), enc(Shape.Circle(9.0): Shape))
    // the miss on both sides leaves the whole alone
    val square: Shape = Shape.Square(3.0)
    assertEquals(jsonR.set(Json.JNum(9))(enc(square)), enc(square))
    assertEquals(Prism.of[Shape, Shape.Circle].modify(_ => Shape.Circle(9.0))(square), square)
  }

  test("the dotted path reads against the schema: fields, an index, a case, and the refusals") {
    val s = summon[Schema[Person]]
    def at(k: String) = JsonOptic.path(s, k).get
    assertEquals(at("name").preview(enc(ada)), Some(Json.JStr("ada")))
    assertEquals(at("address.city").preview(enc(ada)), Some(Json.JStr("Warszawa")))
    assertEquals(at("tags[1]").preview(enc(ada)), Some(Json.JStr("b")))
    assertEquals(at("tags[9]").preview(enc(ada)), None)
    assertEquals(at("address.city").set(Json.JStr("Kraków"))(enc(ada)),
      enc(Lens[Person](_.address).andThen(Lens[Address](_.city)).set("Kraków")(ada)))
    // a key this schema does not write names nothing
    assertEquals(JsonOptic.path(s, "nosuch"), None)
    assertEquals(JsonOptic.path(s, "address.nosuch"), None)
    // the sum's extra level is the SCHEMA's knowledge, not the key's
    val shape = summon[Schema[Shape]]
    assertEquals(JsonOptic.path(shape, "r").get.preview(enc(Shape.Circle(2.0): Shape)), Some(Json.JNum(2.0)))
    assertEquals(JsonOptic.path(shape, "$case").get.preview(enc(Shape.Circle(2.0): Shape)).isDefined, true)
  }
}
