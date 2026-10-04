package okay2.codec

import okay2.Optic._

final case class JoAddress(city: String, zip: Int)
object JoAddress { implicit lazy val schema: Schema[JoAddress] = Schema.derived }
final case class JoPerson(name: String, age: Int, address: JoAddress, tags: Vector[String])
object JoPerson { implicit lazy val schema: Schema[JoPerson] = Schema.derived }

sealed trait JoShape
object JoShape {
  final case class Circle(r: Double) extends JoShape
  final case class Square(side: Double) extends JoShape
  implicit lazy val schema: Schema[JoShape] = Schema.derived
}

/**
 * Optics over Json (okay-codec's TestJsonOptic): the laws of each, and
 * THE DRIFT LAW OF THE SECOND ORDER — for a derived schema, editing the
 * value and editing its JSON are the same edit, seen through the codec.
 */
class TestJsonOptic extends munit.FunSuite {

  /** a value as the Json the codec writes — through the codec's own road */
  def enc[A](a: A)(implicit s: Schema[A]): Json = Json.parse(Json.write(a))

  val ada = JoPerson("ada", 36, JoAddress("Warszawa", 12345), Vector("a", "b"))
  val rnd = new scala.util.Random(20260909)
  def obj(): Json = Json.JObj(Vector("x" -> Json.JNum(rnd.nextInt(99).toDouble), "y" -> Json.JStr(rnd.alphanumeric.take(3).mkString)))

  /** object fields sorted, recursively: one law below holds only in
   * RFC 8259's unordered reading */
  def ordered(j: Json): Json = j match {
    case Json.JObj(fs) => Json.JObj(fs.map { case (n, v) => (n, ordered(v)) }.sortBy(_._1))
    case Json.JArr(vs) => Json.JArr(vs.map(ordered))
    case other => other
  }

  val bump: Json => Json = { case Json.JNum(n) => Json.JNum(n + 1); case o => o }

  test("at: the lawful lens — GetPut, PutGet, PutPut on objects; absent is None, set(None) removes") {
    val a = JsonOptic.at("x")
    for (_ <- 1 to 100) {
      val j = obj()
      val v: Option[Json] = if (rnd.nextBoolean()) Some(Json.JNum(rnd.nextInt(9).toDouble)) else None
      val w: Option[Json] = if (rnd.nextBoolean()) Some(Json.JStr("w")) else None
      assertEquals(a.set(a.get(j))(j), j)
      assertEquals(a.get(a.set(v)(j)), v)
      assertEquals(ordered(a.set(w)(a.set(v)(j))), ordered(a.set(w)(j)))
      if (v.isDefined) assertEquals(a.set(w)(a.set(v)(j)), a.set(w)(j))
    }
    val j0 = Json.JObj(Vector("x" -> Json.JNum(1), "y" -> Json.JNum(2)))
    assertEquals(a.set(Some(Json.JNum(3)))(a.set(None)(j0)),
      Json.JObj(Vector("y" -> Json.JNum(2), "x" -> Json.JNum(3))))
    assertEquals(JsonOptic.at("nope").get(obj()), None)
    assertEquals(JsonOptic.at("x").set(None)(Json.JObj(Vector("x" -> Json.JNull))), Json.JObj(Vector.empty): Json)
    assertEquals(JsonOptic.at("z").set(Some(Json.JNull))(Json.JObj(Vector("x" -> Json.JNum(1)))),
      Json.JObj(Vector("x" -> Json.JNum(1), "z" -> Json.JNull)): Json)
  }

  test("field, index, caseOf: affines — a miss previews nothing and modifies nothing") {
    val j: Json = Json.JObj(Vector("a" -> Json.JNum(1)))
    assertEquals(JsonOptic.field("a").preview(j), Some(Json.JNum(1): Json))
    assertEquals(JsonOptic.field("b").preview(j), None)
    assertEquals(JsonOptic.field("b").set(Json.JNull)(j), j)
    assertEquals(JsonOptic.field("a").set(Json.JNull)(j), Json.JObj(Vector("a" -> Json.JNull)): Json)
    val arr: Json = Json.JArr(Vector(Json.JNum(1), Json.JNum(2)))
    assertEquals(JsonOptic.index(1).preview(arr), Some(Json.JNum(2): Json))
    assertEquals(JsonOptic.index(9).preview(arr), None)
    assertEquals(JsonOptic.index(9).set(Json.JNull)(arr), arr)
    assertEquals(JsonOptic.index(0).set(Json.JNull)(arr), Json.JArr(Vector(Json.JNull, Json.JNum(2))): Json)
    val sum: Json = Json.JObj(Vector("Circle" -> Json.JObj(Vector("r" -> Json.JNum(2)))))
    assertEquals(JsonOptic.caseOf("Circle").preview(sum).isDefined, true)
    assertEquals(JsonOptic.caseOf("Square").preview(sum), None)
    assertEquals(JsonOptic.caseOf("Square").set(Json.JNull)(sum), sum)
    // the wrong SHAPE is the identity too, which is what totality costs
    assertEquals(JsonOptic.field("a").set(Json.JNull)(Json.JNum(3)), Json.JNum(3): Json)
    assertEquals(JsonOptic.index(0).set(Json.JNull)(j), j)
  }

  test("values and entries: the traversal laws, and the keys are kept") {
    val arr: Json = Json.JArr(Vector(Json.JNum(1), Json.JNum(2), Json.JNum(3)))
    assertEquals(JsonOptic.values.modify(identity)(arr), arr)
    val g: Json => Json = { case Json.JNum(n) => Json.JNum(n * 2); case o => o }
    assertEquals(JsonOptic.values.modify(bump.andThen(g))(arr), JsonOptic.values.modify(bump).andThen(JsonOptic.values.modify(g))(arr))
    assertEquals(JsonOptic.values.toVector(arr), Vector[Json](Json.JNum(1), Json.JNum(2), Json.JNum(3)))
    val o: Json = Json.JObj(Vector("a" -> Json.JNum(1), "b" -> Json.JNum(2)))
    assertEquals(JsonOptic.entries.modify(bump)(o), Json.JObj(Vector("a" -> Json.JNum(2), "b" -> Json.JNum(3))): Json)
    assertEquals(JsonOptic.entries.toVector(o), Vector[Json](Json.JNum(1), Json.JNum(2)))
    assertEquals(JsonOptic.values.toVector(o), Vector.empty[Json])
  }

  test("THE DRIFT LAW: the value lens and the Json optic commute with the codec") {
    assertEquals(JsonOptic.field("name").set(Json.JStr("bob"))(enc(ada)), enc(Lens[JoPerson](_.name).set("bob")(ada)))
    assertEquals(JsonOptic.field("age").set(Json.JNum(7))(enc(ada)), enc(Lens[JoPerson](_.age).set(7)(ada)))
    // a NESTED field, through the composed optic on both sides
    val jsonCity = JsonOptic.field("address").andThen(JsonOptic.field("city"))
    val valueCity = Lens[JoPerson](_.address).andThen(Lens[JoAddress](_.city))
    assertEquals(jsonCity.set(Json.JStr("Wroclaw"))(enc(ada)), enc(valueCity.set("Wroclaw")(ada)))
    assertEquals(jsonCity.preview(enc(ada)), Some(Json.JStr(ada.address.city): Json))
    // a LIST, through the traversal on both sides
    val jsonTags = JsonOptic.field("tags").andThen(JsonOptic.values)
    val valueTags = Lens[JoPerson](_.tags).andThen(Traversal.each[String, String])
    assertEquals(jsonTags.modify { case Json.JStr(s) => Json.JStr(s.toUpperCase); case o => o }(enc(ada)),
      enc(valueTags.modify(_.toUpperCase)(ada)))
    assertEquals(jsonTags.toVector(enc(ada)), ada.tags.map(s => Json.JStr(s): Json))
  }

  test("THE DRIFT LAW for a sum: caseOf previews exactly when the value prism does") {
    for (s <- Vector[JoShape](JoShape.Circle(2.0), JoShape.Square(3.0))) {
      val onValue = Prism.of[JoShape, JoShape.Circle].preview(s)
      val onJson = JsonOptic.caseOf("Circle").preview(enc(s))
      assertEquals(onJson.isDefined, onValue.isDefined, s"the two carriers disagreed on which case $s is")
      assertEquals(onJson, onValue.map(c => enc(c)(Schema.derived[JoShape.Circle])))
    }
    val circle: JoShape = JoShape.Circle(2.0)
    val jsonR = JsonOptic.caseOf("Circle").andThen(JsonOptic.field("r"))
    assertEquals(jsonR.set(Json.JNum(9))(enc(circle)), enc(JoShape.Circle(9.0): JoShape))
    val square: JoShape = JoShape.Square(3.0)
    assertEquals(jsonR.set(Json.JNum(9))(enc(square)), enc(square))
    assertEquals(Prism.of[JoShape, JoShape.Circle].modify(_ => JoShape.Circle(9.0))(square), square)
  }

  test("the dotted path reads against the schema: fields, an index, a case, and the refusals") {
    val s = JoPerson.schema
    def at(k: String) = JsonOptic.path(s, k).get
    assertEquals(at("name").preview(enc(ada)), Some(Json.JStr("ada"): Json))
    assertEquals(at("address.city").preview(enc(ada)), Some(Json.JStr("Warszawa"): Json))
    assertEquals(at("tags[1]").preview(enc(ada)), Some(Json.JStr("b"): Json))
    assertEquals(at("tags[9]").preview(enc(ada)), None)
    assertEquals(at("address.city").set(Json.JStr("Kraków"))(enc(ada)),
      enc(Lens[JoPerson](_.address).andThen(Lens[JoAddress](_.city)).set("Kraków")(ada)))
    assertEquals(JsonOptic.path(s, "nosuch"), None)
    assertEquals(JsonOptic.path(s, "address.nosuch"), None)
    // the sum's extra level is the SCHEMA's knowledge, not the key's
    val shape = JoShape.schema
    assertEquals(JsonOptic.path(shape, "r").get.preview(enc(JoShape.Circle(2.0): JoShape)), Some(Json.JNum(2.0): Json))
    assertEquals(JsonOptic.path(shape, "$case").get.preview(enc(JoShape.Circle(2.0): JoShape)).isDefined, true)
  }
}
