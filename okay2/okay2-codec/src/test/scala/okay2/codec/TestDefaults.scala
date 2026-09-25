package okay2.codec

final case class Job(name: String, retries: Int = 3, tag: Option[Int] = Some(5), dryRun: Boolean = false)
final case class Strict(id: String, count: Int)
final case class Computed(a: Int, b: Int = 7, c: String)
final case class Poly[T](x: T, xs: List[T] = Nil)

/** A field's declared default fills its absence (okay-codec's
 * TestDefaults, JSON half). One difference from Scala 3, on purpose:
 * a GENERIC product's default is callable here — the macro applies the
 * companion's default method to the type's own arguments — where Scala
 * 3's `Defaults` macro holds None and the field refuses. */
class TestDefaults extends munit.FunSuite {

  test("partial JSON: absent defaulted fields take their declarations") {
    assertEquals(Json.read[Job]("""{"name":"x"}"""), Right(Job("x", 3, Some(5), false)))
    assertEquals(Json.read[Job]("""{"name":"x","retries":9,"tag":1}"""), Right(Job("x", 9, Some(1), false)))
  }

  test("the default wins over None-if-optional: Option[Int] = Some(5)") {
    assertEquals(Json.read[Job]("""{"name":"x"}""").map(_.tag), Right(Some(5)))
  }

  test("absent undefaulted fields still refuse by name") {
    assertEquals(Json.read[Strict]("""{"id":"a"}"""), Left("missing field 'count' in Strict"))
    assertEquals(Json.read[Job]("""{"retries":1}"""), Left("missing field 'name' in Job"))
  }

  test("round-trip is untouched: the full wire decodes exactly") {
    val job = Job("y", 1, None, true)
    assertEquals(Json.read[Job](Json.write(job)), Right(job))
  }

  test("a default between undefaulted fields") {
    assertEquals(Json.read[Computed]("""{"a":1,"c":"z"}"""), Right(Computed(1, 7, "z")))
  }

  test("a generic product's default is applied to the type's arguments") {
    assertEquals(Json.read[Poly[Int]]("""{"x":1}"""), Right(Poly(1, Nil)))
    assertEquals(Json.read[Poly[String]]("""{"x":"a","xs":["b"]}"""), Right(Poly("a", List("b"))))
  }

  test("the strict reader applies the same defaults") {
    assertEquals(Json.readStrict[Job]("""{"name":"x"}"""), Right(Job("x", 3, Some(5), false)))
    assertEquals(Json.readStrict[Poly[Int]]("""{"x":1}"""), Right(Poly(1, Nil)))
  }
}
