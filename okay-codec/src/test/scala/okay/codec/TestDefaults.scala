package okay.codec

/**
 * codec-defaults' claims, in the SHARED suite deliberately: the macro
 * runs in the compiler, the platforms only run the values it left
 * behind — so the same file passing on JVM, JS and Native is the
 * cross-platform proof.
 */
class TestDefaults extends munit.FunSuite {

  case class Job(name: String, retries: Int = 3, tag: Option[Int] = Some(5),
                 dryRun: Boolean = false) derives Schema
  case class Strict(id: String, count: Int) derives Schema
  case class Computed(a: Int, b: Int = 7, c: String) derives Schema

  test("partial JSON: absent defaulted fields take their declarations") {
    assertEquals(Json.read[Job]("""{"name":"x"}"""),
      Right(Job("x", 3, Some(5), false)))
    // a present field still wins over its default
    assertEquals(Json.read[Job]("""{"name":"x","retries":9,"tag":1}"""),
      Right(Job("x", 9, Some(1), false)))
  }

  test("the default wins over None-if-optional: Option[Int] = Some(5)") {
    val Right(job) = Json.read[Job]("""{"name":"x"}"""): @unchecked
    assertEquals(job.tag, Some(5))
  }

  test("absent undefaulted fields still refuse by name") {
    assertEquals(Json.read[Strict]("""{"id":"a"}"""),
      Left("missing field 'count' in Strict"))
    // and a defaulted product's OWN undefaulted field refuses too
    assertEquals(Json.read[Job]("""{"retries":1}"""),
      Left("missing field 'name' in Job"))
  }

  test("partial CBOR falls back the same way") {
    // encode a full value, then re-decode a hand-built partial map
    val bytes = Cbor.write(Strict("a", 1))   // proves the codec is alive here
    assert(Cbor.read[Strict](bytes).isRight)
    val partial = Cbor.write(PartialJob("x"))
    assertEquals(Cbor.read[Job](partial), Right(Job("x", 3, Some(5), false)))
  }
  // a one-field product whose single field NAME matches Job's — its
  // CBOR map is exactly "a Job wire missing three fields"
  case class PartialJob(name: String) derives Schema

  test("round-trip is untouched: the full wire decodes exactly as before") {
    val job = Job("y", 1, None, true)
    assertEquals(Json.read[Job](Json.write(job)), Right(job))
    assertEquals(Cbor.read[Job](Cbor.write(job)), Right(job))
  }

  test("an uncallable default stays honest: the macro holds None, decode refuses") {
    // `Computed.b = 7` is nullary — callable, so it falls back
    assertEquals(Json.read[Computed]("""{"a":1,"c":"z"}"""), Right(Computed(1, 7, "z")))
    // a generic product's default method takes TYPE parameters — not
    // callable with none in hand, so the macro answers None and the
    // absent field refuses instead of guessing
    assertEquals(Json.read[Poly[Int]]("""{"x":1}"""),
      Left("missing field 'xs' in Poly"))
  }
  case class Poly[T](x: T, xs: List[T] = Nil)
  given Schema[Poly[Int]] = Schema.derived
}
