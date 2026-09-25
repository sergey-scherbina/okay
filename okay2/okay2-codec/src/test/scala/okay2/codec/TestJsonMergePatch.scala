package okay2.codec

import Json._

/** RFC 7396 JSON Merge Patch against the RFC's own examples, then the
 * caveat: sequential application and "combine the patches first"
 * disagree once a later patch deletes a key only the target had
 * (okay-codec's TestJsonMergePatch). */
class TestJsonMergePatch extends munit.FunSuite {

  def j(s: String): Json = Json.parse(s)

  /** objects are unordered where JObj's Vector is not: compare by content */
  def norm(j: Json): Json = j match {
    case JObj(fs) => JObj(fs.map { case (k, v) => (k, norm(v)) }.sortBy(_._1))
    case JArr(vs) => JArr(vs.map(norm))
    case other => other
  }

  def sameJson(a: Json, b: Json)(implicit loc: munit.Location): Unit = assertEquals(norm(a), norm(b))

  val rfcExamples = Seq(
    ("""{"a":"b"}""", """{"a":"c"}""", """{"a":"c"}"""),
    ("""{"a":"b"}""", """{"b":"c"}""", """{"a":"b","b":"c"}"""),
    ("""{"a":"b"}""", """{"a":null}""", """{}"""),
    ("""{"a":"b","b":"c"}""", """{"a":null}""", """{"b":"c"}"""),
    ("""{"a":["b"]}""", """{"a":"c"}""", """{"a":"c"}"""),
    ("""{"a":"c"}""", """{"a":["b"]}""", """{"a":["b"]}"""),
    ("""{"a":{"b":"c"}}""", """{"a":{"b":"d","c":null}}""", """{"a":{"b":"d"}}"""),
    ("""{"a":[{"b":"c"}]}""", """{"a":[1]}""", """{"a":[1]}"""),
    ("""["a","b"]""", """["c","d"]""", """["c","d"]"""),
    ("""{"a":"b"}""", """["c"]""", """["c"]"""),
    ("""{"a":"foo"}""", """null""", """null"""),
    ("""{"a":"foo"}""", """"bar"""", """"bar""""),
    ("""{"e":null}""", """{"a":1}""", """{"e":null,"a":1}"""),
    ("""[1,2]""", """{"a":"b","c":null}""", """{"a":"b"}"""),
    ("""{}""", """{"a":{"bb":{"ccc":null}}}""", """{"a":{"bb":{}}}"""))

  test("RFC 7396 §3's own examples") {
    rfcExamples.foreach { case (target, patch, expected) => sameJson(Json.mergePatch(j(target), j(patch)), j(expected)) }
  }

  test("a non-object patch always replaces") {
    assertEquals(Json.mergePatch(j("""{"a":1,"b":2}"""), JNum(5)), JNum(5))
    assertEquals(Json.mergePatch(JNull, JNum(5)), JNum(5))
    assertEquals(Json.mergePatch(JArr(Vector(JNum(1))), JStr("x")), JStr("x"))
  }

  test("nested merges recurse; a null at any depth deletes only its own key") {
    sameJson(Json.mergePatch(j("""{"x":{"a":1,"b":2},"y":3}"""), j("""{"x":{"a":null,"c":9}}""")),
      j("""{"x":{"b":2,"c":9},"y":3}"""))
  }

  test("deleting a key that is not present is a no-op") {
    sameJson(Json.mergePatch(j("""{"a":1}"""), j("""{"b":null}""")), j("""{"a":1}"""))
  }

  test("the caveat: combine-then-apply can disagree with apply-then-apply") {
    val target = j("""{"x":{"a":1,"b":2}}""")
    val p1 = j("""{"x":{"a":10}}""")
    val p2 = j("""{"x":{"b":null}}""")
    val sequential = Json.mergePatch(Json.mergePatch(target, p1), p2)
    val combinedFirst = Json.mergePatch(target, Json.mergePatch(p1, p2))
    sameJson(sequential, j("""{"x":{"a":10}}"""))
    sameJson(combinedFirst, j("""{"x":{"a":10,"b":2}}"""))
  }
}
