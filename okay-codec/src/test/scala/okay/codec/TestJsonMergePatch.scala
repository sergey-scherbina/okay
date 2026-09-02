package okay.codec

import Json.*

/**
 * RFC 7396 JSON Merge Patch, exercised against the RFC's own test
 * appendix first (the eleven examples the spec ships), then the
 * caveat the doc comment names: sequential application and
 * "combine the patches first" disagree once a later patch deletes a
 * key only the target ever had.
 */
class TestJsonMergePatch extends munit.FunSuite {

  def j(s: String): Json = Json.parse(s)

  /** JSON objects are unordered; Json.JObj is a Vector, so raw `==`
   * is order-sensitive where the spec is not. Compare by content. */
  def norm(j: Json): Json = j match
    case JObj(fs) => JObj(fs.map((k, v) => (k, norm(v))).sortBy(_._1))
    case JArr(vs) => JArr(vs.map(norm))
    case other => other

  def sameJson(a: Json, b: Json)(using munit.Location): Unit =
    assertEquals(norm(a), norm(b))

  // RFC 7396 §3, verbatim
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
    ("""{}""", """{"a":{"bb":{"ccc":null}}}""", """{"a":{"bb":{}}}""")
  )

  test("RFC 7396 §3's own examples") {
    rfcExamples.foreach { (target, patch, expected) =>
      sameJson(Json.mergePatch(j(target), j(patch)), j(expected))
    }
  }

  test("a non-object patch always replaces, regardless of the target's shape") {
    assertEquals(Json.mergePatch(j("""{"a":1,"b":2}"""), JNum(5)), JNum(5))
    assertEquals(Json.mergePatch(JNull, JNum(5)), JNum(5))
    assertEquals(Json.mergePatch(JArr(Vector(JNum(1))), JStr("x")), JStr("x"))
  }

  test("nested merges recurse; a null at any depth deletes only its own key") {
    val target = j("""{"x":{"a":1,"b":2},"y":3}""")
    val patch = j("""{"x":{"a":null,"c":9}}""")
    sameJson(Json.mergePatch(target, patch), j("""{"x":{"b":2,"c":9},"y":3}"""))
  }

  test("deleting a key that is not present is a no-op") {
    sameJson(Json.mergePatch(j("""{"a":1}"""), j("""{"b":null}""")), j("""{"a":1}"""))
  }

  test("sequential application, three patches in a row") {
    val t0 = j("""{"a":1,"b":{"x":1,"y":2}}""")
    val t1 = Json.mergePatch(t0, j("""{"b":{"x":10}}"""))
    val t2 = Json.mergePatch(t1, j("""{"b":{"y":null}}"""))
    val t3 = Json.mergePatch(t2, j("""{"c":3}"""))
    sameJson(t3, j("""{"a":1,"b":{"x":10},"c":3}"""))
  }

  test("the documented caveat: combine-then-apply can disagree with apply-then-apply") {
    // p2 deletes "b", which only the ORIGINAL target ever set — p1
    // never mentions it, so a combined patch has nothing to delete
    val target = j("""{"x":{"a":1,"b":2}}""")
    val p1 = j("""{"x":{"a":10}}""")
    val p2 = j("""{"x":{"b":null}}""")

    val sequential = Json.mergePatch(Json.mergePatch(target, p1), p2)
    val combinedFirst = Json.mergePatch(target, Json.mergePatch(p1, p2))

    sameJson(sequential, j("""{"x":{"a":10}}""")) // b is gone
    sameJson(combinedFirst, j("""{"x":{"a":10,"b":2}}""")) // b survived
    assertNotEquals(norm(sequential), norm(combinedFirst),
      "if this ever passes, the caveat in Json.mergePatch's doc comment is stale")
  }
}
