package okay2.codec

import okay2.optics.Optic._

/**
 * The lawful way to create a missing parent (okay-codec's
 * TestCreatingPath, specs/optics.md stage 6): `Iso.non` moves absence
 * INTO the focus, and a router that creates parents becomes a
 * composition of two lawful optics.
 */
class TestCreatingPath extends munit.FunSuite {

  val empty: Json = Json.JObj(Vector.empty)
  def obj(fs: (String, Json)*): Json = Json.JObj(fs.toVector)

  test("non: absence reads as the default, and the default written back is absence") {
    val o = Iso.non(empty)
    assertEquals(o.get(None), empty)
    assertEquals(o.get(Some(Json.JNum(1))), Json.JNum(1): Json)
    assertEquals(o.set(Json.JNum(1))(None), Some(Json.JNum(1): Json))
    assertEquals(o.set(empty)(Some(Json.JNum(1))), None)
  }

  test("non is an iso modulo ONE normalisation, and here is that case") {
    val o = Iso.non(empty)
    assertEquals(o.set(o.get(Some(Json.JNum(7))))(None), Some(Json.JNum(7): Json))
    assertEquals(o.set(o.get(Some(empty)))(None), None)
  }

  val path: Lens[Json, Json, Json, Json] = JsonOptic.creatingObjects(List("a", "b", "c"))

  test("it creates every missing parent on the way down") {
    assertEquals(path.set(Json.JNum(1))(empty), obj("a" -> obj("b" -> obj("c" -> Json.JNum(1)))))
  }

  test("it reads a missing leaf as the default rather than failing") {
    assertEquals(path.get(empty), empty)
    assertEquals(path.get(obj("a" -> obj("b" -> obj("c" -> Json.JStr("x"))))), Json.JStr("x"): Json)
  }

  test("it leaves the siblings alone") {
    val start = obj("a" -> obj("keep" -> Json.JNum(0)), "other" -> Json.JBool(true))
    assertEquals(path.set(Json.JStr("v"))(start),
      obj("a" -> obj("keep" -> Json.JNum(0), "b" -> obj("c" -> Json.JStr("v"))), "other" -> Json.JBool(true)))
  }

  test("PutGet, on the domain where it holds: every named level an object or absent") {
    for {
      start <- List(empty, obj("a" -> empty), obj("a" -> obj("b" -> empty)),
        obj("a" -> obj("b" -> obj("c" -> Json.JNum(0)))), obj("z" -> Json.JNull))
      v <- List[Json](Json.JNum(1), Json.JStr("s"), Json.JBool(false))
    } assertEquals(path.get(path.set(v)(start)), v, s"start=$start v=$v")
  }

  test("and the domain's edge, named: a SCALAR where a parent belongs refuses the write") {
    val scalarParent = obj("a" -> Json.JNum(9))
    assertEquals(path.set(Json.JStr("v"))(scalarParent), scalarParent)
    assertEquals(path.get(scalarParent), empty)
  }

  test("GetPut: putting back what was read changes nothing, where the default is not stored") {
    val whole = obj("a" -> obj("b" -> obj("c" -> Json.JNum(3))), "z" -> Json.JNull)
    assertEquals(path.set(path.get(whole))(whole), whole)
  }

  test("PutPut: the second put wins") {
    val whole = obj("a" -> obj("b" -> obj("c" -> Json.JNum(3))))
    assertEquals(path.set(Json.JStr("y"))(path.set(Json.JStr("x"))(whole)), path.set(Json.JStr("y"))(whole))
  }

  test("writing the default PRUNES the whole spine, and that is the same law upwards") {
    val whole = obj("a" -> obj("b" -> obj("c" -> Json.JNum(3))))
    assertEquals(path.set(empty)(whole), empty)
    val withSibling = obj("a" -> obj("keep" -> Json.JNum(0), "b" -> obj("c" -> Json.JNum(3))))
    assertEquals(path.set(empty)(withSibling), obj("a" -> obj("keep" -> Json.JNum(0))))
  }

  test("where the parent exists, it agrees with the affine path that refuses to create") {
    val whole = obj("a" -> obj("b" -> obj("c" -> Json.JNum(3))))
    val affine = JsonOptic.field("a").andThen(JsonOptic.field("b")).andThen(JsonOptic.field("c"))
    assertEquals(path.get(whole), affine.preview(whole).get)
    assertEquals(path.set(Json.JNum(4))(whole), affine.set(Json.JNum(4))(whole))
  }

  test("modify goes through it too") {
    val whole = obj("a" -> obj("b" -> obj("c" -> Json.JNum(3))))
    assertEquals(path.modify { case Json.JNum(n) => Json.JNum(n + 1); case j => j }(whole),
      obj("a" -> obj("b" -> obj("c" -> Json.JNum(4)))))
  }

  test("a per-level default: an absent list level is an array, not an object") {
    val p = JsonOptic.creating(List(("xs", Json.JArr(Vector.empty)), ("0", empty)))
    assertEquals(JsonOptic.at("xs").andThen(Iso.non(Json.JArr(Vector.empty): Json)).get(empty), Json.JArr(Vector.empty): Json)
    assert(p.get(empty) == empty)
  }
}
