package okay.codec

import okay.{Iso, Lens, get, set, modify, preview}
import okay.given

/**
 * The lawful way to create a missing parent (specs/optics.md stage 6).
 *
 * The finding this closes: a router that invents a parent during `set`
 * is an unlawful lens, which is why `JsonOptic` refused to have one.
 * `Iso.non` moves absence INTO the focus, and then the same behaviour
 * is a composition of two lawful optics.
 */
class TestCreatingPath extends munit.FunSuite {

  val empty: Json = Json.JObj(Vector.empty)
  def obj(fs: (String, Json)*): Json = Json.JObj(fs.toVector)

  // ---------------------------------------------------------------- non

  test("non: absence reads as the default, and the default written back is absence") {
    val o = Iso.non(empty)
    assertEquals(o.get(None), empty)
    assertEquals(o.get(Some(Json.JNum(1))), Json.JNum(1))
    assertEquals(o.set(Json.JNum(1))(None), Some(Json.JNum(1)))
    assertEquals(o.set(empty)(Some(Json.JNum(1))), None)
  }

  test("non is an iso modulo ONE normalisation, and here is that case") {
    val o = Iso.non(empty)
    // for anything that is not the default, the round trip is exact
    assertEquals(o.set(o.get(Some(Json.JNum(7))))(None), Some(Json.JNum(7)))
    // and `Some(default)` collapses to absence — the stated caveat
    assertEquals(o.set(o.get(Some(empty)))(None), None)
  }

  // ---------------------------------------------------------------- the path

  val path: Lens[Json, Json, Json, Json] = JsonOptic.creatingObjects(List("a", "b", "c"))

  test("it creates every missing parent on the way down") {
    assertEquals(path.set(Json.JNum(1))(empty),
      obj("a" -> obj("b" -> obj("c" -> Json.JNum(1)))))
  }

  test("it reads a missing leaf as the default rather than failing") {
    assertEquals(path.get(empty), empty)
    assertEquals(path.get(obj("a" -> obj("b" -> obj("c" -> Json.JStr("x"))))), Json.JStr("x"))
  }

  test("it leaves the siblings alone") {
    val start = obj("a" -> obj("keep" -> Json.JNum(0)), "other" -> Json.JBool(true))
    assertEquals(path.set(Json.JStr("v"))(start),
      obj("a" -> obj("keep" -> Json.JNum(0), "b" -> obj("c" -> Json.JStr("v"))),
          "other" -> Json.JBool(true)))
  }

  test("PutGet, on the domain where it holds: every named level an object or absent") {
    for start <- List(empty, obj("a" -> empty), obj("a" -> obj("b" -> empty)),
                      obj("a" -> obj("b" -> obj("c" -> Json.JNum(0)))), obj("z" -> Json.JNull))
        v <- List(Json.JNum(1), Json.JStr("s"), Json.JBool(false))
    do assertEquals(path.get(path.set(v)(start)), v, s"start=$start v=$v")
  }

  test("and the domain's edge, named: a SCALAR where a parent belongs refuses the write") {
    // `at` answers `case other => other` for a non-object, so the write
    // is refused rather than clobbering the scalar. That is a choice
    // JsonOptic made before this path existed, and this path inherits
    // it: a number at "a" is not a container, and nothing here will
    // silently make it one.
    val scalarParent = obj("a" -> Json.JNum(9))
    assertEquals(path.set(Json.JStr("v"))(scalarParent), scalarParent)
    assertEquals(path.get(scalarParent), empty)   // and reads as the default
  }

  test("GetPut: putting back what was read changes nothing, where the default is not stored") {
    val whole = obj("a" -> obj("b" -> obj("c" -> Json.JNum(3))), "z" -> Json.JNull)
    assertEquals(path.set(path.get(whole))(whole), whole)
  }

  test("PutPut: the second put wins") {
    val whole = obj("a" -> obj("b" -> obj("c" -> Json.JNum(3))))
    assertEquals(path.set(Json.JStr("y"))(path.set(Json.JStr("x"))(whole)),
      path.set(Json.JStr("y"))(whole))
  }

  test("writing the default PRUNES the whole spine, and that is the same law upwards") {
    // absence and the default are one point at EVERY level, so removing
    // the leaf makes its parent equal to the default, which removes the
    // parent, and so on up. Creation on the way down and pruning on the
    // way up are one law read in two directions.
    val whole = obj("a" -> obj("b" -> obj("c" -> Json.JNum(3))))
    assertEquals(path.set(empty)(whole), empty)
    // a sibling anywhere on the spine stops the pruning exactly there
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
    // the array default is what an absent `xs` reads as
    assertEquals(JsonOptic.at("xs").andThen(Iso.non(Json.JArr(Vector.empty))).get(empty),
      Json.JArr(Vector.empty))
    assert(p.get(empty) == empty)
  }
}
