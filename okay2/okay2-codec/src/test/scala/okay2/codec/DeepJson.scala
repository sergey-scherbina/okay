package okay2.codec

/** Deep values built by loops, and compared by loops: structural `==`
 * on a 100 000-deep tree is its own StackOverflowError, unrelated to
 * the code under test. */
object DeepJson {
  def arrays(d: Int): String = ("[" * d) + "1" + ("]" * d)
  def objects(d: Int): String = ("{\"a\":" * d) + "1" + ("}" * d)
  def kidsChain(n: Int): String = ("{\"kids\":[" * n) + "{\"kids\":[]}" + ("]}" * n)

  def kidsValue(n: Int): Json = {
    var v: Json = Json.JObj(Vector("kids" -> Json.JArr(Vector.empty)))
    var i = 0
    while (i < n) { v = Json.JObj(Vector("kids" -> Json.JArr(Vector(v)))); i += 1 }
    v
  }

  def deepArr(n: Int): Json = {
    var j: Json = Json.JArr(Vector.empty)
    var i = 0
    while (i < n) { j = Json.JArr(Vector(j)); i += 1 }
    j
  }

  def deepObj(n: Int): Json = {
    var j: Json = Json.JObj(Vector.empty)
    var i = 0
    while (i < n) { j = Json.JObj(Vector("a" -> j)); i += 1 }
    j
  }

  /** containers nested one-in-one, counted */
  def depth(j: Json): Int = {
    var d = 0
    var at = j
    var go = true
    while (go) at match {
      case Json.JArr(Vector(one)) => d += 1; at = one
      case Json.JObj(Vector((_, one))) => d += 1; at = one
      case _ => go = false
    }
    d
  }

  def sameChain(a: Json, b: Json): Boolean = {
    var x = a
    var y = b
    var ok = true
    var go = true
    while (go) (x, y) match {
      case (Json.JArr(Vector(one)), Json.JArr(Vector(two))) => x = one; y = two
      case (Json.JObj(Vector((k1, one))), Json.JObj(Vector((k2, two)))) =>
        if (k1 != k2) { ok = false; go = false } else { x = one; y = two }
      case (Json.JArr(v1), Json.JArr(v2)) => ok = v1.isEmpty && v2.isEmpty; go = false
      case (Json.JObj(f1), Json.JObj(f2)) => ok = f1.isEmpty && f2.isEmpty; go = false
      case (Json.JNum(n1), Json.JNum(n2)) => ok = n1 == n2; go = false
      case _ => ok = false; go = false
    }
    ok
  }

  def kidsDepth(t: Kids): Int = {
    var d = 1
    var at = t
    while (at.kids.nonEmpty) { d += 1; at = at.kids.head }
    d
  }
}
