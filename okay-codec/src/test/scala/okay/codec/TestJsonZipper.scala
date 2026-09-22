package okay.codec

import okay.*
import okay.given
import Json.*

/**
 * specs/zipper.md — `Json` as a tree for the zipper: the plate's
 * contract, the two structural helpers, and the round trip that ties
 * a cursor's edit to the path optics `JsonOptic` already has.
 */
class TestJsonZipper extends munit.FunSuite {

  import JsonOptic.given

  val rnd = scala.util.Random(20260922)

  /** a random document, depth-bounded; every node a scalar, array or object */
  def gen(depth: Int): Json =
    if depth == 0 then rnd.nextInt(4) match
      case 0 => JNull
      case 1 => JBool(rnd.nextBoolean())
      case 2 => JNum(rnd.nextInt(100))
      case _ => JStr(rnd.alphanumeric.take(3).mkString)
    else rnd.nextInt(3) match
      case 0 => JArr(Vector.fill(rnd.nextInt(4))(gen(depth - 1)))
      case 1 => JObj(Vector.tabulate(rnd.nextInt(4))(i => s"k$i" -> gen(depth - 1)))
      case _ => gen(0)

  /** every root-first index path into `j` */
  def paths(j: Json, prefix: List[Int] = Nil): Vector[List[Int]] =
    prefix +: JsonOptic.plate.children(j).zipWithIndex.flatMap((k, i) => paths(k, prefix :+ i))

  /** the same path as the optics a user writes by hand: `index` into an
   * array, `field(name)` into an object — the name read off the document */
  val here: Affine[Json, Json, Json, Json] = Affine(Right(_), (_, v) => v)
  def optic(j: Json, path: List[Int]): Affine[Json, Json, Json, Json] = path match
    case Nil => here
    case i :: rest => j match
      case JArr(vs) => JsonOptic.index(i).andThen(optic(vs(i), rest))
      case JObj(fs) => JsonOptic.field(fs(i)._1).andThen(optic(fs(i)._2, rest))
      case _ => here

  val docs: Vector[Json] = Vector.fill(60)(gen(3))

  test("the plate: withChildren(t, children(t)) == t; keys keep their order; another arity keeps the node") {
    val p = JsonOptic.plate
    for d <- docs; path <- paths(d) do
      val node = Zipper(d).at(path).get.focus
      assertEquals(p.withChildren(node, p.children(node)), node)
    val o = JObj(Vector("a" -> JNum(1), "b" -> JNum(2)))
    assertEquals(p.withChildren(o, Vector(JStr("x"), JStr("y"))), JObj(Vector("a" -> JStr("x"), "b" -> JStr("y"))))
    assertEquals(p.withChildren(o, Vector(JStr("x"))), o)
    assertEquals(p.withChildren(JStr("s"), Vector(JNull)), JStr("s"))
    assertEquals(p.withChildren(JArr(Vector(JNull)), Vector()), JArr(Vector()))   // an array may shrink
  }

  test("removeChild / insertChild: arrays and objects, order kept; identity elsewhere") {
    val ofs = Vector("a" -> JNum(1), "b" -> JNum(2), "c" -> JNum(3))
    val avs = Vector(JNum(1), JNum(2), JNum(3))
    val o = JObj(ofs)
    val a = JArr(avs)
    assertEquals(JsonOptic.removeChild(o, 1), JObj(Vector("a" -> JNum(1), "c" -> JNum(3))))
    assertEquals(JsonOptic.removeChild(a, 0), JArr(Vector(JNum(2), JNum(3))))
    assertEquals(JsonOptic.removeChild(o, 3), o)
    assertEquals(JsonOptic.removeChild(JNum(1), 0), JNum(1))
    assertEquals(JsonOptic.insertChild(o, 1, "x", JNull), JObj(Vector("a" -> JNum(1), "x" -> JNull, "b" -> JNum(2), "c" -> JNum(3))))
    assertEquals(JsonOptic.insertChild(o, 3, "x", JNull), JObj(ofs :+ ("x" -> JNull)))
    assertEquals(JsonOptic.insertChild(a, 3, "ignored", JNull), JArr(avs :+ JNull))
    assertEquals(JsonOptic.insertChild(a, 4, "ignored", JNull), a)
    assertEquals(JsonOptic.insertChild(JBool(true), 0, "x", JNull), JBool(true))
    // remove after insert is the identity, at every position of a generated document
    for d <- docs; path <- paths(d); z <- Zipper(d).at(path); i <- 0 to JsonOptic.plate.children(z.focus).length do
      val kind = z.focus match { case _: JArr | _: JObj => true; case _ => false }
      if kind then assertEquals(JsonOptic.removeChild(JsonOptic.insertChild(z.focus, i, "new", JNull), i), z.focus)
  }

  test("a cursor's edit is the path optic's edit, on every path of sixty documents") {
    val v = JStr("edited")
    for d <- docs; path <- paths(d) do
      val viaZipper = Zipper(d).at(path).get.set(v).root
      val viaOptic = optic(d, path).set(v)(d)
      assertEquals(viaZipper, viaOptic, s"path $path of $d")
      assertEquals(Zipper.at[Json](path).preview(d), optic(d, path).preview(d))
  }
}
