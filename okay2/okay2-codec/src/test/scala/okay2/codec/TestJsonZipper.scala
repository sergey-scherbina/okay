package okay2.codec

import okay2.optics.Zipper
import okay2.optics.Optic._
import Json._

/**
 * `Json` as a tree for the zipper (okay-codec's TestJsonZipper,
 * specs/zipper.md): the plate's contract, the two structural helpers,
 * and the round trip that ties a cursor's edit to the path optics.
 */
class TestJsonZipper extends munit.FunSuite {

  import JsonOptic.plate

  val rnd = new scala.util.Random(20260922)

  /** a random document, depth-bounded */
  def gen(depth: Int): Json =
    if (depth == 0) rnd.nextInt(4) match {
      case 0 => JNull
      case 1 => JBool(rnd.nextBoolean())
      case 2 => JNum(rnd.nextInt(100).toDouble)
      case _ => JStr(rnd.alphanumeric.take(3).mkString)
    }
    else rnd.nextInt(3) match {
      case 0 => JArr(Vector.fill(rnd.nextInt(4))(gen(depth - 1)))
      case 1 => JObj(Vector.tabulate(rnd.nextInt(4))(i => s"k$i" -> gen(depth - 1)))
      case _ => gen(0)
    }

  /** every root-first index path into `j` */
  def paths(j: Json, prefix: List[Int] = Nil): Vector[List[Int]] =
    prefix +: plate.children(j).zipWithIndex.flatMap { case (k, i) => paths(k, prefix :+ i) }

  /** the same path as the optics a user writes by hand */
  val here: Affine[Json, Json, Json, Json] = Affine[Json, Json, Json, Json](Right(_), (_, v) => v)
  def optic(j: Json, path: List[Int]): Affine[Json, Json, Json, Json] = path match {
    case Nil => here
    case i :: rest => j match {
      // bound before returning: Scala 2 infers `andThen`'s constraint
      // from an expected type in view, and refuses the affine
      case JArr(vs) => val o = JsonOptic.index(i).andThen(optic(vs(i), rest)); o
      case JObj(fs) => val o = JsonOptic.field(fs(i)._1).andThen(optic(fs(i)._2, rest)); o
      case _ => here
    }
  }

  val docs: Vector[Json] = Vector.fill(60)(gen(3))

  test("the plate: withChildren(t, children(t)) == t; keys keep their order; another arity keeps the node") {
    for (d <- docs; path <- paths(d)) {
      val node = Zipper(d).at(path).get.focus
      assertEquals(plate.withChildren(node, plate.children(node)), node)
    }
    val o: Json = JObj(Vector("a" -> JNum(1), "b" -> JNum(2)))
    assertEquals(plate.withChildren(o, Vector(JStr("x"), JStr("y"))), JObj(Vector("a" -> JStr("x"), "b" -> JStr("y"))): Json)
    assertEquals(plate.withChildren(o, Vector(JStr("x"))), o)
    assertEquals(plate.withChildren(JStr("s"), Vector(JNull)), JStr("s"): Json)
    assertEquals(plate.withChildren(JArr(Vector(JNull)), Vector()), JArr(Vector()): Json)
  }

  test("removeChild / insertChild: arrays and objects, order kept; identity elsewhere") {
    val ofs = Vector[(String, Json)]("a" -> JNum(1), "b" -> JNum(2), "c" -> JNum(3))
    val avs = Vector[Json](JNum(1), JNum(2), JNum(3))
    val o: Json = JObj(ofs)
    val a: Json = JArr(avs)
    assertEquals(JsonOptic.removeChild(o, 1), JObj(Vector("a" -> JNum(1), "c" -> JNum(3))): Json)
    assertEquals(JsonOptic.removeChild(a, 0), JArr(Vector(JNum(2), JNum(3))): Json)
    assertEquals(JsonOptic.removeChild(o, 3), o)
    assertEquals(JsonOptic.removeChild(JNum(1), 0), JNum(1): Json)
    assertEquals(JsonOptic.insertChild(o, 1, "x", JNull), JObj(Vector("a" -> JNum(1), "x" -> JNull, "b" -> JNum(2), "c" -> JNum(3))): Json)
    assertEquals(JsonOptic.insertChild(o, 3, "x", JNull), JObj(ofs :+ ("x" -> JNull)): Json)
    assertEquals(JsonOptic.insertChild(a, 3, "ignored", JNull), JArr(avs :+ JNull): Json)
    assertEquals(JsonOptic.insertChild(a, 4, "ignored", JNull), a)
    assertEquals(JsonOptic.insertChild(JBool(true), 0, "x", JNull), JBool(true): Json)
    for (d <- docs; path <- paths(d); z <- Zipper(d).at(path); i <- 0 to plate.children(z.focus).length) {
      val kind = z.focus match { case _: JArr | _: JObj => true; case _ => false }
      if (kind) assertEquals(JsonOptic.removeChild(JsonOptic.insertChild(z.focus, i, "new", JNull), i), z.focus)
    }
  }

  test("a cursor's edit is the path optic's edit, on every path of sixty documents") {
    val v: Json = JStr("edited")
    for (d <- docs; path <- paths(d)) {
      val viaZipper = Zipper(d).at(path).get.set(v).root
      val viaOptic = optic(d, path).set(v)(d)
      assertEquals(viaZipper, viaOptic, s"path $path of $d")
      assertEquals(Zipper.at[Json](path).preview(d), optic(d, path).preview(d))
    }
  }
}
