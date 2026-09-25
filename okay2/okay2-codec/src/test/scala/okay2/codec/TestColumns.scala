package okay2.codec

import Columns.{ColType, Field, Row}

sealed trait Tag
object Tag {
  case object Spend extends Tag
  case object Mint extends Tag
}
sealed trait Cred
object Cred {
  final case class KeyHash(hash: Array[Byte]) extends Cred
  final case class ScriptHash(hash: Array[Byte]) extends Cred
}
sealed trait DRep
object DRep {
  final case class Key(hash: Array[Byte]) extends DRep
  case object AlwaysAbstain extends DRep
}
final case class ColOut(id: Int, tag: Tag, cred: Cred, drep: DRep, note: Option[String],
                        qty: BigInt, parts: Vector[Long])

final case class ColTree(label: String, kids: Vector[ColTree])
object ColTree {
  implicit lazy val schema: Schema[ColTree] = Schema.derived
}
final case class ColA(b: Option[ColB])
final case class ColB(a: Vector[ColA], n: Int)
object ColA {
  implicit lazy val schema: Schema[ColA] = Schema.derived
}
object ColB {
  implicit lazy val schema: Schema[ColB] = Schema.derived
}

final case class ColBig(n: BigInt)

/** the tabular reading of a Schema, engine-free (specs/scalus.md §4) —
 * the same decisions okay2-spark's TestSparkSchema checks on Spark.
 * The recursive case differs from the Scala 3 core's own TestColumns:
 * `okay2-codec` has not ported `Cbor` (`Columns.scala`'s own comment),
 * so a recursive type's struct here carries `json` alone, not
 * `cbor`+`json`. */
class TestColumns extends munit.FunSuite {

  private val out = ColOut(7, Tag.Mint, Cred.ScriptHash(Array[Byte](9)), DRep.AlwaysAbstain, None,
    (BigInt(1) << 64) - 1, Vector(1L, 2L))

  test("the types: enum as Text, sum as kind + branches for cases WITH fields, option nullable, BigInt decimal") {
    val fs = Columns.fields[ColOut]
    def at(n: String) = fs.find(_.name == n).get
    assertEquals(at("tag").tpe, ColType.Text)
    assertEquals(at("drep").tpe, ColType.Struct(Vector(Field("kind", ColType.Text, false),
      Field("Key", ColType.Struct(Vector(Field("hash", ColType.Binary, false))), true))))
    assert(at("note").nullable)
    assertEquals(at("qty").tpe, ColType.Decimal(38, 0))
    assertEquals(at("parts").tpe, ColType.Arr(ColType.Int64, false))
  }

  test("the values: the case name, exactly one branch set, null for None") {
    val Row(vs) = Columns.row(out)
    assertEquals(vs(0), 7)
    assertEquals(vs(1), "Mint")
    vs(2) match {
      case Row(Vector("ScriptHash", null, Row(Vector(h: Array[Byte])))) => assertEquals(h.toList, List[Byte](9))
      case other => fail(s"cred: $other")
    }
    assertEquals(vs(3), Row(Vector("AlwaysAbstain", null)))
    assertEquals(vs(4), null)
    assertEquals(vs(5), (BigInt(1) << 64) - 1)
    assertEquals(vs(6), Vector(1L, 2L))
  }

  test("recursion by reachability, mutual included; the value is okay JSON (no Cbor here)") {
    assertEquals(Columns.recursiveNames(implicitly[Schema[ColTree]]), Set("ColTree"))
    assertEquals(Columns.recursiveNames(implicitly[Schema[ColA]]), Set("ColA", "ColB"))
    assertEquals(Columns.column(implicitly[Schema[ColTree]]).tpe, Columns.recursiveType)
    val t = ColTree("r", Vector(ColTree("a", Vector.empty)))
    Columns.column(implicitly[Schema[ColTree]]).write(t) match {
      case Row(Vector(json: Json)) =>
        assertEquals(Json.decode(implicitly[Schema[ColTree]])(json), Right(t))
      case other => fail(s"$other")
    }
  }

  test("a BigInt past 38 digits is refused, not rounded") {
    val e = intercept[IllegalArgumentException](Columns.row(ColBig(BigInt(10).pow(40))))
    assert(e.getMessage.contains("38 digits"))
  }

  test("a non-product root is one column named value") {
    assertEquals(Columns.fields[Tag], Vector(Field("value", ColType.Text, false)))
    // Scala 2 does not widen a case object to its sealed parent under
    // generic inference the way Scala 3 widens an enum case (`Tag.Spend`
    // alone would infer A = Spend.type, deriving a field-less PRODUCT
    // schema instead of reusing Tag's sum) — ascribe it.
    assertEquals(Columns.row(Tag.Spend: Tag), Row(Vector("Spend")))
  }
}
