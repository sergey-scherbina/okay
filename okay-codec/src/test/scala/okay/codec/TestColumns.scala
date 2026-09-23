package okay.codec

import Columns.{ColType, Field, Row}

/** the tabular reading of a Schema, engine-free (specs/scalus.md §4) —
 * the same decisions okay-spark's TestSparkSchema checks on Spark */
class TestColumns extends munit.FunSuite:

  enum Tag derives Schema:
    case Spend, Mint
  enum Cred derives Schema:
    case KeyHash(hash: Array[Byte])
    case ScriptHash(hash: Array[Byte])
  enum DRep derives Schema:
    case Key(hash: Array[Byte])
    case AlwaysAbstain
  final case class Out(id: Int, tag: Tag, cred: Cred, drep: DRep, note: Option[String],
                       qty: BigInt, parts: Vector[Long]) derives Schema

  private val out = Out(7, Tag.Mint, Cred.ScriptHash(Array[Byte](9)), DRep.AlwaysAbstain, None,
    (BigInt(1) << 64) - 1, Vector(1L, 2L))

  test("the types: enum as Text, sum as kind + branches for cases WITH fields, option nullable, BigInt decimal") {
    val fs = Columns.fields[Out]
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
    vs(2) match
      case Row(Vector("ScriptHash", null, Row(Vector(h: Array[Byte])))) => assertEquals(h.toList, List[Byte](9))
      case other => fail(s"cred: $other")
    assertEquals(vs(3), Row(Vector("AlwaysAbstain", null)))
    assertEquals(vs(4), null)
    assertEquals(vs(5), (BigInt(1) << 64) - 1)
    assertEquals(vs(6), Vector(1L, 2L))
  }

  final case class Tree(label: String, kids: Vector[Tree])
  given Schema[Tree] = Schema.derived
  final case class A(b: Option[B])
  final case class B(a: Vector[A], n: Int)
  given Schema[A] = Schema.derived
  given Schema[B] = Schema.derived

  test("recursion by reachability, mutual included; the value is okay CBOR + its JSON") {
    assertEquals(Columns.recursiveNames(summon[Schema[Tree]]), Set("Tree"))
    assertEquals(Columns.recursiveNames(summon[Schema[A]]), Set("A", "B"))
    assertEquals(Columns.column(summon[Schema[Tree]]).tpe, Columns.recursiveType)
    val t = Tree("r", Vector(Tree("a", Vector.empty)))
    Columns.column(summon[Schema[Tree]]).write(t) match
      case Row(Vector(cbor: Array[Byte], json: Json)) =>
        assertEquals(Cbor.read[Tree](cbor), Right(t))
        assertEquals(Json.decode(summon[Schema[Tree]])(json), Right(t))
      case other => fail(s"$other")
  }

  final case class Big(n: BigInt) derives Schema
  test("a BigInt past 38 digits is refused, not rounded") {
    val e = intercept[IllegalArgumentException](Columns.row(Big(BigInt(10).pow(40))))
    assert(e.getMessage.contains("38 digits"))
  }

  test("a non-product root is one column named value") {
    assertEquals(Columns.fields[Tag], Vector(Field("value", ColType.Text, false)))
    assertEquals(Columns.row(Tag.Spend), Row(Vector("Spend")))
  }
