package okay.flink

import okay.codec.Schema
import org.apache.flink.api.common.typeinfo.Types
import org.apache.flink.api.java.typeutils.RowTypeInfo
import org.apache.flink.types.Row

/** Columns as Flink types and rows (no cluster needed for the shape) */
class TestFlinkSchema extends munit.FunSuite:

  enum Cred derives Schema:
    case KeyHash(hash: Array[Byte])
    case ScriptHash(hash: Array[Byte])
  enum Tag derives Schema:
    case Spend, Mint
  final case class Tree(label: String, kids: Vector[Tree])
  given Schema[Tree] = Schema.derived
  final case class Out(id: Int, tag: Tag, cred: Cred, qty: BigInt, parts: Vector[Long], tree: Tree) derives Schema

  test("the row type: enum as STRING, sum as a row of kind + branches, decimal, arrays, recursion as json text") {
    val t = FlinkSchema.rowTypeOf[Out].asInstanceOf[RowTypeInfo]
    assertEquals(t.getFieldNames.toList, List("id", "tag", "cred", "qty", "parts", "tree"))
    assertEquals(t.getTypeAt(1), Types.STRING)
    assertEquals(t.getTypeAt(2).asInstanceOf[RowTypeInfo].getFieldNames.toList, List("kind", "KeyHash", "ScriptHash"))
    assertEquals(t.getTypeAt(3), Types.BIG_DEC)
    assertEquals(t.getTypeAt(4), Types.OBJECT_ARRAY(Types.LONG))
    assertEquals(t.getTypeAt(5).asInstanceOf[RowTypeInfo].getFieldNames.toList, List("cbor", "json"))
  }

  test("the rows: values of the types above, one branch set, json as text") {
    val r = FlinkSchema.rows(Seq(Out(1, Tag.Mint, Cred.ScriptHash(Array[Byte](9)), BigInt(5), Vector(1L, 2L),
      Tree("r", Vector.empty)))).head
    assertEquals(r.getField(1), "Mint")
    val cred = r.getField(2).asInstanceOf[Row]
    assertEquals(cred.getField(0), "ScriptHash")
    assertEquals(cred.getField(1), null)
    assertEquals(r.getField(3), java.math.BigDecimal.valueOf(5))
    assertEquals(r.getField(4).asInstanceOf[Array[java.lang.Long]].toList.map(_.longValue), List(1L, 2L))
    assert(r.getField(5).asInstanceOf[Row].getField(1).asInstanceOf[String].contains("\"label\""))
    // the row type accepts the row: Flink's own serializer round-trips it
    val ser = FlinkSchema.rowTypeOf[Out].createSerializer(new org.apache.flink.api.common.serialization.SerializerConfigImpl())
    assertEquals(ser.copy(r).getField(1), "Mint")
  }
