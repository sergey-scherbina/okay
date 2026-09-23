package okay.codec

/** the snippet in docs/modules/okay-codec.md ("Columns"), VERBATIM */
class TestDocExamplesColumns extends munit.FunSuite:

  enum Credential derives Schema:
    case KeyHash(hash: Array[Byte])
    case ScriptHash(hash: Array[Byte])
  final case class Output(id: Int, owner: Credential, lovelace: BigInt) derives Schema

  test("docs: a sum as kind + branches, engine-free") {
    // ---- snippet begins
    val (fields, toRow) = Columns.table[Output]
    // Vector(Field(id,Int32,false), Field(owner,Struct(Vector(Field(kind,Text,false),
    //   Field(KeyHash,Struct(...),true), Field(ScriptHash,Struct(...),true))),false),
    //   Field(lovelace,Decimal(38,0),false))
    val row = toRow(Output(2, Credential.ScriptHash(Array[Byte](2)), BigInt(5_000_000)))
    // Row(Vector(2, Row(Vector(ScriptHash, null, Row(Vector([2])))), 5000000))
    // ---- snippet ends
    assertEquals(fields.map(_.name), Vector("id", "owner", "lovelace"))
    row.values(1) match
      case Columns.Row(Vector("ScriptHash", null, Columns.Row(Vector(_: Array[Byte])))) => ()
      case other => fail(s"$other")
    assertEquals(row.values(2), BigInt(5_000_000))
  }
