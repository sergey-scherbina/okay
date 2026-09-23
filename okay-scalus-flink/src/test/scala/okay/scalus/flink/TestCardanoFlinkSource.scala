package okay.scalus.flink

import _root_.okay.scalus.{CardanoTables, Recorded, Relays}
import _root_.okay.scalus.CardanoTables.Tables
import org.apache.flink.api.common.eventtime.WatermarkStrategy
import org.apache.flink.streaming.api.environment.StreamExecutionEnvironment
import scala.jdk.CollectionConverters.*

/** the FLIP-27 source on a real Flink MiniCluster, over the recorded
 * preprod session: its rows are CardanoTables' rows */
class TestCardanoFlinkSource extends munit.FunSuite:

  Relays.register("flink-recorded", () => Recorded.Replay())
  private val start = s"${Recorded.intersect.slot}:${Recorded.intersect.hash}:${Recorded.intersect.blockNo}"

  private lazy val direct: Tables =
    val session = _root_.okay.scalus.Session.open(Recorded.Replay(), 1).fold(e => fail(e), identity)
    val src = _root_.okay.scalus.ChainSyncSource(session, _root_.okay.scalus.CardanoNetwork.preprod, Some(Recorded.intersect))
    src.open().fold(e => fail(e), identity): Unit
    src.next().fold(e => fail(e), identity): Unit
    src.next().fold(e => fail(e), identity).collect { case _root_.okay.chain.Observed.Forward(b) => CardanoTables.of(b) }
      .foldLeft(Tables.empty)(_ ++ _)

  private def run(table: String, blocks: Int): List[org.apache.flink.types.Row] =
    val env = StreamExecutionEnvironment.getExecutionEnvironment
    env.setParallelism(1)
    val source = CardanoFlinkSource(CardanoConfig("registered:flink-recorded", "preprod", table,
      confirmations = 0, start = start, maxBlocks = Some(blocks)))
    env.fromSource(source, WatermarkStrategy.noWatermarks(), "cardano").executeAndCollect().asScala.toList

  test("a bounded job over five blocks: the outputs are CardanoTables' outputs") {
    val rows = run("outputs", 5)
    val names = CardanoFlinkSource(CardanoConfig("x", "preprod", "outputs")).getProducedType
      .asInstanceOf[org.apache.flink.api.java.typeutils.RowTypeInfo].getFieldNames.toList
    def i(n: String) = names.indexOf(n)
    val got = rows.map(r => (r.getFieldAs[String](i("txHash")), r.getFieldAs[Int](i("index")),
      r.getFieldAs[String](i("address")), r.getFieldAs[Long](i("lovelace")))).toSet
    val want = direct.outputs.map(o => (o.txHash, o.index, o.address, o.lovelace)).toSet
    assertEquals(got, want)
    assertEquals(rows.size, 11)
  }

  test("the split's state survives its serializer (what a Flink checkpoint stores)") {
    val s = ChainSplit(Some(Recorded.intersect), 3)
    assertEquals(ChainSplit.Serializer.deserialize(1, ChainSplit.Serializer.serialize(s)), s)
    assertEquals(ChainSplit.Serializer.deserialize(1, ChainSplit.Serializer.serialize(ChainSplit(None, 0))), ChainSplit(None, 0))
  }
