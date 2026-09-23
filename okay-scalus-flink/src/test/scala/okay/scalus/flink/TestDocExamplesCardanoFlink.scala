package okay.scalus.flink

import _root_.okay.scalus.{Recorded, Relays}
import org.apache.flink.api.common.eventtime.WatermarkStrategy
import org.apache.flink.streaming.api.environment.StreamExecutionEnvironment
import scala.jdk.CollectionConverters.*

/** the snippet in docs/modules/okay-scalus-flink.md, VERBATIM, run on a
 * MiniCluster against the recorded session under the relay name the
 * snippet uses for a live one */
class TestDocExamplesCardanoFlink extends munit.FunSuite:

  test("docs: a bounded Flink job over the chain") {
    Relays.register("doc-relay", () => Recorded.Replay())
    val relay = "registered:doc-relay"      // in production: "backbone.cardano.iog.io:3001"
    val start = s"${Recorded.intersect.slot}:${Recorded.intersect.hash}:${Recorded.intersect.blockNo}"
    // ---- snippet: flink
    val env = StreamExecutionEnvironment.getExecutionEnvironment
    val source = CardanoFlinkSource(CardanoConfig(
      relay = relay, network = "preprod", table = "transactions",
      confirmations = 0, start = start, maxBlocks = Some(5)))   // no maxBlocks: follow forever
    val txs = env.fromSource(source, WatermarkStrategy.noWatermarks(), "cardano")
      .setParallelism(1)                                           // the chain is one sequence
      .executeAndCollect().asScala.toList
    // ---- snippet ends
    assertEquals(txs.size, 4)
  }
