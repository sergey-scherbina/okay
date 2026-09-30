package okay.cluster

import okay.*
import okay.given
import okay.Tables.read
import okay.Chunks.elements
import okay.Row.plus
import okay.Streamed.joinWithin
import okay.Tables.collect

/** docs/modules/okay-cluster.md's `FlowBulk` example, line for line
 * (TestDocSnippets pins each line of the page to a line here) */
class TestDocExamplesFlowBulk extends munit.FunSuite {

  private val files = Map(
    "big.csv" -> "k,b\n1,x\n2,y\n1,z\n",
    "small.csv" -> "k,s\n1,one\n2,two\n")
  private val lines: String => Iterator[String] = p => files(p).linesIterator
  private val sizes: String => Option[Long] = p => files.get(p).map(_.length.toLong)

  test("okay-cluster: a Tables program on the engine answers what it answers in one JVM") {
    val sameProgram =
      val small = read("small.csv").select(r => r("k") -> r("s"))
      val big = read("big.csv").select(r => r("k") -> r("b"))
      big.join(small).select { case (k, (b, s)) => (k, b, s) }.collect.map(_.elements.toVector.sorted)
    val B = FlowBulk(4, lines, sizes)                                    // four partitions, files read by name
    val rows = Tables.run(B):
      val small = read("small.csv").select(r => r("k") -> r("s"))
      val big = read("big.csv").select(r => r("k") -> r("b"))
      big.join(small).select { case (k, (b, s)) => (k, b, s) }.collect.map(_.elements.toVector.sorted)
    assertEquals(rows, Tables.run(Bulk.local(lines, sizes))(sameProgram))    // the agreement law
    assertEquals(rows, Vector(("1", "x", "one"), ("1", "z", "one"), ("2", "y", "two")))
  }

  test("okay-cluster: a windowed join by key, as a Tables + Streamed program on the engine") {
    val clicks = Vector(("u1", (0L, "home")), ("u2", (3L, "cart")), ("u1", (30L, "pay")))
    val buys = Vector(("u1", (8L, 9.99)), ("u2", (50L, 4.50)))
    val paid = Tables.of(clicks).plus[Streamed].joinWithin(Tables.of(buys).plus[Streamed], 10L, 0L)(_._1, _._1)
      .collect.map(_.elements.toVector)
    assertEquals(FlowBulk(4).run(paid), Vector(("u1", ((0L, "home"), (8L, 9.99)))))
  }
}
