package okay.codec

import okay.{Chunks, Spill}
import okay.Chunks.elements
import okay.codec.RunCodecs.given

/** `RunCodecs.fromSchema`: a case class sorted through spilled runs */
class TestRunCodecs extends munit.FunSuite {
  final case class Row(id: Long, name: String, tags: List[String], score: Option[Double])
  given Schema[Row] = Schema.derived

  test("a Schema type sorts through spilled runs, every field back as it went") {
    val rnd = scala.util.Random(5)
    val xs = Vector.tabulate(300)(i => Row(rnd.nextLong(), s"n$i", List.fill(i % 3)("t"), Option.when(i % 2 == 0)(i * 0.5)))
    given spill: Spill.Memory = Spill.memory
    val got = Chunks.sortBy(Chunks.fromIterator(xs.iterator), 17)(_.id).elements.toVector
    assertEquals(got, xs.sortBy(_.id))
    assertEquals(spill.opened, 18)
    assertEquals(spill.live, 0)
  }
}
