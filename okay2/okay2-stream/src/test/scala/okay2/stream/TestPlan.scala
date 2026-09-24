package okay2.stream

import java.nio.file.Files
import okay2._
import okay2.stream.Chunks.ChunksOps
import okay2.stream.Tables.{Plan, read}
import TablesFixtures._

/** The plan rewrites — what they change, what they leave alone. The
 * Scala 3 core's okay-stream TestPlan */
class TestPlan extends munit.FunSuite {
  import Plan._

  val sized: String => Option[Long] = { case "big.csv" => Some(1000L); case "small.csv" => Some(10L); case _ => None }

  test("a projection meets its read, and the platform prunes at the parser") {
    val p = Columns(Columns(Read("big.csv", None), Set("a", "b", "c")), Set("b", "c", "d"))
    assertEquals(optimize(p, sized), Read("big.csv", Some(Set("b", "c"))): Plan[Csv.Row])
  }

  test("a projection above a function stays where it is: the function is opaque") {
    val p = Columns(Select(Read("big.csv", None), (r: Csv.Row) => r), Set("a"))
    assertEquals(optimize(p, sized), p: Plan[Csv.Row])
  }

  test("the small side of a join goes to the right, and the answer is turned back") {
    val small = Select(Read("small.csv", None), (r: Csv.Row) => r("k") -> r("s"))
    val big = Select(Read("big.csv", None), (r: Csv.Row) => r("k") -> r("b"))
    // by type pattern: scalac 2 will not instantiate `Join`'s
    // constructor pattern under the `Select`'s wildcard input
    optimize(Join(small, big), sized) match {
      case s: Select[_, _] => s.p match {
        case j: Join[_, _, _] => assert(j.l == big && j.r == small, s"sides: ${j.l}, ${j.r}")
        case other => fail(s"not turned: ${show(other)}")
      }
      case other => fail(s"not turned: ${show(other)}")
    }
    assertEquals(optimize(Join(big, small), sized), Join(big, small): Plan[(String, (String, String))])
    val held = Held[(String, String)](3)
    assertEquals(optimize(Join(small, held), sized), Join(small, held): Plan[(String, (String, String))])
  }

  test("the turned join answers exactly what the written one does, and the log shows the big side first") {
    val f = Files.createTempFile("big", ".csv"); Files.writeString(f, "k,b\n1,x\n2,y\n1,z\n" * 200)
    val g = Files.createTempFile("small", ".csv"); Files.writeString(g, "k,s\n1,one\n2,two\n")
    def prog(swap: Boolean): Vector[(String, String, String)] ! Tables = {
      val small = read(g.toString).select(r => r("k") -> r("s"))
      val big = read(f.toString).select(r => r("k") -> r("b"))
      (if (swap) small.join(big).select { case (k, (s, b)) => (k, b, s) } else big.join(small).select { case (k, (b, s)) => (k, b, s) })
        .collect.map(_.elements.toVector.sorted)
    }
    val plans = scala.collection.mutable.ListBuffer.empty[String]
    val viaSwap = State.run(Tables.Heap.empty[Chunks])(Tables.via[Vector[(String, String, String)], Chunks, Pure](localBulk, p => { plans += Plan.show(p); () })(prog(true)))._2
    assertEquals(viaSwap, Tables.run(localBulk)(prog(false)))
    val lines = plans.head.linesIterator.toSeq
    assert(lines.indexWhere(_.contains("Read(big")) < lines.indexWhere(_.contains("Read(small")), plans.head)
    Files.delete(f); Files.delete(g)
  }

  test("columns prune what a local read builds") {
    val f = Files.createTempFile("wide", ".csv"); Files.writeString(f, "a,b,c\n1,2,3\n4,5,6\n")
    val rows = Tables.run(localBulk)(read(f.toString).columns("a", "c").collect.map(_.elements.toVector))
    assertEquals(rows, Vector(Map("a" -> "1", "c" -> "3"), Map("a" -> "4", "c" -> "6")))
    Files.delete(f)
  }
}
