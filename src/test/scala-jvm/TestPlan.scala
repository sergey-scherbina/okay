package okay

import okay.Tables.{Plan, read, of}
import okay.Chunks.elements
import java.nio.file.Files
import scala.jdk.CollectionConverters.*
import okay.RowLift.plus

/** The plan rewrites (specs/bulk.md, bulk-plan): what they change, what they leave alone. */
class TestPlan extends munit.FunSuite {
  import Plan.*

  val sized: String => Option[Long] = { case "big.csv" => Some(1000L); case "small.csv" => Some(10L); case _ => None }

  test("a projection meets its read, and the platform prunes at the parser") {
    val p = Columns(Columns(Read("big.csv", None), Set("a", "b", "c")), Set("b", "c", "d"))
    assertEquals(optimize(p, sized), Read("big.csv", Some(Set("b", "c"))))
  }

  test("a projection above a function stays where it is: the function is opaque") {
    val p = Columns(Select(Read("big.csv", None), (r: Csv.Row) => r), Set("a"))
    assertEquals(optimize(p, sized), p)
  }

  test("the small side of a join goes to the right, and the answer is turned back") {
    val small = Select(Read("small.csv", None), (r: Csv.Row) => r("k") -> r("s"))
    val big = Select(Read("big.csv", None), (r: Csv.Row) => r("k") -> r("b"))
    optimize(Join(small, big), sized) match
      case Select(Join(l, r), _) => assert(l == big && r == small, s"sides: $l, $r")
      case other => fail(s"not turned: ${show(other)}")
    // and a join already the right way round is left alone
    assertEquals(optimize(Join(big, small), sized), Join(big, small))
    // unknown sizes: no guess
    assertEquals(optimize(Join(small, Held[(String, String)](3)), sized), Join(small, Held(3)))
  }

  test("the turned join answers exactly what the written one does") {
    val f = Files.createTempFile("big", ".csv"); Files.writeString(f, "k,b\n1,x\n2,y\n1,z\n" * 200)
    val g = Files.createTempFile("small", ".csv"); Files.writeString(g, "k,s\n1,one\n2,two\n")
    val B = Bulk.local(p => Files.lines(java.nio.file.Path.of(p)).iterator().asScala, p => Some(Files.size(java.nio.file.Path.of(p))))
    def prog(swap: Boolean) =
      val small = read(g.toString).select(r => r("k") -> r("s"))
      val big = read(f.toString).select(r => r("k") -> r("b"))
      (if swap then small.join(big).select { case (k, (s, b)) => (k, b, s) } else big.join(small).select { case (k, (b, s)) => (k, b, s) })
        .collect.map(_.elements.toVector.sorted)
    val plans = scala.collection.mutable.ListBuffer.empty[String]
    val viaSwap = State.run(Tables.Heap.empty[Chunks])(Tables.via(B, p => plans += Plan.show(p))(prog(true).plus[okay.Pure]))._2
    val direct = Tables.run(B)(prog(false))
    assertEquals(viaSwap, direct)
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
