package okay

import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/** The local `Bulk` and the CSV it reads (specs/bulk.md). */
class TestBulk extends munit.FunSuite {

  test("Csv.fields: quotes, doubled quotes, commas inside quotes") {
    assertEquals(Csv.fields("a,\"b,c\",\"say \"\"hi\"\"\",,d"), Vector("a", "b,c", "say \"hi\"", "", "d"))
    assertEquals(Csv.fields(""), Vector(""))
  }

  test("Csv.rows: the header names the columns and a BOM is stripped") {
    val rows = Csv.rows(Iterator("﻿id,name", "1,\"Krasińskiego\"", "2,KŁOKOCZYCE")).toList
    assertEquals(rows, List(Map("id" -> "1", "name" -> "Krasińskiego"), Map("id" -> "2", "name" -> "KŁOKOCZYCE")))
  }

  test("a local source is replayable: a file is re-read on every run") {
    val f = Files.createTempFile("bulk", ".csv")
    Files.writeString(f, "k,v\n1,10\n2,20\n1,5\n")
    var reads = 0
    val B = Bulk.local(p => { reads += 1; Files.lines(java.nio.file.Path.of(p)).iterator().asScala })
    // B.map, not d.map: on a CONCRETE Chunks the collection view loses to
    // the program's own monadic map — the view is for code generic in D
    val d = B.map(B.csv(f.toString))(r => r("k").toInt -> r("v").toLong)
    assertEquals(B.aggregate(d)(Aggregator.count[(Int, Long)]), 3L)
    assertEquals(B.aggregate(d)(Aggregator.sum[Long].contramap[(Int, Long)](_._2)), 35L)
    assertEquals(reads, 2)
    // and cached, it is read once more and then held
    val c = B.cache(d)
    assertEquals(B.aggregate(c)(Aggregator.count[(Int, Long)]), 3L)
    assertEquals(B.aggregate(c)(Aggregator.count[(Int, Long)]), 3L)
    assertEquals(reads, 3)
    Files.delete(f)
  }

  test("join is the equi-join, right side hashed, left side streamed") {
    val B = localBulk
    val l = B.of(List(1 -> "a", 2 -> "b", 1 -> "c", 3 -> "d"))
    val r = B.of(List(1 -> 10, 1 -> 11, 2 -> 20))
    val all = Aggregator[(Int, (String, Int)), Vector[(Int, (String, Int))], Vector[(Int, (String, Int))]](Vector.empty)(_ :+ _)(_ ++ _)(identity)
    val joined = B.aggregate(B.join(l, r))(all)
    assertEquals(joined.sorted, Vector(1 -> ("a", 10), 1 -> ("a", 11), 1 -> ("c", 10), 1 -> ("c", 11), 2 -> ("b", 20)))
  }
}
