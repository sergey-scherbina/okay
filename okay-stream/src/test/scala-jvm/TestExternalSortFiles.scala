package okay


import java.nio.file.Files
import scala.jdk.CollectionConverters.*
import Chunks.elements

/** `SpillFiles`: runs on disk, deleted once the output is read to its end */
class TestExternalSortFiles extends munit.FunSuite {
  test("200 000 rows in runs of 10 000 on disk sort as in memory, and leave no file behind") {
    val dir = Files.createTempDirectory("extsort")
    given Spill = SpillFiles(dir.toFile)
    val rnd = scala.util.Random(3)
    val xs = Vector.fill(200000)((rnd.nextLong(), rnd.alphanumeric.take(4).mkString))
    val it = Chunks.sortBy(Chunks.fromIterator(xs.iterator), 10000)(_._1).elements
    assert(it.hasNext)
    assertEquals(Files.list(dir).iterator().asScala.size, 20, "one file per run while the merge reads")
    assertEquals(it.toVector, xs.sortBy(_._1))
    assertEquals(Files.list(dir).iterator().asScala.size, 0, "the runs outlived the output")
  }
}
