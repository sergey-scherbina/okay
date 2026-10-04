package okay2.persist

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/** the battery over the FileStore ARBITER (okay-persist's
 * TestElectionFile) — the dev deployment's control log: one process, one
 * disk, total order for free */
class TestElectionFile extends ElectionSuite {
  private var dirs = List.empty[Path]
  def mkControl(): Topic = {
    val d = Files.createTempDirectory("okay2-election")
    dirs ::= d
    FileStore.open(d).topic("__control")
  }
  override def afterAll(): Unit = {
    def wipe(p: Path): Unit = {
      if (Files.isDirectory(p)) {
        val l = Files.list(p)
        try l.iterator.asScala.foreach(wipe) finally l.close()
      }
      val _ = Files.deleteIfExists(p)
    }
    dirs.foreach(wipe)
  }
}
