package okay.persist

import java.nio.file.Files

/** the battery over the FileStore ARBITER — the dev deployment's
 * control log (specs/consensus.md): one process, one disk, total
 * order for free; failover availability traded, correctness not */
class TestElectionFile extends ElectionSuite:
  private var dirs = List.empty[java.nio.file.Path]
  def mkControl(): Topic =
    val d = Files.createTempDirectory("okay-election")
    dirs ::= d
    FileStore.open(d).topic("__control")
  override def afterAll(): Unit =
    def wipe(p: java.nio.file.Path): Unit =
      if Files.isDirectory(p) then
        val l = Files.list(p)
        try l.forEach(wipe) finally l.close()
      Files.deleteIfExists(p)
      ()
    dirs.foreach(wipe)
