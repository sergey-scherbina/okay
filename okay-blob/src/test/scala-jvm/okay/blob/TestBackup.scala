package okay.blob

import okay.{!, Async}
import okay.given
import okay.persist.{Ack, Doctor, FileStore, Policy}
import java.nio.file.{Files, Path}

/**
 * Backup is boring, proven end to end: closed segments copy
 * incrementally to a Blob, restore is placing files back, recovery
 * (the ordinary startup path) reads them, and the Doctor certifies
 * the copy BEFORE the incident.
 */
class TestBackup extends munit.FunSuite {

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  def store(): (Path, okay.persist.Topic) =
    val root = Files.createTempDirectory("okay-backup-src")
    (root, FileStore.open(root).topic("events", 1, Policy(segmentBytes = 256)))

  def fill(t: okay.persist.Topic, n: Int, from: Int = 0): Unit =
    for i <- from until (from + n) do
      val _ = t.append(0, s"k$i".getBytes, s"value-$i-padded-for-realistic-frames".getBytes, Ack.Durable)

  test("closed segments copy; a second run copies nothing; a roll adds one") {
    val (root, t) = store()
    fill(t, 40)
    val blob = Fs(Files.createTempDirectory("okay-backup-dst"))
    val first = run(Backup.copy(root, blob))
    assert(first.nonEmpty, "nothing was closed?")
    assertEquals(run(Backup.copy(root, blob)), Vector.empty)   // incremental: boring
    fill(t, 40, from = 40)                                     // roll more segments
    val second = run(Backup.copy(root, blob))
    assert(second.nonEmpty)
    assert(first.toSet.intersect(second.toSet).isEmpty, "a closed segment copied twice")
  }

  test("restore places files back; recovery reads them; the doctor certifies the copy") {
    val (root, t) = store()
    fill(t, 40)
    val blob = Fs(Files.createTempDirectory("okay-backup-dst2"))
    val copied = run(Backup.copy(root, blob))
    assert(copied.nonEmpty)

    val fresh = Files.createTempDirectory("okay-backup-restore")
    val placed = run(Backup.restore(blob, fresh))
    assertEquals(placed.sorted, copied.map(_.stripPrefix("persist/")).sorted)

    // the doctor certifies the restored copy offline
    val verdict = Doctor.scan(fresh)
    assert(verdict.restorable, verdict.problems.toString)

    // and recovery — the ordinary startup path — serves the records
    val t2 = FileStore.open(fresh).topic("events", 1, Policy(segmentBytes = 256))
    t2.read(0, t2.begin(0), 1000) match
      case okay.persist.Topic.Read.Records(rs) =>
        assert(rs.nonEmpty)
        assert(rs.forall(r => String(r.value, "UTF-8").startsWith("value-")))
      case other => fail(other.toString)
  }
}
