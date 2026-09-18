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

  /**
   * THE DEFECT (backup-active-segment): a backup that copies only
   * CLOSED segments is bounded by `segmentBytes` of unsaved books — a
   * shop that appends and then backs up gets everything except what it
   * just wrote, which is the part it would miss most.
   */
  test("a backup holds what was written up to NOW, not up to the last roll") {
    val (root, t) = store()
    fill(t, 3)                                    // too few to roll: all of it is active
    val blob = Fs(Files.createTempDirectory("okay-backup-now"))
    val copied = run(Backup.copy(root, blob))
    assert(copied.nonEmpty, "the active segment was left home, so the backup is empty")

    val fresh = Files.createTempDirectory("okay-backup-now-restore")
    run(Backup.restore(blob, fresh)): Unit
    // recovery reads a torn tail on the last segment as the ordinary
    // crash artifact, which is exactly what a copy of a live file is
    assert(Doctor.scan(fresh).restorable, Doctor.scan(fresh).problems.toString)
    val t2 = FileStore.open(fresh).topic("events", 1, Policy(segmentBytes = 256))
    t2.read(0, t2.begin(0), 1000) match
      case okay.persist.Topic.Read.Records(rs) =>
        assertEquals(rs.length, 3, "the records written since the last roll are missing")
      case other => fail(other.toString)
  }

  /**
   * WHAT INCREMENTAL MEANS once the active segment travels too
   * (backup-active-segment): a CLOSED segment is copied once and never
   * again — they never change — while the ACTIVE one is copied
   * whenever it has grown, until it rolls and joins the closed ones
   * for good. A run that changed nothing still answers nothing.
   */
  test("closed segments copy once; the active one until it rolls; an idle run copies nothing") {
    val (root, t) = store()
    fill(t, 40)
    val blob = Fs(Files.createTempDirectory("okay-backup-dst"))
    val first = run(Backup.copy(root, blob))
    assert(first.nonEmpty, "nothing was copied?")
    assertEquals(run(Backup.copy(root, blob)), Vector.empty)   // idle: boring
    fill(t, 40, from = 40)                                     // roll more segments
    val second = run(Backup.copy(root, blob))
    assert(second.nonEmpty)
    // the only key in both runs is the segment that was ACTIVE in the
    // first and had grown or closed by the second
    val twice = first.toSet.intersect(second.toSet)
    assert(twice.size <= 1, s"more than the active segment was copied twice: $twice")
    // and with `active = false` the strict old property holds
    val strict = Fs(Files.createTempDirectory("okay-backup-strict"))
    val a = run(Backup.copy(root, strict, active = false))
    fill(t, 40, from = 80)
    val b = run(Backup.copy(root, strict, active = false))
    assert(a.toSet.intersect(b.toSet).isEmpty, "a closed segment copied twice")
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
