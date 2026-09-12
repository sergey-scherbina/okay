package okay.demo

import okay.{!, Async}
import okay.given
import okay.blob.Fs
import java.nio.file.Files

/**
 * The one-binary story, run end to end: record, report, page,
 * backup, restore, report again — one process, one test.
 */
class TestLedger extends munit.FunSuite {
  import Ledger.*

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  /** three days of a small shop: three items, deterministic amounts */
  val day0 = 1_700_000_000_000L / Day * Day
  val items = Vector("coffee", "bread", "cheese")
  def sales(n: Int): Vector[Sale] =
    (0 until n).toVector.map { i =>
      val h = (i * 2654435761L) & 0xffffffffL
      Sale(day0 + (i.toLong * 3 * Day / n) + (h % 1000), items((h >>> 8).toInt % items.length), 100 + (h >>> 16) % 2000)
    }

  test("record, report, page: the engine's daily totals are the spreadsheet's") {
    val root = Files.createTempDirectory("okay-ledger")
    val ledger = Ledger(root, segmentBytes = 4096)
    val ss = sales(600)
    ss.foreach(s => ledger.record(s): Unit)
    assertEquals(ledger.recorded, 600L)

    val got = run(ledger.report())
    assertEquals(got.value, byHand(ss))
    assertEquals(got.dropped, 0L, "nothing is late in a log written in order")
    assertEquals(got.value.lines.map(_.day).distinct.length, 3, "three days")
    assertEquals(got.value.lines.length, 9, "three items on each of three days")

    val page = lines(got.value)
    println(("\n  the page, as the terminal host draws it:\n" +: page.map("    " + _)).mkString("\n"))
    // the page shows every line's total, in currency
    for l <- got.value.lines do
      val shown = f"${l.cents / 100}%d.${l.cents % 100}%02d"
      assert(page.exists(_.contains(shown)), s"the page does not show $shown for ${l.item}")
    ledger.close()
  }

  test("backup leaves the directory; restore comes back certified; the report over the copy is the books up to the last roll") {
    val root = Files.createTempDirectory("okay-ledger")
    val ledger = Ledger(root, segmentBytes = 4096)
    val ss = sales(600)
    ss.foreach(s => ledger.record(s): Unit)
    val whole = run(ledger.report()).value

    // "somewhere that is not this machine": a Blob — a directory
    // here, the same trait S3 implements
    val blob = Fs(Files.createTempDirectory("okay-ledger-offsite"))
    val copied = run(ledger.backup(blob))
    assert(copied.nonEmpty, "nothing rolled — the fixture's segments are too large")
    assertEquals(run(ledger.backup(blob)), Vector.empty, "a second backup copies nothing")

    val fresh = Files.createTempDirectory("okay-ledger-restored")
    val (placed, verdict, restored) = run(Restored(blob, fresh, 4096))
    assertEquals(placed.length, copied.length)
    assert(verdict.restorable, verdict.problems.mkString("; "))

    // the copy holds the books up to the last roll — not the active
    // segment; that bound is `segmentBytes`, and it is stated, not hidden
    val back = restored.recorded
    assert(back > 0L && back < 600L, s"expected a prefix of the books, got $back of 600")
    val over = run(restored.report()).value
    assertEquals(over, byHand(ss.take(back.toInt)))
    assertNotEquals(over, whole, "the active segment came back — Backup.copy's contract changed")
    // and the report over the copy is a PREFIX of the whole: no
    // day's total is larger in the copy than in the books
    for l <- over.lines do
      val w = whole.lines.find(x => x.day == l.day && x.item == l.item).getOrElse(fail(s"$l is not in the books"))
      assert(l.cents <= w.cents)
    ledger.close(); restored.close()
  }
}
