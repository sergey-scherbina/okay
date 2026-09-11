package okay.chat.leads

import okay.{Aggregator, Bulk, Csv}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.jdk.CollectionConverters.*

/**
 * THE LEDGER: one CSV file, appended to, never rewritten
 * (specs/leads.md).
 *
 * A database would be the reflex and it would be wrong here. What this
 * has to do is survive a restart, be readable by the person who will
 * sell from it (a spreadsheet opens it), and be aggregable by the
 * algebra this repository already has (`Bulk.csv` reads it, on one JVM
 * or on a cluster, unchanged). A file does all three and costs nothing
 * to run, which is the whole point at the stage where the hosting bill
 * is the thing being covered.
 *
 * Append-only is not tidiness either: a lead's OUTCOME changes later
 * (open → matched → contacted), and a ledger that updated rows in
 * place would lose the history that says how long a match took. The
 * last row per (session, at) wins; `Demand` folds them that way.
 */
final class Ledger(val path: Path):

  /** the header is written once, when the file is made */
  private def ensure(): Unit =
    if !Files.exists(path) then
      Option(path.getParent).foreach(d => { val _ = Files.createDirectories(d) })
      val _ = Files.writeString(path, Lead.header + "\n", UTF_8)

  /** one row, flushed: a lead that is still in a buffer when the
   * process dies is a lead that never happened */
  def append(l: Lead): Unit =
    synchronized:
      ensure()
      val _ = Files.writeString(path, Csv.line(l.row) + "\n", UTF_8,
        StandardOpenOption.CREATE, StandardOpenOption.APPEND)

  def appendAll(ls: IterableOnce[Lead]): Unit = ls.iterator.foreach(append)

  /** every row that reads back, and every row that does not, said so
   * rather than dropped — a ledger that silently loses rows is worse
   * than one that is short */
  def readEither(): (Vector[Lead], Vector[String]) =
    if !Files.exists(path) then (Vector.empty, Vector.empty)
    else
      val src = Files.lines(path, UTF_8)
      try
        val rows = Csv.rows(src.iterator().asScala)
        rows.foldLeft((Vector.empty[Lead], Vector.empty[String])):
          case ((ok, bad), r) => Lead.fromRow(r) match
            case Right(l) => (ok :+ l, bad)
            case Left(why) => (ok, bad :+ why)
      finally src.close()

  def read(): Vector[Lead] = readEither()._1

  /** the ledger through the `Bulk` seam: the same aggregation runs
   * here on one JVM and on a cluster when the file becomes a folder */
  def rows[D[_]](using B: Bulk[D]): D[Csv.Row] = B.csv(path.toString)

  /** how many rows, without reading them into memory */
  def size[D[_]](using B: Bulk[D]): Long = B.aggregate(rows[D])(Aggregator.count[Csv.Row])

object Ledger:
  def at(path: String): Ledger = new Ledger(Path.of(path))
