package okay.chat.leads

import okay.{Aggregator, Bulk, Csv}
import okay.chat.leads.Lead.Outcome
import java.time.Instant

/**
 * The two lines a chat needs, and the report a person reads
 * (specs/leads.md).
 *
 * `watch` is deliberately the whole integration: a chat route that
 * already has the turn's text calls it and carries on. It answers the
 * `Lead` it wrote so that the caller can hand it back later —
 * `outcome` — when the turn ended in something; a caller that never
 * does still has a ledger of what was asked, which is the number that
 * matters first.
 */
object Leads:

  /** record one turn. The salt is the deployment's (see
   * `Lead.pseudonym`); the message is READ and not kept. */
  def watch(ledger: Ledger, salt: String)(sessionId: String, message: String,
                                          at: Instant = Instant.now()): Lead =
    val l = Capture.lead(message, sessionId, salt, at)
    ledger.append(l)
    l

  /** the same lead, later, with what came of it — appended, because
   * the ledger keeps history and `Demand.latest` reads the last word */
  def outcome(ledger: Ledger, l: Lead, o: Outcome, contact: Option[String] = None): Lead =
    val done = l.copy(outcome = o, contact = contact.orElse(l.contact))
    ledger.append(done)
    done

  /**
   * The counts straight off the CSV, through the `Bulk` seam: the same
   * value runs on one JVM today and on a cluster the day the file
   * becomes a folder (specs/bulk.md). It reads the rows rather than
   * `Lead`s because a count of what was asked for, where, needs three
   * columns and not a parsed record.
   */
  def demandByCategoryCity[D[_]](rows: D[Csv.Row])(using B: Bulk[D]): Map[(String, String), Long] =
    B.aggregate(rows)(Aggregator.groupBy((r: Csv.Row) =>
      (r.getOrElse("category", "Unknown"), r.get("city").filter(_.nonEmpty).getOrElse("—")))(
      Aggregator.count[Csv.Row]))

/**
 * The report, as a command: `sbt "okayChat/runMain okay.chat.leads.Report leads.csv"`.
 *
 * It exists because the first customer conversation is not a dashboard
 * — it is a person reading five lines out loud to somebody who might
 * pay for them.
 */
object Report:
  def main(args: Array[String]): Unit =
    val path = args.headOption.getOrElse("leads.csv")
    val (leads, bad) = Ledger.at(path).readEither()
    if leads.isEmpty then println(s"$path: nothing recorded yet")
    else
      val r = Demand.report(leads)
      println(r.show)
      val medians = Demand.medianBudgets(leads)
      if medians.nonEmpty then
        println("median budget where one was written down:")
        medians.toVector.sortBy(_._1.toString).foreach { case (c, (n, m)) =>
          println(f"  $c%-8s ${m}%8.0f  (of $n%d)") }
      val rolling = r.rolling(7)
      if rolling.nonEmpty then
        println(f"7-day rolling demand: ${rolling.last._2}%.0f requests to ${rolling.last._1}")
    if bad.nonEmpty then println(s"${bad.size} unreadable rows: ${bad.take(3).mkString("; ")}")
