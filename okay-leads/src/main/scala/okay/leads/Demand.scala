package okay.leads

import okay.{Aggregator, Sketch, sliding}
import okay.given
import okay.leads.Lead.{Category, Outcome, Urgency}
import java.time.LocalDate

/**
 * WHAT THE LEDGER SAYS — the numbers you take to somebody who might
 * pay for them (specs/leads.md).
 *
 * Every figure here is one `Aggregator`, which means two things that
 * matter more than the arithmetic. It is ONE PASS: the whole report is
 * `zip`ped together and the ledger is read once, so a report costs
 * what reading the file costs whether it answers three questions or
 * thirty. And it is the SAME VALUE on a cluster: when the file becomes
 * a folder and one JVM is not enough, `Bulk` runs these unchanged
 * (specs/bulk.md) — nothing here needs rewriting for that day, and
 * nothing here should be written differently because of it either.
 *
 * The quantiles are a t-digest rather than a sorted list because a
 * budget distribution is what a provider argues about ("who are these
 * people at 1500?") and holding every value to answer that is the one
 * thing a ledger must not start doing.
 */
object Demand:

  /** the last word on a lead: a ledger is append-only, so a later row
   * for the same (session, at) is a correction of the earlier one */
  def latest(leads: Iterable[Lead]): Vector[Lead] =
    leads.groupBy(l => (l.session, l.at)).values.map(_.last).toVector.sortBy(_.at)

  // ------------------------------------------------------- the counts
  val total: Aggregator[Lead, Long, Long] = Aggregator.count[Lead]

  /** how many people, not how many requests — the number a buyer means
   * when they ask how big this is */
  val people: Aggregator[Lead, Set[String], Long] = Aggregator.distinct[String].contramap[Lead](_.session)

  val byCategory = Aggregator.groupBy((l: Lead) => l.category)(Aggregator.count[Lead])
  val byCity = Aggregator.groupBy((l: Lead) => l.city.getOrElse("—"))(Aggregator.count[Lead])
  val byUrgency = Aggregator.groupBy((l: Lead) => l.urgency)(Aggregator.count[Lead])
  val byOutcome = Aggregator.groupBy((l: Lead) => l.outcome)(Aggregator.count[Lead])

  /** the sellable shape: what is wanted, where — the pair a provider
   * buys, and the pair a provider refuses when the city is not theirs */
  val byCategoryCity = Aggregator.groupBy((l: Lead) => (l.category, l.city.getOrElse("—")))(Aggregator.count[Lead])

  /** budgets, where a budget was written down. The count travels with
   * the digest because "median 2800" over four people is not a median,
   * and the reader has to be able to see that. */
  val budgets = Aggregator.groupBy((l: Lead) => l.category)(
    Aggregator.count[Lead].zip(Sketch.tDigest().contramap[Lead](_.budget.fold(0.0)(_.value.toDouble))))

  /** what a lead is worth telling a provider about: it has a contact,
   * so it may be passed on at all, and it is not finished */
  def deliverable(leads: Iterable[Lead]): Vector[Lead] =
    latest(leads).filter(l => l.deliverable && (l.outcome == Outcome.Open || l.outcome == Outcome.Matched))

  /** the whole report in ONE pass over the ledger */
  val everything = total.zip(people).zip(byCategory).zip(byCity).zip(byUrgency).zip(byOutcome).zip(byCategoryCity)

  final case class Report(total: Long, people: Long,
                          byCategory: Map[Category, Long], byCity: Map[String, Long],
                          byUrgency: Map[Urgency, Long], byOutcome: Map[Outcome, Long],
                          byCategoryCity: Map[(Category, String), Long],
                          daily: Vector[(LocalDate, Long)], deliverable: Long):

    /** how many turned into something a provider could be sold */
    def conversion: Double =
      val won = byOutcome.getOrElse(Outcome.Matched, 0L) + byOutcome.getOrElse(Outcome.Contacted, 0L)
      if total == 0 then 0.0 else won * 100.0 / total

    /**
     * Demand over the last `days` days, rolling — the group's window
     * (specs/aggregators.md): the newcomer added, what aged out
     * subtracted, so a chart of a year costs a year, not a year times
     * the window.
     */
    def rolling(days: Int): Vector[(LocalDate, Double)] =
      if daily.isEmpty then Vector.empty
      else
        val from = daily.head._1
        val span = (daily.last._1.toEpochDay - from.toEpochDay).toInt + 1
        val perDay = daily.toMap
        val series = LazyList.tabulate(span)(i => perDay.getOrElse(from.plusDays(i), 0L).toDouble)
        sliding(series)(days).drop(days - 1).toVector.zipWithIndex
          .map((v, i) => (from.plusDays(i + days - 1), v))

    /** the pairs nobody answered — the list you actually walk into an
     * agency with, largest first */
    def unmet(top: Int = 10): Vector[((Category, String), Long)] =
      byCategoryCity.toVector.sortBy(-_._2).take(top)

    /** for the eye, and for an email */
    def show: String =
      val lines = Vector.newBuilder[String]
      lines += f"$total%,d requests from $people%,d people; $deliverable%,d with consent to be contacted"
      lines += f"conversion to matched/contacted: $conversion%.1f%%"
      lines += "by category: " + byCategory.toVector.sortBy(-_._2).map((c, n) => s"$c $n").mkString(", ")
      lines += "by urgency:  " + byUrgency.toVector.sortBy(-_._2).map((u, n) => s"$u $n").mkString(", ")
      lines += "top pairs (what, where):"
      unmet().foreach { case ((c, city), n) => lines += f"  $c%-8s $city%-12s $n%5d" }
      lines.result().mkString("\n")

  /** the report, in one pass over whatever the ledger handed back */
  def report(leads: Iterable[Lead]): Report =
    val rows = latest(leads)
    val ((((((t, p), cat), city), urg), out), pair) = everything.run(rows)
    val daily = rows.groupBy(_.day).view.mapValues(_.size.toLong).toVector.sortBy(_._1.toEpochDay)
    Report(t, p, cat, city, urg, out, pair, daily, deliverable(rows).size.toLong)

  /** the median budget per category, and how many said one — read off
   * the same digests the one-pass aggregate built */
  def medianBudgets(leads: Iterable[Lead]): Map[Category, (Long, Double)] =
    budgets.run(latest(leads).filter(_.budget.isDefined))
      .view.mapValues((n, d) => (n, d.quantile(0.5))).toMap
