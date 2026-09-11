package okay.leads

import okay.intent.Amount
import java.time.{Instant, LocalDate, ZoneOffset}

/**
 * WHAT THE CHAT LEARNED FROM ONE REQUEST, and nothing else
 * (specs/leads.md).
 *
 * The reason this type exists before any revenue does: a lead you
 * cannot count is a lead you cannot sell, and a conversation you did
 * not reduce to fields is not evidence of anything. Everything a
 * provider, an agency or a landlord would pay for is here —
 * what, where, how much, how soon, what came of it — and everything
 * that would make it illegal to pass on is deliberately NOT here.
 *
 * WHAT IS NOT STORED, and why that is a feature rather than a
 * limitation: the message. A ledger of what people typed is a pile of
 * personal data under the GDPR the moment somebody writes their phone
 * number into a chat, and it is worth nothing extra — the fields below
 * are what a buyer actually reads. `session` is a SALTED HASH of the
 * conversation id, so the same person's two requests can be told apart
 * from two people's, and nobody downstream can walk it back to a
 * person. `contact` exists only when a person asked to be contacted;
 * without it a lead is a statistic, which is the only thing this
 * ledger is by default.
 *
 * Every unknown stays unknown. A category is assigned when a cue
 * fires, a city when a city is named, a budget when an amount is
 * written down — never guessed, because a guessed field is a number
 * somebody will later quote back at you.
 */
final case class Lead(at: Instant,
                      session: String,
                      category: Lead.Category,
                      city: Option[String],
                      budget: Option[Amount],
                      urgency: Lead.Urgency,
                      outcome: Lead.Outcome,
                      /** only with an explicit ask to be contacted */
                      contact: Option[String] = None):

  def day: LocalDate = at.atZone(ZoneOffset.UTC).toLocalDate

  /** may this row be handed to a provider at all */
  def deliverable: Boolean = contact.isDefined

  /** the CSV row — a spreadsheet is what a buyer opens, and
   * `Bulk.csv` is what reads it back */
  def row: Vector[String] = Vector(
    at.toString, session, category.toString, city.getOrElse(""),
    budget.fold("")(_.value.bigDecimal.stripTrailingZeros.toPlainString),
    budget.fold("")(_.currency), urgency.toString, outcome.toString,
    contact.getOrElse(""))

object Lead:
  /** what a person is after. Five, because five is what the demand in
   * a city actually splits into and a sixth would be a guess. */
  enum Category:
    case Job, Housing, Service, Goods, Leisure, Unknown

  /** how soon — the field that decides what a lead is worth, and the
   * one a provider asks about first */
  enum Urgency:
    case Now, Week, Month, Later, Unknown

  /** what came of it. `Open` is the honest default: most requests are
   * still requests when the turn ends. */
  enum Outcome:
    case Open, Answered, Matched, Contacted, Dropped

  val columns: Vector[String] =
    Vector("at", "session", "category", "city", "budget", "currency", "urgency", "outcome", "contact")

  def header: String = columns.mkString(",")

  /** the session id as it may be stored: salted, hashed, truncated —
   * enough to tell two conversations apart, not enough to identify
   * one. The salt is the deployment's, and rotating it forgets
   * everybody, which is the point. */
  def pseudonym(sessionId: String, salt: String): String =
    val md = java.security.MessageDigest.getInstance("SHA-256")
    md.update(salt.getBytes("UTF-8"))
    md.update(sessionId.getBytes("UTF-8"))
    md.digest().take(8).map(b => f"${b & 0xff}%02x").mkString

  private def opt(s: String): Option[String] = Option(s).map(_.trim).filter(_.nonEmpty)

  /** back from a CSV row, total: a row this cannot read is a row this
   * says it cannot read, rather than a Lead with invented fields */
  def fromRow(r: Map[String, String]): Either[String, Lead] =
    def field(n: String) = r.getOrElse(n, "")
    for
      at <- opt(field("at")).toRight("no `at`").flatMap(s =>
        scala.util.Try(Instant.parse(s)).toEither.left.map(_ => s"not an instant: $s"))
      cat <- Category.values.find(_.toString == field("category")).toRight(s"unknown category: ${field("category")}")
      urg <- Urgency.values.find(_.toString == field("urgency")).toRight(s"unknown urgency: ${field("urgency")}")
      out <- Outcome.values.find(_.toString == field("outcome")).toRight(s"unknown outcome: ${field("outcome")}")
    yield Lead(at, field("session"), cat, opt(field("city")),
      opt(field("budget")).flatMap(v => scala.util.Try(BigDecimal(v)).toOption)
        .map(v => Amount(v, opt(field("currency")).getOrElse("PLN"))),
      urg, out, opt(field("contact")))
