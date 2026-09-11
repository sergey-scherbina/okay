package okay.chat.leads

import okay.intent.{Amount, Temporal}
import okay.chat.leads.Lead.{Category, Outcome, Urgency}
import java.time.{Instant, ZoneOffset}

/**
 * One message in, the fields a lead is made of out — with NO model
 * (specs/leads.md).
 *
 * This is the cheap tier on purpose. A classification that costs a
 * token is a classification you stop doing when the bill arrives, and
 * a ledger with holes in it proves nothing; cues and the parsers
 * `okay-intent` already ships cost nothing and run on every turn
 * forever. What they cannot read stays `Unknown`, and a later tier
 * (the model that is already answering the person) may fill it in by
 * calling `refine` — the ledger does not care which tier was right,
 * only that nothing was invented.
 *
 * The cues are three languages because the city this was written for
 * is: Polish, Russian, English. A cue is a word that means the person
 * wants something of that kind, not a word that merely occurs in such
 * requests — "mieszkanie" is a cue, "miasto" is not.
 */
object Capture:

  private def normal(s: String): String =
    s.toLowerCase.replace('ł', 'l').replace('ą', 'a').replace('ę', 'e')
      .replace('ó', 'o').replace('ś', 's').replace('ż', 'z').replace('ź', 'z')
      .replace('ć', 'c').replace('ń', 'n')

  /** the cue tables. Order matters only where a word belongs to two
   * kinds; where it does, the more specific table comes first. */
  private val cues: Vector[(Category, Vector[String])] = Vector(
    Category.Housing -> Vector("mieszkan", "pokoj", "wynaj", "kawaler", "stancj", "dom do",
      "квартир", "комнат", "снять жиль", "жиль", "аренд",
      "flat", "apartment", "room to rent", "rent a", "housing", "accommodation"),
    Category.Job -> Vector("prac", "zatrudni", "etat", "zlecenie", "rekrut", "cv",
      "работ", "ваканс", "подработ", "резюме", "трудоустр",
      "job", "vacancy", "hiring", "employment", "internship"),
    Category.Service -> Vector("hydraulik", "elektryk", "remont", "sprzatan", "naprawa", "przeprowadzk",
      "kurs", "korepetycj", "fryzjer", "uslug",
      "сантехник", "электрик", "ремонт", "уборк", "переезд", "репетитор", "мастер", "услуг",
      "plumber", "electrician", "repair", "cleaning", "moving", "tutor", "service"),
    Category.Goods -> Vector("kupi", "sprzedam", "rower", "meble", "lodowk", "telefon", "samochod",
      "куплю", "продам", "велосипед", "мебель", "холодильник", "телефон", "машин",
      "buy a", "sell my", "second hand", "for sale", "looking to buy"),
    Category.Leisure -> Vector("koncert", "bilet", "restauracj", "impreza", "wystaw", "silown", "basen",
      "концерт", "билет", "ресторан", "выставк", "спортзал", "бассейн", "куда сходить",
      "concert", "ticket", "restaurant", "gym", "what to do", "event"))

  /** cities this deployment serves. A city is only recognised when it
   * is named — the alternative, inferring it from an IP, is both worse
   * data and a thing to have to explain to a regulator. */
  val cities: Vector[(String, Vector[String])] = Vector(
    "Wrocław" -> Vector("wroclaw", "wroclawiu", "wroca", "вроцлав", "wro"),
    "Kraków" -> Vector("krakow", "krakowie", "краков"),
    "Warszawa" -> Vector("warszaw", "варшав", "warsaw"),
    "Poznań" -> Vector("poznan", "познан"),
    "Gdańsk" -> Vector("gdansk", "гданьск"))

  private val nowWords = Vector("pilne", "na juz", "dzis", "natychmiast",
    "срочно", "сегодня", "сейчас", "urgent", "today", "asap", "right now")
  private val weekWords = Vector("w tym tygodniu", "do piatku", "jutro",
    "на этой неделе", "завтра", "this week", "tomorrow", "within a week")
  private val monthWords = Vector("w tym miesiacu", "od pazdziernika", "od wrzesnia",
    "в этом месяце", "через месяц", "this month", "next month", "from october", "from september")
  private val laterWords = Vector("kiedys", "na przyszly rok", "docelowo",
    "когда-нибудь", "в будущем", "someday", "eventually", "long term")

  private def firstHit(text: String, words: Vector[String]): Boolean = words.exists(text.contains)

  def category(message: String): Category =
    val t = normal(message)
    cues.collectFirst { case (c, ws) if firstHit(t, ws) => c }.getOrElse(Category.Unknown)

  def city(message: String): Option[String] =
    val t = normal(message)
    cities.collectFirst { case (name, ws) if firstHit(t, ws) => name }

  /** the budget, by `okay-intent`'s own amount parser — which reads
   * "3000 zł", "do 3 tys.", "€500" and refuses what it cannot read */
  def budget(message: String): Option[Amount] = Amount.find(message).map(_.value)

  /**
   * How soon. Words first, and a DATE second: "od 15 października" is
   * an urgency even though no urgent word appears in it, and the
   * temporal parser already knows how to find it. A date more than a
   * month out is `Later`, not `Unknown` — the person did say when.
   */
  def urgency(message: String, today: Instant): Urgency =
    val t = normal(message)
    if firstHit(t, nowWords) then Urgency.Now
    else if firstHit(t, weekWords) then Urgency.Week
    else if firstHit(t, monthWords) then Urgency.Month
    else if firstHit(t, laterWords) then Urgency.Later
    else
      val d = today.atZone(ZoneOffset.UTC).toLocalDate
      Temporal.find(message, Temporal.Date(d.getYear, d.getMonthValue, d.getDayOfMonth)) match
        case Some(found) =>
          val w = found.value.date
          val days = java.time.LocalDate.of(w.year, w.month, w.day).toEpochDay - d.toEpochDay
          if days <= 1 then Urgency.Now else if days <= 7 then Urgency.Week
          else if days <= 31 then Urgency.Month else Urgency.Later
        case None => Urgency.Unknown

  /** everything at once: one turn, one row, nothing invented */
  def lead(message: String, sessionId: String, salt: String, at: Instant = Instant.now()): Lead =
    Lead(at, Lead.pseudonym(sessionId, salt), category(message), city(message),
      budget(message), urgency(message, at), Outcome.Open)

  /**
   * A later tier saw more than the cues did. Only the fields that were
   * `Unknown`/absent are filled: a model may complete a ledger, it may
   * not overwrite what a person actually wrote down.
   */
  def refine(l: Lead, category: Option[Category] = None, city: Option[String] = None,
             budget: Option[Amount] = None, urgency: Option[Urgency] = None): Lead =
    l.copy(
      category = if l.category == Category.Unknown then category.getOrElse(l.category) else l.category,
      city = l.city.orElse(city),
      budget = l.budget.orElse(budget),
      urgency = if l.urgency == Urgency.Unknown then urgency.getOrElse(l.urgency) else l.urgency)
