package okay.leads

import okay.leads.Lead.Outcome
import java.time.Instant

/**
 * A ledger with plausible traffic in it, so `Report` can be seen doing
 * its job before a single real user exists:
 * `sbt "okayLeads/Test/runMain okay.leads.Seed /tmp/leads.csv"`.
 */
object Seed:
  private val messages = Vector(
    "szukam mieszkania 2 pokoje we Wrocławiu do 3000 zł, pilne",
    "ищу работу java разработчиком во Вроцлаве",
    "potrzebny hydraulik na dziś, Wrocław",
    "kupię rower dla dziecka do 500 zł",
    "куда сходить в выходные во Вроцлаве с детьми",
    "wynajmę kawalerkę w Krakowie od 15 października, budżet 2200 zł",
    "need a plumber tomorrow in Wroclaw",
    "szukam pracy na pół etatu, od tego miesiąca",
    "mieszkanie 3 pokoje Wrocław Krzyki, do 4000 zł",
    "sprzedam meble, Wrocław")

  def main(args: Array[String]): Unit =
    val path = args.headOption.getOrElse("/tmp/leads.csv")
    val ledger = Ledger.at(path)
    val start = Instant.parse("2026-08-24T09:00:00Z")
    val leads = for
      day <- 0 until 18
      i <- 0 to (day % 3)
      msg = messages((day * 3 + i) % messages.length)
    yield Leads.watch(ledger, "demo-salt")(s"conv-$day-$i", msg, start.plusSeconds(86400L * day + 3600L * i))
    // a third of them ended in something, and some left a contact
    leads.zipWithIndex.foreach:
      case (l, i) if i % 3 == 0 => Leads.outcome(ledger, l, Outcome.Matched, Some(s"tg:@user$i"))
      case (l, i) if i % 7 == 0 => Leads.outcome(ledger, l, Outcome.Dropped)
      case _ => ()
    println(s"seeded ${leads.size} leads into $path")
    Report.main(Array(path))
