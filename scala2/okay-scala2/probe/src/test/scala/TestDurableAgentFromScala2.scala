package scala2probe

import okay.agent.Durable
import okay.codec.Schema
import okay.scala2._

object DurableModel {
  final case class Pay(to: String, cents: Int)
  object Pay {
    implicit val schema: Schema[Pay] = Schemas.product2("Pay", "to", "cents")(Pay.apply)(p => (p.to, p.cents))
  }
}

/** a durable agent from Scala 2.13: okay-agent's Durable through Chat (stage 15.7) */
class TestDurableAgentFromScala2 extends munit.FunSuite {
  import DurableModel._

  def script: Model = Model.scriptedCalls(
    ("paying", Seq("pay" -> """{"to":"ada","cents":500}""")),
    ("paid", Seq.empty))

  test("a restart over the same journal replays the payment instead of paying twice") {
    var payments = 0
    val tools = Tools.empty.on[Pay]("pay", "send money")(p => { payments += 1; s"sent ${p.cents} to ${p.to}" })
    val journal = new Durable.MemoryJournal

    val first = Chat(script, tools, Policy.all, journal = Some(journal))
    assertEquals(Eff.runAsync(first.say("pay ada")), "paid")
    assertEquals(payments, 1)
    assertEquals(journal.all.map(e => (e.op, e.answer)), Vector(("pay", Some("sent 500 to ada"))))

    // the process died; a new one runs the same conversation over the journal
    val second = Chat(script, tools, Policy.all, journal = Some(journal))
    assertEquals(Eff.runAsync(second.say("pay ada")), "paid")
    assertEquals(payments, 1)
  }

  test("without a journal the same restart pays again") {
    var payments = 0
    val tools = Tools.empty.on[Pay]("pay", "send money")(p => { payments += 1; s"sent ${p.cents} to ${p.to}" })
    Eff.runAsync(Chat(script, tools, Policy.all).say("pay ada"))
    Eff.runAsync(Chat(script, tools, Policy.all).say("pay ada"))
    assertEquals(payments, 2)
  }
}
