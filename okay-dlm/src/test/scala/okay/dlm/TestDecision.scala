package okay.dlm

import munit.FunSuite

class TestDecision extends FunSuite:
  import Decision.*

  case class Ev(route: Option[Route], pendingAct: Option[String] = None, courtesy: Boolean = false,
                      hasModel: Boolean = false, override val correcting: Boolean = false,
                      pairs: Set[String] = Set.empty) extends Evidence:
    override def distinguishable(pair: Vector[String]): Boolean = pairs(Phrasing.pairKey(pair(0), pair(1)))

  val st = State("ann", "ru")
  val fired = Route.Fires("need", Map.empty, Support.Exact(None))
  val unclear = Route.Unclear(Vector("need", "offer"), 0.4f)

  test("a question of ours outstanding: whatever comes next answers it, with the act head's verdict") {
    val waiting = st.copy(pending = Some(Pending.Field(Some("city"))))
    assertEquals(decide(waiting, Ev(Some(fired), pendingAct = Some("correct"))), Action.AnswerPending(Some("correct")))
    assertEquals(waiting.asked, Some("city"))
    assertEquals(decide(st.copy(pending = Some(Pending.Answer("confirm"))), Ev(None)), Action.AnswerPending(None))
  }

  test("nothing outstanding: the router decides") {
    assertEquals(decide(st, Ev(Some(fired))), Action.Act(fired))
  }

  test("the fallback ladder: courtesy, correction, a narrowing question, then one plain question, the menu, shorter") {
    assertEquals(decide(st, Ev(Some(unclear), courtesy = true)), Action.Acknowledge)
    assertEquals(decide(st, Ev(Some(unclear), correcting = true)), Action.ShowRecord)
    assertEquals(decide(st, Ev(Some(unclear), pairs = Set("need|offer"))), Action.Distinguish(Vector("need", "offer")))
    assertEquals(decide(st, Ev(Some(unclear))), Action.AskPlainly)
    assertEquals(decide(st, Ev(Some(unclear), hasModel = true)), Action.AskModel)
    assertEquals(decide(st.copy(stuck = 1), Ev(Some(unclear))), Action.Menu)
    assertEquals(decide(st.copy(stuck = 2), Ev(Some(unclear))), Action.Shorter)
    assertEquals(decide(st.copy(stuck = 9), Ev(Some(unclear))), Action.Shorter)
  }

  test("what counts as not understood, and what does not") {
    assert(Decision.unclear(Action.AskPlainly) && Decision.unclear(Action.Menu) && Decision.unclear(Action.Distinguish(Vector.empty)))
    assert(!Decision.unclear(Action.Acknowledge) && !Decision.unclear(Action.ShowRecord) && !Decision.unclear(Action.Act(fired)))
  }

  test("the record is a function of the action, and recall reads it back") {
    val actions = Vector(Action.AnswerPending(Some("meta")), Action.Act(fired), Action.Acknowledge,
      Action.AskPlainly, Action.AskModel, Action.ShowRecord, Action.Menu, Action.Shorter,
      Action.Distinguish(Vector("need", "offer")))
    for a <- actions do
      val r = record(a, asked = Some("city"), route = Some(unclear), also = Vector("contact"))
      assertEquals(recall(r), Some(a), r.toString)
      assertEquals(Record.decode(Record.encode(r)), Some(r), Record.encode(r).toString)
    // an answer carries the field and what the router READ, not a verdict
    val ans = record(Action.AnswerPending(None), Some("city"), Some(fired))
    assertEquals((ans.asks, ans.routed, ans.verdict), (Some("city"), Some(fired), None))
    // an acted verdict travels; an unclear one travels beside the ladder
    assertEquals(record(Action.Act(fired), None).verdict, Some(fired))
    assertEquals(record(Action.Menu, None, Some(unclear)).verdict, Some(unclear))
    assertEquals(record(Action.Menu, None, Some(fired)).verdict, None)
    // a record written before actions had names recalls nothing, honestly
    assertEquals(recall(Record("")), None)
  }

  test("the second facts a message carried are what was noticed beyond what was acted on") {
    val e = new Ev(Some(fired)) { override def noticed = Vector("need" -> Support.Exact(None), "contact" -> Support.Exact(None)) }
    assertEquals(dropped(e), Vector("contact"))
  }

  test("a standing row's (intent, rule) pair is unique, or the collision is named") {
    val rows = Vector(Standing.Row("a", "source", Some("city")), Standing.Row("b", "source", Some("bare")),
      Standing.Row("c", "source", Some("city")))
    assertEquals(Standing.collisions(rows).map((x, y) => (x.name, y.name)), Vector(("a", "c")))
  }
