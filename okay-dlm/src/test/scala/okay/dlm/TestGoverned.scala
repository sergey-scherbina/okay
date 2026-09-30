package okay.dlm

import munit.FunSuite

/** the learning mode, governed, audited, explainable, correctable —
 * one test per behavior line of specs/dlm-learning.md */
class TestGoverned extends FunSuite:
  import Ledger.Entry
  import Teaching.Channel

  val embed = okay.rag.Vectors.hashing(256)
  val need = Intent("need", rules = Vector("(?iU)\\b(?:нужен|ищу)\\b"),
    byLang = Map("ru" -> Vector("нужен сантехник", "ищу электрика")),
    slots = Vector(Slot("what", "(?iU)(?:нужен|ищу)\\s+(.+)", fallback = true)))
  val offer = Intent("offer", rules = Vector("(?iU)\\b(?:умею|могу)\\b"),
    byLang = Map("ru" -> Vector("умею чинить", "могу помочь с ремонтом")))
  val listings = Intent("listings", rules = Vector("(?iU)\\b(?:мои объявления)\\b"), semantic = false)
  val intents = Intents(Vector(need, offer, listings))
  val vectors = Exemplars.compile(intents.rows, embed, "hashing-256")
  val router = Router(intents, Some(vectors), Some(embed), margin = 0.2f)

  var clock = 1000L
  def governed(teaching: Teaching = Teaching.ours, rules: Memory.Rules = Memory.defaults,
               sink: Ledger.Sink = Ledger.silent) =
    Governed(router, teaching, sink, rules, now = () => { clock += 1; clock },
      encoder = "hashing-256", tables = Map("intents" -> vectors.hash))

  test("a lesson to a class the model does not have is refused, and the refusal is in the ledger") {
    val g = governed()
    assertEquals(g.teach("ann", "ann", "мои заявки", "orders"), Left("no such class: orders"))
    g.ledger() match
      case Vector(Entry.Refused("ann", what, "no such class: orders", _, "ann")) => assert(what.contains("orders"))
      case other => fail(s"$other")
    assertEquals(g.lessons("ann"), Vector.empty)
  }

  test("a lesson never changes a rule, a slot, a threshold or a table") {
    val g = governed()
    val before = (g.intents, g.router.question, g.router.margin, g.tables, vectors.hash)
    for i <- 1 to 5 do assert(g.teach("ann", "ann", s"мои заявки $i", "listings").isRight)
    assert(g.teach("ann", "ann", "нужен покой", "offer").isRight)   // overrules a rule for HER words
    assertEquals((g.intents, g.router.question, g.router.margin, g.tables, vectors.hash), before)
    assertEquals(g.intents.byName("need").get.rules, need.rules)
  }

  test("a person's lesson routes only that person's words until the sharing bar") {
    val g = governed(rules = Memory.Rules(people = 3))
    assert(g.teach("ann", "ann", "мои заявки", "listings").isRight)
    assertEquals(g.route("мои заявки", "ann").named, Some("listings"))
    assertEquals(g.route("мои заявки", "bob").named, None)   // too short for the judge, no lesson of his
    assert(g.teach("bob", "bob", "мои заявки", "listings").isRight)
    assertEquals(g.shared, Vector.empty)
    assert(g.teach("cid", "cid", "мои заявки", "listings").isRight)
    assertEquals(g.shared.map(_.intent), Vector("listings"))
    assertEquals(g.route("мои заявки", "stranger").named, Some("listings"))
    // and the ledger says when it became everyone's, with how many holders
    assert(g.ledger().exists { case Entry.Shared("мои заявки", "listings", 3, _, "cid") => true; case _ => false }, g.ledger().toString)
  }

  test("the kill switch: every teach a refusal, the fold as it was, no redeploy") {
    val g = governed(Teaching.off)
    assertEquals(g.teach("ann", "ann", "мои заявки", "listings"), Left("learning is off"))
    assertEquals(g.forget("ann", "ann", "мои заявки"), Left("withdrawal is off"))
    assertEquals(g.memory, Memory.empty)
    assertEquals(g.ledger().length, 2)
    // one channel switched, not all
    val lessonsOnly = governed(Teaching.switched(Teaching.ours, Channel.Withdrawal, on = false))
    assert(lessonsOnly.teach("ann", "ann", "мои заявки", "listings").isRight)
    assertEquals(lessonsOnly.forget("ann", "ann", "мои заявки"), Left("withdrawal is off"))
    assertEquals(lessonsOnly.lessons("ann").length, 1)
  }

  test("forget by the person, or by a steward — and not by a stranger") {
    val g = governed(Teaching.roles(stewards = _ == "boss"))
    assert(g.teach("ann", "ann", "мои заявки", "listings").isRight)
    assertEquals(g.forget("bob", "ann", "мои заявки"), Left("bob may not forget for ann"))
    assertEquals(g.lessons("ann").length, 1)
    assert(g.forget("boss", "ann", "мои заявки").isRight)
    assertEquals(g.lessons("ann"), Vector.empty)
    assert(g.teach("ann", "ann", "мои заявки", "listings").isRight)
    assert(g.forget("ann", "ann", "мои заявки").isRight)
    assertEquals(g.lessons("ann"), Vector.empty)
    // a steward may also teach for somebody
    assert(g.teach("boss", "ann", "покажи мои объявления", "listings").isRight)
    assertEquals(g.lessons("ann").map(_.who), Vector("ann"))
    assertEquals(g.ledger().collect { case e: Entry.Learned => e.by }, Vector("ann", "ann", "boss"))
  }

  test("share by a teacher makes one person's pair everyone's; by anybody else it is refused") {
    val g = governed(Teaching.roles(teachers = _ == "prof"))
    assertEquals(g.share("ann", "мои заявки", "listings"), Left("ann is not a teacher"))
    assert(g.share("prof", "мои заявки", "listings").isRight)
    assertEquals(g.shared.map(l => (l.intent, l.who)), Vector(("listings", "prof")))
    assertEquals(g.route("мои заявки", "anyone").named, Some("listings"))
    assert(g.ledger().exists { case Entry.Shared(_, "listings", 1, _, "prof") => true; case _ => false })
  }

  test("a person may be barred from a class, and told so in the ledger") {
    val g = governed(Teaching.roles(own = (who, intent) => !(who == "kid" && intent == "offer")))
    assertEquals(g.teach("kid", "kid", "могу всё", "offer"), Left("kid may not be taught offer"))
    assert(g.teach("kid", "kid", "мои заявки", "listings").isRight)
  }

  test("explain names the layer, the rule verbatim, the lesson and its owner, the judge and the encoder") {
    val g = governed()
    val byRule = g.explain("нужен сантехник", "ann")
    assertEquals(byRule.layer, Some(Layer.Rule))
    assertEquals(byRule.rule, need.rules.headOption)
    assertEquals((byRule.lesson, byRule.judge, byRule.encoder), (None, None, "hashing-256"))
    assertEquals(byRule.tables, Map("intents" -> vectors.hash))
    assert(g.teach("ann", "ann", "мои заявки", "listings").isRight)
    val byLesson = g.explain("мои заявки", "ann")
    assertEquals(byLesson.layer, Some(Layer.Memory))
    assertEquals(byLesson.lesson.map(l => (l.intent, l.who)), Some(("listings", "ann")))
    val byJudge = g.explain("помогу с ремонтом квартиры в выходные", "ann")
    assertEquals(byJudge.layer, Some(Layer.Semantic))
    assertEquals(byJudge.judge, Some("probe"))
    assert(byJudge.scores.nonEmpty)
    val nobody = g.explain("ок", "ann")
    assertEquals((nobody.layer, nobody.route.named, nobody.rule, nobody.lesson), (None, None, None, None))
    // …and it travels as JSON, layer and judge included
    val printed = okay.codec.Json.print(Explanation.encode(byJudge))
    assert(printed.contains("\"judge\":\"probe\"") && printed.contains("\"layer\":\"semantic\""), printed)
  }

  test("the ledger replays: the fold over Learned/Forgotten is the memory the live value holds") {
    val sink = Ledger.Recorded()
    val g = governed(Teaching.roles(teachers = _ == "prof"), Memory.Rules(people = 2), sink)
    assert(g.teach("ann", "ann", "мои заявки", "listings").isRight)
    assert(g.teach("bob", "bob", "мои заявки", "listings").isRight)
    assert(g.teach("ann", "ann", "что сегодня", "offer").isRight)
    assert(g.forget("ann", "ann", "что сегодня").isRight)
    assert(g.share("prof", "покажи всё", "listings").isRight)
    val replayed = Ledger.replay(sink.entries, Memory.Rules(people = 2), Teaching.roles(teachers = _ == "prof"))
    assertEquals(replayed, g.memory)
    // the wire round trip keeps every entry
    assertEquals(sink.entries.map(e => Ledger.decode(Ledger.encode(e))), sink.entries.map(Some(_)))
  }

  test("a rebuilt table is an entry with the hash before and after and its corpus") {
    val g = governed()
    val e = g.rebuilt("ci", "intents", Some("abc"), vectors.hash, "corpus/intents.json@f00")
    assertEquals(e, Entry.Rebuilt("intents", "hashing-256", Some("abc"), vectors.hash, "corpus/intents.json@f00", clock, "ci"))
    // the hash is a pure function of the numbers
    assertEquals(Exemplars.compile(intents.rows, embed, "hashing-256").hash, vectors.hash)
    assertNotEquals(vectors.copy(encoder = "other").hash, vectors.hash)
  }

  test("with learning off, a replay of the ledger reaches the same decisions as were recorded") {
    val sink = Ledger.Recorded()
    val live = governed(sink = sink)
    assert(live.teach("ann", "ann", "мои заявки", "listings").isRight)
    val texts = Vector("нужен сантехник", "мои заявки", "помогу с ремонтом квартиры в выходные", "ок")
    val recorded = texts.map(t => live.route(t, "ann"))
    // the same router over the ledger folded with learning off: the
    // lesson is not armed, and every other decision is the same
    val cold = Governed(router, Teaching.off, initial = Ledger.replay(sink.entries, teaching = Teaching.off))
    val again = texts.map(t => cold.route(t, "ann"))
    assertEquals(again.zip(recorded).count(_ != _), 1)
    assertEquals(again(1).named, None)
    assertEquals(again.patch(1, Nil, 1), recorded.patch(1, Nil, 1))
    // and folded with learning on, the lesson is back and nothing else moved
    val warm = Governed(router, initial = Ledger.replay(sink.entries))
    assertEquals(texts.map(t => warm.route(t, "ann")), recorded)
  }

  test("AN EXPLANATION OF A MISSING SLOT still names the layer that decided the intent — a rule's, and a lesson's") {
    // the intent is clear, the value it cannot work without is not: the
    // route carries no support, and without the fallback to `noticed` the
    // audit reads «layer: none» for a turn a layer decided
    val asks = Intent("asks", rules = Vector("(?iU)\\b(?:проверь)\\b"),
      byLang = Map("ru" -> Vector("проверь адрес")),
      slots = Vector(Slot("address", "(?iU)(0x[0-9a-fA-F]{40})")), require = Vector("address"))
    val set = Intents(Vector(asks, offer))
    val r = Router(set, alphabet = Alphabet.of("ru", "en").toOption.get)
    val g = Governed(r, now = () => { clock += 1; clock })
    val byRule = g.explain("проверь это", "ann")
    assertEquals(byRule.route, Route.Missing("asks", "address"))
    assertEquals(byRule.layer, Some(Layer.Rule))
    assertEquals(byRule.rule, Some("(?iU)\\b(?:проверь)\\b"))
    // and by a LESSON, which is the case learning must be able to show
    assert(g.teach("ann", "ann", "а это вообще надёжно", "asks").isRight)
    val byLesson = g.explain("а это вообще надёжно", "ann")
    assertEquals(byLesson.route, Route.Missing("asks", "address"))
    assertEquals(byLesson.layer, Some(Layer.Memory))
    assertEquals(byLesson.lesson.map(l => l.text -> l.who), Some("а это вообще надёжно" -> "ann"))
    // a route that names no intent still names no layer, which is honest
    assertEquals(g.explain("qwzx", "ann").layer, None)
  }

  // ---- §10, dlm-erasure: a person's own words out of the ledger

  test("ERASURE: the person's sentences go from the ledger and the memory; the FACT stays, by a digest, and is never itself erased") {
    val store = Ledger.Recorded()
    val g = governed(sink = store)
    assert(g.teach("ann", "ann", "мои заявки", "listings").isRight)
    assert(g.teach("ann", "ann", "нужен покой", "offer").isRight)
    assert(g.teach("bob", "bob", "мои объявления сейчас", "listings").isRight)
    assertEquals(g.teach("ann", "ann", "мои заявки", "orders").isLeft, true)   // a refusal names her too
    val erased = g.erase("ann", "ann", "the person asked").toOption.get
    // nothing of hers is left anywhere: the sink, this value's own history, the memory
    val left = (store.entries ++ g.ledger()).distinct
    assert(!left.exists(e => Ledger.line(e).contains("мои заявки")), left.map(Ledger.line).mkString("\n"))
    assert(!left.exists(e => Ledger.line(e).contains("\"ann\"")) , "not even her identifier, except as a digest")
    assertEquals(g.lessons("ann"), Vector.empty)
    // bob is untouched — erasing one person is not forgetting everybody
    assertEquals(g.lessons("bob").map(_.intent), Vector("listings"))
    // and the record: how many entries went, why, who did it, and the subject as a digest
    erased match
      case Entry.Erased(subject, n, "the person asked", _, actor) =>
        assertEquals(subject, Ledger.digest("ann"))
        assert(subject.startsWith("sha256:") && !subject.contains("ann"), subject)
        assertEquals(actor, subject, "she erased herself: recorded as the subject, not by name")
        assertEquals(n, 3, "her two lessons and the refusal she was given; nothing of hers was shared, so there is no Shared to take")
      case other => fail(s"$other")
    // a second erasure does not take the first record away
    assert(g.erase("ann", "ann", "again").isRight)
    assertEquals(g.ledger().count { case Entry.Erased(_, _, _, _, _) => true; case _ => false }, 2)
  }

  test("erasure works with LEARNING SWITCHED OFF, and only for oneself or by a steward") {
    val store = Ledger.Recorded()
    val g = governed(sink = store)
    assert(g.teach("ann", "ann", "мои заявки", "listings").isRight)
    // a stranger may not
    assertEquals(g.erase("eve", "ann", "curious"), Left("eve may not erase for ann"))
    assert(g.lessons("ann").nonEmpty, "and nothing of hers moved")
    // a steward may, for somebody else
    val steward = Governed(router, Teaching.roles(stewards = _ == "root"), store, now = () => { clock += 1; clock })
    steward.erase("root", "ann", "a request by mail") match
      case Right(Entry.Erased(subject, _, _, _, "root")) => assertEquals(subject, Ledger.digest("ann"),
        "the steward is named — an operator is not the data subject — and she is not")
      case other => fail(s"$other")
    assertEquals(Ledger.replay(store.entries).forPerson("ann"), Vector.empty)
    // AND WITH THE KILL SWITCH THROWN: a system that cannot learn must still forget
    val off = Governed(router, Teaching.off, Ledger.Recorded(), now = () => { clock += 1; clock })
    assertEquals(off.teach("ann", "ann", "мои заявки", "listings").isLeft, true, "learning is off")
    assert(off.erase("ann", "ann", "the person asked").isRight, "erasure is not")
  }

  test("the pure erase: whose entry is whose, and a table's entry is nobody's words") {
    val entries = Vector[Entry](
      Entry.Learned("ann", "мои заявки", "listings", 1, 10, "ann"),
      Entry.Learned("bob", "мои объявления", "listings", 2, 11, "bob"),
      Entry.Forgotten("ann", "мои заявки", 12, "ann"),
      Entry.Shared("мои заявки", "listings", 3, 13, "ann"),
      Entry.Refused("ann", "teach x", "no such class: x", 14, "root"),
      Entry.Refused("bob", "teach y", "no such class: y", 15, "ann"),
      Entry.Rebuilt("intents", "hashing-256", None, "h2", "corpus", 16, "root"),
      Entry.Pruned("intents", "h1", "last:5", 17, "root"),
      Entry.Erased("sha256:whoever", 3, "asked", 18, "root"))
    val (stays, gone) = Ledger.erase(entries, "ann")
    assertEquals(gone, 5, "her lessons, her withdrawal, what she shared, the refusal to her, the refusal she caused")
    assertEquals(stays.size, 4)
    assert(stays.exists { case Entry.Learned("bob", _, _, _, _, _) => true; case _ => false })
    assert(stays.exists { case Entry.Erased(_, _, _, _, _) => true; case _ => false }, "the evidence of an erasure always stays")
    assert(stays.exists { case Entry.Rebuilt(_, _, _, _, _, _, _, _) => true; case _ => false }, "a table's entry is nobody's words")
    assertEquals(Ledger.erase(entries, "nobody")._2, 0)
  }
