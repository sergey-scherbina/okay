package okay.dlm

import munit.FunSuite

/** the seams: an encoder, a judge, a detector — ours by default, any
 * other by a given, and the doors that summon them */
class TestJudge extends FunSuite:

  val embed = okay.rag.Vectors.hashing(256)
  val acts = Exemplars.compile(Vector(
    "answer" -> "Вроцлав", "answer" -> "программист на Scala",
    "social" -> "спасибо большое", "social" -> "хорошего дня",
    "correct" -> "ты меня не понял", "correct" -> "не так, исправь"), embed, "hashing-256")

  /** a judge that reads the question's words and nothing else — what
   * a remote one looks like from here */
  final class Fixed(answer: Map[String, Double], conf: Option[Double] = None) extends Judge:
    val name = "fixed"
    var asked = Vector.empty[Judge.Question]
    def choose(text: String, q: Judge.Question) =
      asked :+= q
      Judge.Choice.of(answer.filter((k, _) => q.names.contains(k)), conf)

  test("ours: the probe answers over the options ASKED, renormalised, with no confidence to claim") {
    val j = Judge.probe(acts, embed)
    val all = j.choose("спасибо большое", Judge.Question.of(acts.labels)).get
    assertEquals(all.best, "social")
    assertEquals(all.confidence, None)
    assertEqualsDouble(all.probabilities.map(_._2).sum, 1.0, 1e-9)
    val two = j.choose("спасибо большое", Judge.Question.of(Vector("answer", "correct"))).get
    assertEquals(two.probabilities.map(_._1).toSet, Set("answer", "correct"))
    assertEqualsDouble(two.probabilities.map(_._2).sum, 1.0, 1e-9)
    assertEquals(Judge.probe(Exemplars.empty, embed).choose("x", Judge.Question.of(Vector("a"))), None)
  }

  test("a choice knows its margin and runner-up, and ranks what it is given") {
    val c = Judge.Choice.of(Map("a" -> 0.2, "b" -> 0.7, "c" -> 0.1), Some(0.9)).get
    assertEquals((c.best, c.runnerUp, c.confidence), ("b", Some("a"), Some(0.9)))
    assertEqualsDouble(c.margin, 0.5, 1e-9)
    assertEquals(Judge.Choice.of(Map.empty[String, Double]), None)
  }

  test("the default given is ours: an encoder that needs nothing, a judge fitted from the table") {
    val e = summon[Embedder]
    assertEquals(e.name, "hashing-256")
    assertEquals(e.dim, 256)
    val fit = summon[Judge.Fit]
    assertEquals(fit(acts).name, "probe")
    // a table compiled through the encoder in scope carries its name
    assertEquals(Exemplars.compile(Vector("a" -> "x")).encoder, "hashing-256")
  }

  test("a given in scope replaces ours everywhere a door summons it") {
    given Judge.Fit = Judge.Fit.constant(Fixed(Map("social" -> 0.9, "answer" -> 0.1), Some(0.8)))
    val head = Head.of(Some(acts), margin = 0.5f, quiet = Some("answer"),
      instructions = "what kind of move is this?", descriptions = Map("social" -> "a pleasantry"))
    assertEquals(head.judge.name, "fixed")
    assertEquals(head.of("anything at all"), Some("social"))
    assertEquals(head.verdict("x").flatMap(_.confidence), Some(0.8))
    // the words a remote judge is told
    assertEquals(head.question.instructions, "what kind of move is this?")
    assertEquals(head.question.options.toMap.apply("social"), "a pleasantry")
    assertEquals(head.question.options.toMap.apply("answer"), "")
    // and the model's own door takes the same given
    val intents = Intents(Vector(Intent("need", byLang = Map("ru" -> Vector("нужен сантехник")))))
    val m = Dlm.of(intents, exemplars = Some(Exemplars.compile(intents.rows)), heads = Map("acts" -> (acts, 0.5f))).toOption.get
    assertEquals(m.router.judge.map(_.name), Some("fixed"))
    assertEquals(m.head("acts").judge.name, "fixed")
  }

  test("a judge behind the router's rules: asked only what a rule could not place, never an exact-argument intent") {
    val need = Intent("need", rules = Vector("(?iU)\\b(?:нужен)\\b"), byLang = Map("ru" -> Vector("нужен сантехник")))
    val offer = Intent("offer", byLang = Map("ru" -> Vector("умею чинить")))
    val accept = Intent("accept", rules = Vector("(?iU)\\b(?:берусь)\\b"), semantic = false)
    val intents = Intents(Vector(need, offer, accept))
    val fixed = Fixed(Map("offer" -> 0.95, "need" -> 0.05))
    val r = Router.judged(intents, fixed, margin = 0.5f)
    assertEquals(r.question.names, Vector("need", "offer"))
    assertEquals(r.route("нужен электрик сегодня").named, Some("need"))        // a rule, the judge unasked
    assertEquals(fixed.asked, Vector.empty)
    assertEquals(r.route("могу починить стиральную машину").named, Some("offer"))  // the judge
    assertEquals(fixed.asked.length, 1)
    assertEquals(r.scores("что угодно длиннее двенадцати букв").head._1, "offer")
  }

  test("orElse: the first that answers; guarded: a throw is a strike, three retire the judge for the cooldown") {
    var t = 0L
    var failures = 0
    val broken: Judge = new Judge:
      val name = "broken"
      def choose(text: String, q: Judge.Question) = throw RuntimeException("no credits")
    val g = Judge.guarded(broken, timeoutMs = 1000L, retireAfter = 3, cooldownMs = 500L, now = () => t, report = _ => failures += 1)
    val q = Judge.Question.of(Vector("a", "b"))
    for _ <- 1 to 3 do assertEquals(g.choose("x", q), None)
    assertEquals(failures, 3)
    // retired: no call, no failure counted
    assertEquals(g.choose("x", q), None)
    assertEquals(failures, 3)
    t = 500L
    assertEquals(g.choose("x", q), None)
    assertEquals(failures, 4)
    // …and ours behind it answers meanwhile
    val both = Judge.orElse(g, Judge.probe(acts, embed))
    assertEquals(both.name, "broken|probe")
    assertEquals(both.choose("спасибо большое", Judge.Question.of(acts.labels)).map(_.best), Some("social"))
    // a slow judge is a timeout, and the answer arrives from nobody
    val slow: Judge = new Judge:
      val name = "slow"
      def choose(text: String, q: Judge.Question) = { Thread.sleep(2000); None }
    assertEquals(Judge.guarded(slow, timeoutMs = 50L).choose("x", q), None)
  }

  test("an encoder as a value: any function under the name its artifacts carry; the static table's blind spot is the zero vector") {
    val e = Embedder.of("minilm", 3, _ => okay.rag.embedding(Array(1f, 0f, 0f)))
    assertEquals((e.name, e.dim, e("x").length), ("minilm", 3, 3))
    val table = okay.intent.Static.table(Map("привет" -> okay.rag.embedding(Array(0f, 1f))), Seq("привет"), split = okay.intent.Static.units3)
    val s = Embedder.static("minilm", table)
    assertEquals(s.name, "static:minilm")
    assertEquals(s("привет").toVector, Vector(0f, 1f))
    assertEquals(s("никогда не виденное").toVector, Vector(0f, 0f))
  }

  test("a judge as the language detector, gated by the alphabet like ours") {
    val alphabet = Alphabet.of("ru", "pl").toOption.get
    val fixed = Fixed(Map("pl" -> 0.9, "ru" -> 0.1))
    val d = Language.Judged(fixed, Vector("ru" -> "Russian", "pl" -> "Polish"), margin = 0.3f, alphabet = alphabet)
    assertEquals(d.of("szukam pracy"), Some("pl"))
    assertEquals(d.of("ищу работу"), None)               // Cyrillic text is never Polish, whatever a judge says
    assertEquals(d.scores("szukam pracy").head._1, "pl")
    assertEquals(fixed.asked.head.options.toMap.apply("ru"), "Russian")
    assertEquals(Language.Detector.none.of("anything"), None)
    assert(!Language.Detector.none.nonEmpty)
  }
