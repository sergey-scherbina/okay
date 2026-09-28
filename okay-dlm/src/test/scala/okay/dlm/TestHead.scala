package okay.dlm

import munit.FunSuite

class TestHead extends FunSuite:

  val embed = okay.rag.Vectors.hashing(256)
  val acts = Exemplars.compile(Vector(
    "answer" -> "Вроцлав", "answer" -> "программист на Scala", "answer" -> "пять лет опыта",
    "social" -> "спасибо большое", "social" -> "хорошего дня", "social" -> "благодарю вас",
    "correct" -> "нет, я имел в виду другое", "correct" -> "ты меня не понял", "correct" -> "не так, исправь"),
    embed, "hashing-256")

  test("a head answers the winner above the margin, and its quiet class is silence") {
    val head = Head(Some(acts), Some(embed), margin = 0.2f, quiet = Some("answer"))
    assert(head.live)
    assertEquals(head.classes, Vector("answer", "social", "correct"))
    assertEquals(head.of("спасибо большое"), Some("social"))
    assertEquals(head.of("ты меня не понял"), Some("correct"))
    assertEquals(head.of("Вроцлав"), None)              // the widest class, and silence means it
    assertEquals(head.scores("спасибо большое").head._1, "social")
    assert(head.verdict("спасибо большое").exists(_.best == "social"))
  }

  test("the same head at a different bar is the caller's business") {
    // a bar no margin of probabilities can reach: the head is silent
    val head = Head(Some(acts), Some(embed), margin = 2f)
    assertEquals(head.of("спасибо большое"), None)
    assertEquals(head.of("спасибо большое", 0.2f), Some("social"))
  }

  test("a head with nothing behind it never answers, and says so") {
    assert(!Head.off.live)
    assertEquals(Head.off.of("anything"), None)
    assertEquals(Head.off.scores("anything"), Vector.empty)
    assert(!Head(Some(acts), None, 0.2f).live)
  }

  test("the model as one value: named heads, live tiers") {
    val intents = Intents(Vector(Intent("need", rules = Vector("(?iU)\\b(?:нужен)\\b"),
      byLang = Map("ru" -> Vector("нужен сантехник")))))
    given Embedder = Embedder.of("hashing-256", 256, embed)
    val m = Dlm.of(intents, heads = Map("acts" -> (acts, 0.2f))).toOption.get
    assertEquals(m.head("acts").of("спасибо большое"), Some("social"))
    // a table another encoder compiled is refused by name, not read
    assert(Dlm.of(intents, heads = Map("acts" -> (acts.copy(encoder = "other"), 0.2f))).left.exists(_.contains("refused")))
    assertEquals(m.head("none").of("спасибо большое"), None)
    assertEquals(m.tiers, Vector("rules", "typos", "acts", "language"))
    assertEquals(Dlm.rules(intents).tiers, Vector("rules", "typos"))
  }
