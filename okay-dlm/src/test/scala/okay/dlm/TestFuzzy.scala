package okay.dlm

import munit.FunSuite

class TestFuzzy extends FunSuite:

  test("bounded Levenshtein: the true distance up to the bound, the bound plus one past it") {
    assertEquals(Fuzzy.distance("ищу", "ищю", 1), 1)
    assertEquals(Fuzzy.distance("нужен", "нужн", 1), 1)
    assertEquals(Fuzzy.distance("сантехник", "программист", 2), 3)
    assertEquals(Fuzzy.distance("abc", "abc", 0), 0)
    assertEquals(Fuzzy.distance("Abc", "aBC", 0), 0)   // case does not count
  }

  test("tolerance grows with the word: none for two letters, one to five, two after") {
    assertEquals(Fuzzy.tolerance(2), 0)
    assertEquals(Fuzzy.tolerance(3), 1)
    assertEquals(Fuzzy.tolerance(5), 1)
    assertEquals(Fuzzy.tolerance(6), 2)
  }

  test("«да» and «не» are one edit apart and are never each other's typo") {
    assertEquals(Fuzzy.bestMatch("да", Vector("не")), None)
    assertEquals(Fuzzy.bestMatch("ищю", Vector("ищу", "умею")), Some("ищу" -> 1))
  }

  test("only the simple rule shape yields fuzzy vocabulary") {
    assertEquals(Fuzzy.literalTriggers("(?iU)\\b(?:нужен|нужна|ищу)\\b"), Vector("нужен", "нужна", "ищу"))
    assertEquals(Fuzzy.literalTriggers("(?iU)\\b(?:ремонт|чин)\\w*\\b"), Vector("ремонт", "чин"))
    assertEquals(Fuzzy.literalTriggers("(?iU)\\b(?:берусь)\\s+(\\d+)"), Vector.empty)
    assertEquals(Fuzzy.literalTriggers("(?iU)^[\\w.]+@[\\w.]+$"), Vector.empty)
    assertEquals(Fuzzy.literalTriggers("(?iU)\\b(?:нужен сантехник)\\b"), Vector.empty)
  }

  test("tokens carry their span") {
    assertEquals(Fuzzy.tokenize("ищу, программиста!").map(t => (t.text, t.start, t.end)),
      Vector(("ищу", 0, 3), ("программиста", 5, 17)))
  }

  test("collisions name the trigger pairs from different intents a typo could confuse") {
    val vocab = Map("need" -> Vector("ищу", "нужен"), "offer" -> Vector("ищем", "умею"), "rent" -> Vector("ищи"))
    val found = Fuzzy.collisions(vocab)
    // «ищу»/«ищем» and «нужен»/«нужна» are two edits apart at a tolerance of one: not collisions
    assertEquals(found.map(c => (c._2, c._4)), Vector(("ищу", "ищи")))
    // …unless the two belong to different languages, which the router never mixes
    val isolated = Fuzzy.collisions(vocab, w => if w == "ищу" then Set("ru") else Set("uk"))
    assertEquals(isolated, Vector.empty)
  }
