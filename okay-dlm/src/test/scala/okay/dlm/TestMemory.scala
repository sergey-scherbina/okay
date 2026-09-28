package okay.dlm

import munit.FunSuite

class TestMemory extends FunSuite:
  import Memory.Event.*

  test("the fold: taught adds, the latest teaching of one sentence wins, withdrawn removes") {
    val m = Memory.of(Vector(
      Taught("ann", "мои заявки", "offer", 1L, 100L),
      Taught("ann", "Мои  заявки!", "listings", 2L, 101L),
      Taught("ann", "что сегодня", "whatson", 3L, 102L),
      Withdrawn("ann", "что сегодня")))
    assertEquals(m.mine("ann").map(l => (l.text, l.intent, l.offset)), Vector(("Мои  заявки!", "listings", 2L)))
    assertEquals(m.shared, Vector.empty)
  }

  test("nothing before `since` is armed; a person holds at most `perPerson`, oldest out") {
    val rules = Memory.Rules(since = 100L, perPerson = 2)
    val m = Memory.of(Vector(
      Taught("ann", "старое", "a", 1L, 50L),
      Taught("ann", "первое", "a", 2L, 100L),
      Taught("ann", "второе", "b", 3L, 101L),
      Taught("ann", "третье", "c", 4L, 102L)), rules)
    assertEquals(m.mine("ann").map(_.text), Vector("второе", "третье"))
  }

  test("a pair enough distinct people taught is shared and serves everyone; a teacher shares alone") {
    val events = Vector(
      Taught("a", "мои заявки", "listings", 1L, 1L),
      Taught("b", "мои заявки", "listings", 2L, 2L),
      Taught("c", "мои заявки", "listings", 3L, 3L),
      Taught("d", "мои заявки", "offer", 4L, 4L))   // a different intent: a different pair
    val m = Memory.of(events, Memory.Rules(people = 3))
    assertEquals(m.shared.map(l => (l.intent, l.offset)), Vector(("listings", 1L)))
    assertEquals(Memory.exact(m, "stranger", "мои заявки").map(_._1.intent), Some("listings"))
    val taught = Memory.of(Vector(Taught("t", "покажи всё", "show", 9L, 9L)),
      Memory.Rules(teacher = _ == "t"))
    assertEquals(taught.shared.map(_.intent), Vector("show"))
    // and forgetting the only holder's lesson unshares it
    assertEquals(Memory.forget(taught, "t", "покажи всё", Memory.Rules(teacher = _ == "t")).shared, Vector.empty)
  }

  test("a control word or two letters teach nothing") {
    val rules = Memory.Rules(control = Set("да", "нет").contains)
    assertEquals(Memory.learn(Memory.empty, "a", "да", "x", 1L, rules), Memory.empty)
    assertEquals(Memory.learn(Memory.empty, "a", "ок", "x", 1L), Memory.empty)
  }

  test("the exact band: the same words, or a typo per word within two edits in all, same count and order") {
    val m = Memory.learn(Memory.empty, "a", "мои заявки", "listings", 1L)
    assertEquals(Memory.exact(m, "a", "МОИ ЗАЯВКИ").map(_._2), Some(1.0f))
    assertEquals(Memory.exact(m, "a", "мои заявкы").map(_._2), Some(0.8f))
    assertEquals(Memory.exact(m, "a", "заявки мои"), None)         // order matters
    assertEquals(Memory.exact(m, "a", "мои заявки сегодня"), None) // count matters
    assertEquals(Memory.exact(m, "b", "мои заявки"), None)         // whose lesson matters
  }

  test("the near band: this person's own lessons to a semantic intent, by cosine against a bar") {
    val embed = okay.rag.Vectors.hashing(256)
    val m = Memory.learn(Memory.empty, "a", "покажи мои объявления", "listings", 1L)
    val hit = Memory.near(m, "a", "покажи мои объявления пожалуйста", embed, _ == "listings", 0.5f)
    assert(hit.exists(_._2 >= 0.5f), hit.toString)
    assertEquals(Memory.near(m, "a", "покажи мои объявления пожалуйста", embed, _ => false, 0.5f), None)
    assertEquals(Memory.near(m, "a", "совсем другое предложение", embed, _ == "listings", 0.9f), None)
  }
