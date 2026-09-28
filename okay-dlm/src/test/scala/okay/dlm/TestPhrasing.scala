package okay.dlm

import munit.FunSuite

class TestPhrasing extends FunSuite:

  val raw = """{
    "_": "notes for the person editing this file are ignored",
    "readback": {"work-need": {"ru": "Записал: {what}. Верно?", "en": "Noted: {what}. Right?"}},
    "distinguish": {"need|offer": {"ru": "Вам нужен мастер — или вы предлагаете?"},
                    "housing": {"ru": "сдаёте — или ищете, где снять?", "en": "renting out — or looking?"}},
    "field": {"city": {"ru": "город", "en": "city"}}
  }"""
  val ph = Phrasing.parse(raw).toOption.get

  test("cells are keyed by language and `{what}` is filled") {
    assertEquals(ph.readBack("work-need", "ru", "Scala"), Some("Записал: Scala. Верно?"))
    assertEquals(ph.readBack("work-need", "pl", "Scala"), None)
    assertEquals(ph.distinguishing("housing", "en"), Some("renting out — or looking?"))
    assertEquals(ph.fieldName("city", "en"), Some("city"))
  }

  test("a pair is distinguishable when its question is written in this language, order-free") {
    assert(ph.distinguishable(Vector("offer", "need"), "ru"))
    assert(!ph.distinguishable(Vector("offer", "need"), "en"))
    assert(!ph.distinguishable(Vector("a", "b", "c"), "ru"))
    assertEquals(Phrasing.pairKey("offer", "need"), "need|offer")
  }

  test("a cell written in some languages and not all is a hole, named") {
    assertEquals(ph.holes(Set("ru", "en")), Vector(("distinguish", "need|offer", Vector("en"))))
    assertEquals(ph.holes(Set("ru")), Vector.empty)
  }

  test("which cell a reply came from, by its template") {
    assertEquals(ph.cellOf("Записал: Scala. Верно? (черновик)"), Some(("readback", "work-need", "ru")))
    assertEquals(ph.cellOf("renting out — or looking?"), Some(("distinguish", "housing", "en")))
    assertEquals(ph.cellOf("something else"), None)
    assert(Phrasing.fromTemplate("a {what} b", "a xyz b and more"))
    assert(!Phrasing.fromTemplate("a {what} b", "a xyz c"))
  }

  test("malformed cells are refused, and an absent resource is empty") {
    assert(Phrasing.parse("""{"readback": {"k": "not an object"}}""").isLeft)
    assertEquals(Phrasing.resource("/no-such.json"), Phrasing.empty)
  }
