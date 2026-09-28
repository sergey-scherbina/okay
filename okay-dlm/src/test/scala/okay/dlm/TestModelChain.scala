package okay.dlm

import munit.FunSuite

class TestModelChain extends FunSuite:
  import ModelChain.Event

  val ask = Lane.Ask("что сегодня?", Nil, Map.empty, "ru")

  test("the first lane that answers wins; a failed, empty or thrown lane is skipped") {
    val chain = ModelChain(Vector(
      Lane("empty", _ => ""), Lane("throws", _ => throw RuntimeException("no credits")),
      Lane("null", _ => null), Lane("ok", _ => "  привет  "), Lane("never", _ => fail("asked after an answer"))))
    assertEquals(chain.answer(ask), Some("ok" -> "привет"))
  }

  test("three strikes retire a lane for the cooldown, and the clock brings it back") {
    var t = 0L
    var events = Vector.empty[Event]
    val chain = ModelChain(Vector(Lane("flaky", _ => ""), Lane("ok", _ => "x")),
      retireAfter = 3, cooldownMs = 1000L, now = () => t, report = events :+= _)
    for _ <- 1 to 3 do chain.answer(ask)
    assertEquals(chain.live.map(_.name), Vector("ok"))
    assert(events.exists { case Event.Retired("flaky", 1000L) => true; case _ => false }, events.toString)
    t = 1000L
    assertEquals(chain.live.map(_.name), Vector("flaky", "ok"))
  }

  test("a daily cap counts every attempt, is seeded from a journal, and closes the lane until tomorrow") {
    var t = 0L
    var events = Vector.empty[Event]
    val chain = ModelChain(Vector(Lane("paid", _ => "x")), dailyCap = 2, now = () => t, report = events :+= _)
    chain.seed("paid", 0L)
    assertEquals(chain.spentToday("paid"), 1)
    assertEquals(chain.answer(ask), Some("paid" -> "x"))
    assert(chain.capped("paid"))
    assertEquals(chain.answer(ask), None)
    assert(events.exists { case Event.Capped("paid", 2) => true; case _ => false })
    t = 24L * 3600 * 1000
    assert(!chain.capped("paid"))
  }

  test("a timeout is a failure the request path does not wait past") {
    var events = Vector.empty[Event]
    val chain = ModelChain(Vector(Lane("slow", _ => { Thread.sleep(2000); "late" }), Lane("ok", _ => "x")),
      timeoutMs = 50L, report = events :+= _)
    assertEquals(chain.answer(ask), Some("ok" -> "x"))
    assert(events.exists { case Event.TimedOut("slow", 50L) => true; case _ => false })
  }

  test("an answer with a fingerprint is reported for the door's own measurement") {
    var events = Vector.empty[Event]
    val chain = ModelChain(Vector(Lane("ok", _ => "x", fingerprint = "v3")), report = events :+= _)
    chain.answer(ask): Unit
    assertEquals(events, Vector(Event.Answered("ok", "v3", "что сегодня?", "x")))
  }

  test("the free lanes go first, in the operator's order among themselves") {
    assertEquals(ModelChain.prioritise(Vector("openai", "local", "mistral", "ollama"), Set("local", "ollama")),
      Vector("local", "ollama", "openai", "mistral"))
  }
