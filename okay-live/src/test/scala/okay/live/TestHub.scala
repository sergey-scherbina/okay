package okay.live

import okay.*
import okay.given

/** specs/live.md — broadcast: every current subscriber gets every publish. */
class TestHub extends munit.FunSuite {

  test("two subscribers both receive a published value") {
    val hub = Hub[String]()
    val a = hub.subscribe()
    val b = hub.subscribe()
    hub.publish("hi")
    assertEquals(a.receiveBlocking(), Some("hi"))
    assertEquals(b.receiveBlocking(), Some("hi"))
  }

  test("a subscriber added AFTER a publish does not see it") {
    val hub = Hub[String]()
    val early = hub.subscribe()
    hub.publish("before")
    val late = hub.subscribe()
    hub.publish("after")
    assertEquals(early.receiveBlocking(), Some("before"))
    assertEquals(early.receiveBlocking(), Some("after"))
    assertEquals(late.receiveBlocking(), Some("after"))
  }

  test("independent hubs do not cross-talk") {
    val h1 = Hub[String](); val h2 = Hub[String]()
    val a = h1.subscribe(); val b = h2.subscribe()
    h1.publish("only h1")
    assertEquals(a.receiveBlocking(), Some("only h1"))
    a.offer("sentinel-a"): Unit
    // b's channel never got anything from h1's publish — proven by
    // b now receiving ITS OWN sentinel first, not h1's "only h1"
    b.offer("sentinel-b"): Unit
    assertEquals(b.receiveBlocking(), Some("sentinel-b"))
  }
}
