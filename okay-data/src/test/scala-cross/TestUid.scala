package okay

/**
 * The six laws of specs/coordination-free.md stage 1. They are the
 * deliverable — a sortable id whose sortability is not tested is a
 * random id with a story.
 *
 * Every law that can be run against a CONTROLLED clock is, because
 * the hazard this design exists for is a clock that steps backwards
 * and there is no way to wait for one.
 */
class TestUid extends munit.FunSuite {

  /** a clock the test drives: starts at a fixed epoch, moves when told */
  final class Fake(var t: Long):
    def source: () => Long = () => t
    def advance(by: Long): Unit = t += by
    def stepBack(by: Long): Unit = t -= by

  private val Epoch = 1_700_000_000_000L   // 2023-11-14, a real millisecond

  /** `"x" * n` and `s.map` resolve to okay's OWN extensions inside
   * package okay — `String.map` there is the functor's, and answers
   * `Id[Char | String]`. So the string handling here is spelled out. */
  private def rep(c: Char, n: Int): String =
    val b = new StringBuilder
    var i = 0
    while i < n do { b.append(c); i += 1 }
    b.toString

  private def retype(s: String)(f: Char => Char): String =
    val b = new StringBuilder
    var i = 0
    while i < s.length do { b.append(f(s.charAt(i))); i += 1 }
    b.toString

  test("1. monotonic: successive ids strictly increase, inside one millisecond too") {
    val clock = Fake(Epoch)
    val gen = Uid.at(clock.source)
    val ord = summon[Ordering[Uid]]
    var prev = gen.next()
    var i = 0
    while i < 5000 do            // more than the 4096 one millisecond holds
      val next = gen.next()
      assert(ord.gt(next, prev), s"id $i did not increase: $prev then $next")
      prev = next
      i += 1
  }

  test("2. sortable as text: lexicographic order of the ULID is the value's order") {
    val clock = Fake(Epoch)
    val gen = Uid.at(clock.source)
    val ord = summon[Ordering[Uid]]
    val ids = (0 until 2000).map { i =>
      if i % 100 == 0 then clock.advance(1)
      gen.next()
    }.toList
    ids.sliding(2).foreach {
      case a :: b :: Nil =>
        assert(ord.lt(a, b), s"value order broken: $a then $b")
        assert(a.ulid < b.ulid, s"text order disagrees with value order: ${a.ulid} then ${b.ulid}")
        assertEquals(a.ulid.length, 26)
      case _ => ()
    }
  }

  test("3. a clock that steps BACKWARDS does not make an id go back") {
    // the law the whole design exists for: NTP steps, a VM suspends,
    // and System.currentTimeMillis is not monotonic
    val clock = Fake(Epoch)
    val gen = Uid.at(clock.source)
    val ord = summon[Ordering[Uid]]
    var prev = gen.next()
    var i = 0
    while i < 200 do
      if i % 10 == 0 then clock.stepBack(60_000)   // a minute into the past
      else clock.advance(1)
      val next = gen.next()
      assert(ord.gt(next, prev),
        s"a backward clock produced a smaller id at $i: $prev then $next")
      prev = next
      i += 1
  }

  test("4. RFC 9562: version 7 and variant 0b10, in the value and in the text") {
    val gen = Uid.at(Fake(Epoch).source)
    var i = 0
    while i < 100 do
      val u = gen.next()
      assertEquals(((u.hi >>> 12) & 0xFL).toInt, 7, "version nibble")
      assertEquals(((u.lo >>> 62) & 0x3L).toInt, 2, "variant bits")
      val s = u.uuid
      assertEquals(s.length, 36)
      assertEquals(s.charAt(14), '7', s"version character in $s")
        assert("89ab".indexOf(s.charAt(19).toInt) >= 0, s"variant character in $s")
      i += 1
  }

  test("5. round trip: both spellings parse back to the same value, garbage does not") {
    val gen = Uid.at(Fake(Epoch).source)
    var i = 0
    while i < 500 do
      val u = gen.next()
      assertEquals(Uid.parseUlid(u.ulid), Some(u), s"ulid round trip for ${u.ulid}")
      assertEquals(Uid.parseUuid(u.uuid), Some(u), s"uuid round trip for ${u.uuid}")
      assertEquals(Uid.parseUuid(u.uuid.replace("-", "")), Some(u), "uuid without hyphens")
      i += 1
    // and refusals, rather than exceptions
    assertEquals(Uid.parseUlid(""), None)
    assertEquals(Uid.parseUlid("TOOSHORT"), None)
    assertEquals(Uid.parseUlid(rep('U', 26)), None, "U is not in Crockford's alphabet")
    assertEquals(Uid.parseUlid("8" + rep('0', 25)), None,
      "a first character above 7 does not fit in 128 bits")
    assertEquals(Uid.parseUuid("not-a-uuid"), None)
    assertEquals(Uid.parseUuid(rep('z', 32)), None)
  }

  test("5b. Crockford's confusions: I and L read as 1, O reads as 0") {
    val gen = Uid.at(Fake(Epoch).source)
    val u = gen.next()
    val s = u.ulid
    val typed = retype(s) { c => if c == '1' then 'l' else if c == '0' then 'O' else c }
    assertEquals(Uid.parseUlid(typed), Some(u),
      "a human retyping the id must land on the same value")
    assertEquals(Uid.parseUlid(s.toLowerCase), Some(u), "lower case is the same id")
  }

  test("the millisecond survives the round trip and reads as a time") {
    val clock = Fake(Epoch)
    val u = Uid.at(clock.source).next()
    assertEquals(u.millis, Epoch)
    assertEquals(Uid.parseUlid(u.ulid).get.millis, Epoch)
    assertEquals(Uid.parseUuid(u.uuid).get.millis, Epoch)
  }

  test("the counter borrows a millisecond rather than repeating or failing") {
    val clock = Fake(Epoch)
    val gen = Uid.at(clock.source)
    // 4096 fit in one millisecond; the 4097th must move to the next
    val ids = (0 until 4098).map(_ => gen.next())
    assertEquals(ids.head.millis, Epoch)
    assertEquals(ids(4095).millis, Epoch, "the last id of the millisecond")
    assertEquals(ids(4095).counter, 4095)
    assertEquals(ids(4096).millis, Epoch + 1, "the 4097th borrows the next millisecond")
    assertEquals(ids(4096).counter, 0)
  }
}
