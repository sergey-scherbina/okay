package okay2

/** The six laws of the sortable id — the Scala 3 core's okay-data
 * TestUid and TestUidConcurrent. Every law that can run against a
 * CONTROLLED clock does: the hazard is a clock stepping backwards. */
class TestUid extends munit.FunSuite {
  import TestDataClock._

  val ord: Ordering[Uid] = implicitly[Ordering[Uid]]

  test("1. monotonic: successive ids strictly increase, inside one millisecond too") {
    val gen = Uid.at(new Fake(Epoch).source)
    var prev = gen.next()
    var i = 0
    while (i < 5000) {            // more than the 4096 one millisecond holds
      val next = gen.next()
      assert(ord.gt(next, prev), s"id $i did not increase: $prev then $next")
      prev = next
      i += 1
    }
  }

  test("2. sortable as text: lexicographic order of the ULID is the value's order") {
    val clock = new Fake(Epoch)
    val gen = Uid.at(clock.source)
    val ids = (0 until 2000).map { i =>
      if (i % 100 == 0) clock.advance(1)
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
    val clock = new Fake(Epoch)
    val gen = Uid.at(clock.source)
    var prev = gen.next()
    var i = 0
    while (i < 200) {
      if (i % 10 == 0) clock.stepBack(60000) else clock.advance(1)
      val next = gen.next()
      assert(ord.gt(next, prev), s"a backward clock produced a smaller id at $i: $prev then $next")
      prev = next
      i += 1
    }
  }

  test("4. RFC 9562: version 7 and variant 0b10, in the value and in the text") {
    val gen = Uid.at(new Fake(Epoch).source)
    for (_ <- 0 until 100) {
      val u = gen.next()
      assertEquals(((u.hi >>> 12) & 0xFL).toInt, 7, "version nibble")
      assertEquals(((u.lo >>> 62) & 0x3L).toInt, 2, "variant bits")
      val s = u.uuid
      assertEquals(s.length, 36)
      assertEquals(s.charAt(14), '7', s"version character in $s")
      assert("89ab".indexOf(s.charAt(19).toInt) >= 0, s"variant character in $s")
    }
  }

  test("5. round trip: both spellings parse back to the same value, garbage does not") {
    val gen = Uid.at(new Fake(Epoch).source)
    for (_ <- 0 until 500) {
      val u = gen.next()
      assertEquals(Uid.parseUlid(u.ulid), Some(u), s"ulid round trip for ${u.ulid}")
      assertEquals(Uid.parseUuid(u.uuid), Some(u), s"uuid round trip for ${u.uuid}")
      assertEquals(Uid.parseUuid(u.uuid.replace("-", "")), Some(u), "uuid without hyphens")
    }
    assertEquals(Uid.parseUlid(""), None)
    assertEquals(Uid.parseUlid("TOOSHORT"), None)
    assertEquals(Uid.parseUlid("U" * 26), None, "U is not in Crockford's alphabet")
    assertEquals(Uid.parseUlid("8" + "0" * 25), None, "a first character above 7 does not fit in 128 bits")
    assertEquals(Uid.parseUuid("not-a-uuid"), None)
    assertEquals(Uid.parseUuid("z" * 32), None)
  }

  test("5b. Crockford's confusions: I and L read as 1, O reads as 0") {
    val u = Uid.at(new Fake(Epoch).source).next()
    val typed = u.ulid.map(c => if (c == '1') 'l' else if (c == '0') 'O' else c)
    assertEquals(Uid.parseUlid(typed), Some(u), "a human retyping the id must land on the same value")
    assertEquals(Uid.parseUlid(u.ulid.toLowerCase), Some(u), "lower case is the same id")
  }

  test("the millisecond survives the round trip and reads as a time") {
    val u = Uid.at(new Fake(Epoch).source).next()
    assertEquals(u.millis, Epoch)
    assertEquals(Uid.parseUlid(u.ulid).get.millis, Epoch)
    assertEquals(Uid.parseUuid(u.uuid).get.millis, Epoch)
  }

  test("the counter borrows a millisecond rather than repeating or failing") {
    val gen = Uid.at(new Fake(Epoch).source)
    val ids = (0 until 4098).map(_ => gen.next())
    assertEquals(ids.head.millis, Epoch)
    assertEquals(ids(4095).millis, Epoch, "the last id of the millisecond")
    assertEquals(ids(4095).counter, 4095)
    assertEquals(ids(4096).millis, Epoch + 1, "the 4097th borrows the next millisecond")
    assertEquals(ids(4096).counter, 0)
  }
}
