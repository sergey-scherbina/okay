package okay.kernel

class TestVersion extends munit.FunSuite:

  test("parse reads 1, 1.2, 1.2.3 and a pre-release; refuses anything else by name") {
    assertEquals(Version.parse("1"), Right(Version(1, 0, 0)))
    assertEquals(Version.parse("1.2"), Right(Version(1, 2, 0)))
    assertEquals(Version.parse(" 1.2.3 "), Right(Version(1, 2, 3)))
    assertEquals(Version.parse("1.2.3-rc.1"), Right(Version(1, 2, 3, "rc.1")))
    for bad <- Seq("", "v1", "1.x", "1.2.3.4", "-1") do
      val e = Version.parse(bad)
      assert(e.isLeft && e.left.exists(_.contains(s"'$bad'")), s"$bad -> $e")
  }

  test("order: numeric by part, a pre-release below its release") {
    val vs = Vector("1.10.0", "1.2.0", "1.2.0-rc.1", "0.9.9", "1.2.0-beta", "2.0.0").map(Version(_))
    assertEquals(vs.sorted.map(_.toString),
      Vector("0.9.9", "1.2.0-beta", "1.2.0-rc.1", "1.2.0", "1.10.0", "2.0.0"))
  }

  test("caret: same major, at least the floor; on 0.x same minor") {
    val r = Range("^1.2")
    assert(r.accepts(Version("1.2.0")) && r.accepts(Version("1.9.3")))
    assert(!r.accepts(Version("1.1.9")) && !r.accepts(Version("2.0.0")))
    val z = Range("^0.3")
    assert(z.accepts(Version("0.3.9")))
    assert(!z.accepts(Version("0.4.0")))
  }

  test("exact, between and any") {
    assert(Range("=1.2.3").accepts(Version("1.2.3")) && !Range("=1.2.3").accepts(Version("1.2.4")))
    val b = Range(">=1.2 <2")
    assert(b.accepts(Version("1.2.0")) && b.accepts(Version("1.99.0")) && !b.accepts(Version("2.0.0")))
    assert(Range("*").accepts(Version("9.9.9")))
    assert(Range.parse("~1.2").isLeft)
  }
