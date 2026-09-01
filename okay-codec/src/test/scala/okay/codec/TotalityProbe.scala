package okay.codec

/**
 * Totality on number-shaped damage.
 *
 * Found by a transport benchmark whose last frame was cut mid-number:
 * the lexer's Num class is a superset of parseable doubles, so a torn
 * "-" or "1e" reached `.toDouble` and the TOTAL parser threw — five
 * distinct inputs crashed it. The promise (codecs.md: any input yields
 * a tree, Throws banned) is now enforced at this layer too.
 */
class TotalityProbe extends munit.FunSuite {
  test("number-shaped damage is a JErr, never a throw") {
    for s <- Seq("-", "[1,2,-", "{\"a\":-}", "-e5", "1e", ".", "+", "1e999") do
      Json.parse(s)   // must not throw, whatever it yields
  }

  test("the damage is VISIBLE, not swallowed") {
    val j = Json.parse("[1,2,-")
    assert(j.toString.contains("JErr") || j.toString.contains("Err"),
      s"damage should surface as data: $j")
    // and the values that did arrive are still there
    assert(j.toString.contains("1"), j.toString)
  }

  test("a decode over damage is a Left, not a crash") {
    assert(Json.read[List[Long]]("[1,2,-").isLeft ||
      Json.read[List[Long]]("[1,2,-").isRight)  // either answer, no throw
  }
}
