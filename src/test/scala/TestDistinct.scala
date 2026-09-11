package okay

/** two signatures with nothing but their class to be told apart by */
enum Ping[+A] derives Effect:
  case Pong() extends Ping[Int]

enum Peng[+A] derives Effect:
  case Pung() extends Peng[String]

/**
 * `Distinct[R]` against the rows `TestRowIdentity` already runs.
 *
 * That suite is the specification: every row it shows routing
 * correctly must compile here, and every row it shows misrouting must
 * not. The two suites are the same statement at the two times a row
 * can be wrong — and this one is the time you can still fix it.
 */
class TestDistinct extends munit.FunSuite {

  test("two signatures: different classes, nothing to confuse") {
    summon[Distinct[Reader % Int + Writer % String]]
  }

  test("two READERS misroute, and the compiler now says so") {
    val e = compileErrors("summon[Distinct[Reader % Int + Reader % String]]")
    assert(e.contains("cannot be told apart in one row"), e)
    assert(e.contains("docs/many-instances.md"), e)
  }

  test("two WRITERS are told apart — the test reads the told value") {
    // TestRowIdentity runs this row and both writers collect the
    // right elements; the check must not refuse what works, and it
    // knows only because `writerK` declares TypeableK.ByValue
    summon[Distinct[Writer % String + Writer % Int]]
  }

  test("three members, the collision in the middle") {
    val e = compileErrors(
      "summon[Distinct[Writer % String + Reader % Int + Reader % Long]]")
    assert(e.contains("cannot be told apart in one row"), e)
  }

  test("two KEYS over the same signature: a good row") {
    summon[Distinct[Tag.Of["a", Reader % Int] + Tag.Of["b", Reader % Int]]]
  }

  test("ONE key over one signature, twice: the case Tag cannot fix") {
    // tag-test-the-signature-too made the runtime test key AND
    // signature; this is the half it could not reach, since the
    // signature erases to one class and the key is the same
    val e = compileErrors(
      """summon[Distinct[
           Tag.Of["same", Reader % Int] + Tag.Of["same", Reader % String]]]""")
    assert(e.contains("cannot be told apart in one row"), e)
  }

  test("one key over TWO signatures: the row tag-test-the-signature-too opened") {
    summon[Distinct[Tag.Of["k", Ping] + Tag.Of["k", Peng]]]
  }

  test("an untagged signature does not collide with itself under a key") {
    summon[Distinct[Ping + Tag.Of["k", Ping]]]
  }

  /**
   * A ROW CANNOT REPEAT A MEMBER AT ALL, which is worth pinning
   * because it is the reason the check never has to consider it: `+`
   * is a UNION, `F | F` is `F`, and the compiler collapses it before
   * any macro sees the type. Two `Instances.Of[Ping]` are the one
   * member `Instances` was written to be — which is also the right
   * answer, since one member is one test and there is nothing to
   * misroute.
   */
  test("a repeated member is ONE member: the union collapses") {
    summon[(Instances.Of[Ping] + Instances.Of[Ping])[Int] =:= Instances.Of[Ping][Int]]
    summon[Distinct[Instances.Of[Ping] + Instances.Of[Ping]]]
    summon[Distinct[Reader % Int + Reader % Int]]
  }

  test("Instances over different signatures is a row") {
    summon[Distinct[Instances.Of[Ping] + Instances.Of[Peng]]]
  }

  /**
   * An abstract row is ALLOWED, and this is not a gap in the check —
   * it is what row-generic code is. An interpreter's residual `G` is
   * unknown at the definition site and checked where it is
   * instantiated, which is the only place the answer exists.
   */
  test("an abstract member is allowed: nothing is known, so nothing is refused") {
    def residual[G[+_]](using Distinct[Reader % Int + G]): Int = 1
    assertEquals(residual[Writer % String], 1)
  }

  test("Pure is a member that collides with nothing") {
    summon[Distinct[Ping + Pure]]
    summon[Distinct[Pure + Pure]]
  }
}
