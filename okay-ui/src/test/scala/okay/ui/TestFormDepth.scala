package okay.ui

import okay.codec.{Json, Schema}

/**
 * `Form.editAt`/`render`/`field`/`sumUi`/`listUi`/`errorsOf`/
 * `listErrors` all recurse on a Schema+Json pair's own depth, not a
 * fixed schema shape — for a RECURSIVE schema, that is however deep
 * the actual VALUE is, driven by however long a dotted PATH string an
 * `Event` carries or however many edits a batch folds
 * (form-recursive-depth-safety). Every value/path here is built
 * directly (a loop, never through the real edit flow), so these tests
 * do not depend on `edit`'s own correctness — only on the fixed
 * functions' depth safety.
 *
 * `render`/`errorsOf` are tested at a SMALLER depth than `edit`'s
 * 100 000, deliberately: `key(prefix, name)` rebuilds the whole dotted
 * path as a fresh string at every level, so N nested UI elements each
 * carrying their own O(N)-length key cost O(N²) TOTAL bytes — not a
 * fixable inefficiency (`editAt`'s own `List[Seg]` never rebuilds a
 * string, which is why IT stays safe at 100 000), just the inherent
 * cost of giving every nested element its own addressable dotted key.
 * MEASURED: 100 000 levels of `render` exhausted a stock test JVM's
 * heap (~10 GB of path-string bytes, 1+2+...+100 000 characters) —
 * the stack-safety fix worked exactly as intended (no
 * StackOverflowError at all, the failure moved to OutOfMemoryError,
 * per encode-side-depth-safety's own stated tradeoff) but a test at
 * that depth proves nothing else useful past this size. 5 000 is
 * comfortably past `Codecs.NativeThreshold` (24) while staying
 * light (~12 MB of path-string bytes).
 */
class TestFormDepth extends munit.FunSuite:

  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  enum Chain derives Schema:
    case Leaf(label: String)
    case Node(next: Chain)

  def deepChain(n: Int): Chain =
    var c: Chain = Chain.Leaf("bottom")
    var i = 0
    while i < n do { c = Chain.Node(c); i += 1 }
    c

  def depthOf(c: Chain): Int =
    var d = 0
    var at = c
    var go = true
    while go do at match
      case Chain.Node(next) => d += 1; at = next
      case Chain.Leaf(_) => go = false
    d

  test("Form.edit follows a genuinely long dotted path — no cap, no stack overflow") {
    val n = 100000
    val value = Json.parse(Json.write(deepChain(n)))
    val path = ("next." * n) + "label"
    val edited = Form.edit[Chain](value, Event.Edited(path, "hit"))
    Json.decode(summon[Schema[Chain]])(edited) match
      case Left(e) => fail(s"expected a value, got: $e")
      case Right(c) =>
        assertEquals(depthOf(c), n)
        var at = c
        var go = true
        while go do at match
          case Chain.Node(next) => at = next
          case Chain.Leaf(label) => assertEquals(label, "hit"); go = false
  }

  test("Form.render on a genuinely deep value — no cap, no stack overflow") {
    val n = 5000
    val value = Json.parse(Json.write(deepChain(n)))
    val ui = Form.render(summon[Schema[Chain]], value, Vector.empty, "")
    // the point is that building it did not overflow; a deep Ui tree
    // is its own StackOverflowError risk to INSPECT (the same
    // assertEquals-on-a-deep-tree trap the codec tests avoid), so this
    // only checks the shape at the top, not the whole tree
    assert(ui.isInstanceOf[Ui.Column], ui.getClass.getSimpleName)
  }

  test("Form.errors on a genuinely deep, well-formed value — no cap, no stack overflow") {
    val n = 5000
    val value = Json.parse(Json.write(deepChain(n)))
    assertEquals(Form.errors[Chain](value), Vector.empty)
  }

  test("ordinary shapes below the threshold are untouched: edit, render, errors") {
    final case class Address(city: String, zip: Int)
    final case class Person(name: String, age: Int, address: Address, tags: Vector[String])
    given Schema[Address] = Schema.derived
    given Schema[Person] = Schema.derived

    val p = Person("ada", 36, Address("Warszawa", 12345), Vector("a", "b"))
    val value = Json.parse(Json.write(p))
    assertEquals(Form.errors[Person](value), Vector.empty)

    val edited = Form.edit[Person](value, Event.Edited("address.city", "Kraków"))
    assertEquals(Json.decode(summon[Schema[Person]])(edited), Right(p.copy(address = p.address.copy(city = "Kraków"))))

    val ui = Form.render(summon[Schema[Person]], value, Vector.empty, "")
    assert(ui.isInstanceOf[Ui.Column])
  }
