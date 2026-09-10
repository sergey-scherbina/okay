package okay.codec

/**
 * The depth of a message is the SENDER's number, on both wires
 * (input-depth-both-wires).
 *
 * `Codecs.maxDepth` was justified like this: every other read
 * recurses on the depth of the SCHEMA, which the program wrote, while
 * a skip recurses on the depth of the INPUT, which the sender wrote.
 * The first half was true of Cbor.scala and false of the other wire.
 * MEASURED 2026-09-10 on a default JVM stack, before this suite
 * existed: `JsonValue.parse` threw a `StackOverflowError` at 20 000
 * nested arrays and the lossless projection between 1 000 and 5 000 —
 * where the fast road's own doc says "never a throw" and `Json.parse`
 * promises a value. A hostile document needs no cleverness to reach
 * it: `"[" * 20000`.
 *
 * So depth is bounded on both wires now, and each road refuses in the
 * idiom it already had for every other kind of damage — the fast road
 * is NOT SURE, the lossless road makes the cut a `JErr`, the CBOR
 * decoder returns a `Left` naming the limit.
 */
class TestInputDepth extends munit.FunSuite:

  /** `n` nested arrays around a number */
  def nest(n: Int): String = ("[" * n) + "1" + ("]" * n)

  /** how many arrays deep the value goes, and what is at the bottom */
  def bottom(j: Json): (Int, Json) =
    var d = 0
    var at = j
    var done = false
    while !done do
      at match
        case Json.JArr(Vector(one)) => d += 1; at = one
        case _ => done = true
    (d, at)

  test("past the limit: a value, not a stack overflow — and the document says why") {
    // each road is given the depth that used to KILL IT, and no more:
    // the fast road died at 20 000 and refuses there in constant time,
    // while the projection died between 1 000 and 5 000 and every
    // document it is handed here costs a whole CST
    assertEquals(JsonValue.parse(nest(20000)), None, "the fast road is not sure about damage")
    val deep = nest(5000)
    val v = Json.parse(deep)                      // must not throw
    assertEquals(v, Json.lossless(deep), "the two roads answer one value, as they do for all damage")
    assert(Json.isCut(v), s"the document IS the cut, not a tree with one in it: $v")
    v match
      case Json.JErr(m) => assert(m.contains(s"nested deeper than ${Codecs.maxDepth}"), m)
      case other => fail(s"expected the cut, got $other")
  }

  test("the limit is exact, and counts every container — objects as well as arrays") {
    val ok = nest(Codecs.maxDepth)
    assert(JsonValue.parse(ok).isDefined, "a document at the limit is not damage")
    assertEquals(Json.parse(ok), Json.lossless(ok))
    assertEquals(bottom(Json.parse(ok)), (Codecs.maxDepth, Json.JNum(1.0)),
      "every one of the containers at the limit is a value, and the number is under them")
    assert(Json.isCut(Json.parse(nest(Codecs.maxDepth + 1))), "one more is the cut")

    def objs(d: Int) = ("{\"a\":" * d) + "1" + ("}" * d)
    assertEquals(Json.parse(objs(Codecs.maxDepth)) match
      case Json.JObj(_) => true
      case _ => false, true, "objects at the limit read as objects")
    assert(Json.isCut(Json.parse(objs(Codecs.maxDepth + 1))), "and one more is the cut")
  }

  test("a DECLARED field past the limit is a refusal on both wires") {
    // what matters is that the cut lands in a position the schema
    // DECLARES, whatever shape that position has
    final case class Known(a: String, deep: List[String])
    given Schema[Known] = Schema.derived
    val text = "{\"a\":\"kept\",\"deep\":" + nest(500) + "}"
    assert(Json.decode(summon[Schema[Known]])(Json.parse(text)).isLeft,
      "a cut at a position the schema declares cannot decode")
    assert(Json.readStrict[Known](text).isLeft, "the strict door refuses it too")
  }

  test("an UNDECLARED field past the limit refuses too: depth is the DOCUMENT's") {
    final case class OnlyA(a: String)
    given Schema[OnlyA] = Schema.derived
    val text = "{\"a\":\"kept\",\"deep\":" + nest(500) + "}"
    // This is where this lane came from. input-depth-both-wires left
    // the JSON roads reading such a document (a cut inside a field
    // nobody declares is data nobody reads) and called the difference
    // structural — the totality of the door. But a RECURSIVE schema
    // really can write that value, so "one Schema, one value, one
    // answer on either wire" really was broken by it. A cut is not
    // damage at a SPOT: the reader cannot say what it did not descend
    // into, so it is the whole document that is unreadable, and the
    // projection says so at the root.
    assert(Json.isCut(Json.parse(text)), s"the document is the cut, not a tree with one in it")
    def named(e: Either[String, OnlyA], who: String): Unit = e match
      case Left(m) => assert(m.contains("nested deeper than"), s"$who said: $m")
      case Right(v) => fail(s"$who answered $v for a document it could not read to the bottom")
    named(Json.decode(summon[Schema[OnlyA]])(Json.parse(text)), "Json.decode")
    named(Json.read[OnlyA](text), "Json.read")
    named(Json.readStrict[OnlyA](text), "Json.readStrict")
    named(Staged.strict[OnlyA].decode(text), "Staged.strict")
    val out = new Cbor.Out
    out.mapHeader(2)
    out.text("a"); out.text("kept")
    out.text("deep")
    for _ <- 0 to Codecs.maxDepth + 1 do out.arrayHeader(1)
    out.integer(1)
    named(Cbor.read[OnlyA](out.toArray), "Cbor.read")
  }

  test("a cut is not a damaged element: no list silently gets shorter") {
    // the defect this rule replaces, kept as its test: with the cut
    // left in place, `decode`'s damaged-element rule dropped it and a
    // 256-level tree came back as a 128-level one with Right
    final case class Box(xs: List[String])
    given Schema[Box] = Schema.derived
    val text = "{\"xs\":[\"one\"," + nest(400) + "]}"
    Json.read[Box](text) match
      case Left(m) => assert(m.contains("nested deeper than"), m)
      case Right(b) => fail(s"the list came back as ${b.xs}")
  }

  // ── the depth a RECURSIVE schema's own value reaches ─────────────
  //
  // This is where the limit stops being the skip's business. A
  // recursive schema lets the SENDER choose the depth of a DECLARED
  // value, and every door recursed on it: MEASURED 2026-09-10 under
  // sbt's -Xss8m, `Cbor.read` threw a StackOverflowError at a
  // 5 000-level tree and the strict JSON door at 20 000, while the
  // lossless road — once its parse was bounded and nothing else —
  // answered Right to a 1 000-level tree by silently SKIPPING the
  // cut as if it were a damaged element. A wrong value with no error
  // is worse than either throw.

  final case class Tree(kids: Vector[Tree])
  given Schema[Tree] = Schema.derived

  /** `{kids:[{kids:[...{kids:[]}]}]}` — `d` levels, and each level is
    * TWO containers on both wires (a map/object and an array), which
    * is why the two wires refuse at the same tree depth */
  def jsonTree(d: Int): String = ("{\"kids\":[" * d) + "{\"kids\":[]}" + ("]}" * d)

  /** the same tree as CBOR, written by hand so that no encoder's own
    * recursion is what the test measures */
  def cborTree(d: Int): Array[Byte] =
    val out = new Cbor.Out
    for _ <- 0 until d do
      out.mapHeader(1); out.text("kids"); out.arrayHeader(1)
    out.mapHeader(1); out.text("kids"); out.arrayHeader(0)
    out.toArray

  test("a recursive value AT the limit reads on every door, and to the same value") {
    val d = Codecs.maxDepth / 2 - 1            // two containers per level
    val fromCbor = Cbor.read[Tree](cborTree(d))
    val fromJson = Json.read[Tree](jsonTree(d))
    val fromStrict = Json.readStrict[Tree](jsonTree(d))
    assert(fromCbor.isRight, fromCbor.toString)
    assertEquals(fromJson, fromCbor, "the two wires read one value")
    assertEquals(fromStrict, fromCbor, "the strict door reads the same value")
    assertEquals(fromCbor.map(depthOf), Right(d + 1))
  }

  test("a recursive value PAST the limit is refused by every door — never a shorter tree") {
    val d = Codecs.maxDepth                    // twice past it
    def named(e: Either[String, Tree], who: String): Unit = e match
      case Left(m) => assert(m.contains("nested deeper than"), s"$who said: $m")
      case Right(t) => fail(s"$who answered a ${depthOf(t)}-level tree for a $d-level document")
    named(Cbor.read[Tree](cborTree(d)), "Cbor.read")
    named(Json.read[Tree](jsonTree(d)), "Json.read")
    named(Json.readStrict[Tree](jsonTree(d)), "Json.readStrict")
    // and the generated decoders answer what the folds answer — the
    // cut is read through ONE place (Json.isCut, Cbor.In.enter), which
    // is what keeps the three of them alike
    named(Staged.cbor[Tree].decode(cborTree(d)), "Staged.cbor")
    named(Staged.json[Tree].decode(Json.parse(jsonTree(d))), "Staged.json")
    named(Staged.strict[Tree].decode(jsonTree(d)), "Staged.strict")
  }

  def depthOf(t: Tree): Int =
    var d = 1
    var at = t
    while at.kids.nonEmpty do { d += 1; at = at.kids.head }
    d
