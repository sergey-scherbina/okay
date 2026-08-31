package okay.parse

import okay.{!, %, Writer, through, pure}
import okay.toLazyList
import okay.lex.{Scan, Token}
import okay.lex.Json as JsonLex
import okay.lex.Json.K

/** Total parsing: any input yields a tree, errors are data in it. */
class TestParse extends munit.FunSuite {

  def chars(s: String, i: Int = 0): Unit ! Writer % Char =
    if i >= s.length then pure(())
    else Writer.tell(s.charAt(i)).flatMap(_ => chars(s, i + 1))

  /** the whole pipeline: chars -> scanner stage -> parser stage -> CST */
  def parse(s: String, d: Parse.Driver[K, ?] = JsonParse.driver): Cst[K] =
    Parse.toCst(through(through(chars(s))(Scan.stage(JsonLex.scan)))(d).toLazyList)

  val sample = "{\"a\": [1, 2.5e3, true],\n \"b\": null}"

  test("lossless: the CST reproduces the input, trivia included") {
    assertEquals(Cst.lexemes(parse(sample)), sample)
  }

  test("totality: a truncated stream is a tree with holes (the LLM case)") {
    val t = parse("{\"a\": [1, 2")
    assertEquals(Cst.lexemes(t), "{\"a\": [1, 2")
    val errs = Cst.errors(t)
    assertEquals(errs.map(_._2), Vector("unclosed", "unclosed"))
  }

  test("sibling recovery: one bad token damages one leaf, not the tree") {
    val t = parse("[1, @, 3]")
    assertEquals(Cst.lexemes(t), "[1, @, 3]")
    val errs = Cst.errors(t)
    assertEquals(errs.map(_._2), Vector("unexpected '@'"))
    // the siblings survived
    def leaves(c: Cst[K]): Vector[String] = c match
      case Cst.Node(_, cs) => cs.flatMap(leaves)
      case Cst.Leaf(tok) if tok.kind == K.Num => Vector(tok.lexeme)
      case _ => Vector.empty
    assertEquals(leaves(t), Vector("1", "3"))
  }

  test("a close with nothing open is an error leaf, not a fault") {
    val t = parse("}]")
    assertEquals(Cst.lexemes(t), "}]")
    assertEquals(Cst.errors(t).map(_._2),
      Vector("nothing to close", "nothing to close"))
  }

  test("the two surfaces produce the SAME tree (the convergence contract)") {
    for s <- List(sample, "{\"a\": [1, 2", "[1, @, 3]", "}]", "") do
      assertEquals(parse(s, JsonParse.combinators), parse(s, JsonParse.driver))
  }

  test("streaming: a prefix parses to a prefix-consistent tree") {
    val full = parse(sample)
    val half = parse(sample.take(sample.length / 2))
    // the half tree is lossless over the half input
    assertEquals(Cst.lexemes(half), sample.take(sample.length / 2))
    // and its non-error frontier is a prefix of the full one's tokens
    def toks(c: Cst[K]): Vector[String] = c match
      case Cst.Node(_, cs) => cs.flatMap(toks)
      case Cst.Leaf(t) => Vector(t.lexeme)
      case Cst.Err(t, _) => t.map(_.lexeme).toVector
    assert(toks(full).mkString.startsWith(toks(half).mkString.dropRight(1)))
  }

  val doc =
    "{\"alpha\": [1, 2, 3],\n \"beta\": 123,\n \"gamma\": {\"x\": true},\n \"delta\": [4, 5]}"

  /** the nested object node (gamma's value) inside the root object */
  def gammaNode(t: Cst[K]): Cst[K] =
    val Cst.Node(_, rootKids) = t: @unchecked
    val Some(obj @ Cst.Node("object", kids)) = rootKids.headOption: @unchecked
    kids.collectFirst { case n @ Cst.Node("object", _) => n }.get

  test("incremental reparse: a length-preserving edit reuses subtrees by reference") {
    val old = Parse.full(JsonLex.scan, JsonParse.instrs)(doc, snapshotEvery = 8)
    val edited = doc.replace("123", "987")
    assertEquals(edited.length, doc.length)
    var steps = 0
    val probe: Token[K] => Vector[Instr[K]] = t => { steps += 1; JsonParse.instrs(t) }
    val at = doc.indexOf("123")
    val re = Parse.reparse(JsonLex.scan, probe)(old, doc, edited, at, at + 3, at + 3, 8)
    assertEquals(re.tree, Parse.full(JsonLex.scan, JsonParse.instrs)(edited).tree)
    assert(steps < old.lexed.tokens.length / 2,
      s"not incremental: $steps of ${old.lexed.tokens.length} tokens re-driven")
    assert(gammaNode(re.tree) eq gammaNode(old.tree),
      "the untouched subtree must be the SAME object, not a rebuild")
  }

  test("incremental reparse: a length-changing edit still equals the full parse") {
    val old = Parse.full(JsonLex.scan, JsonParse.instrs)(doc, snapshotEvery = 8)
    val edited = doc.replace("123", "1")   // two chars shorter
    val at = doc.indexOf("123")
    val re = Parse.reparse(JsonLex.scan, JsonParse.instrs)(old, doc, edited, at, at + 3, at + 1, 8)
    assertEquals(re.tree, Parse.full(JsonLex.scan, JsonParse.instrs)(edited).tree)
    assertEquals(Cst.lexemes(re.tree), edited)   // spans rebased, content exact
  }
}
