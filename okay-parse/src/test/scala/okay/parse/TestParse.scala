package okay.parse

import okay.{!, %, Writer, through, pure}
import okay.given
import okay.toLazyList
import okay.lex.{Channel, Scan, Token}
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
}
