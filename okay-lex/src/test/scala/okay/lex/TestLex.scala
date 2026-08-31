package okay.lex

import okay.{!, %, Chunks, Writer, through, pure}
import okay.toLazyList
import Json.K

/** The total streaming scanner: lossless, exact spans, incremental. */
class TestLex extends munit.FunSuite {

  val sample = "{\"a\": [1, 2.5e3, true],\n \"b\": null}"

  test("lossless: the concatenated lexemes of all channels are the input") {
    assertEquals(Scan.all(Json.scan)(sample).tokens.map(_.lexeme).mkString, sample)
    val garbage = "{\"x\": @@ 12 tru}"
    assertEquals(Scan.all(Json.scan)(garbage).tokens.map(_.lexeme).mkString, garbage)
  }

  test("totality: garbage lands on the Error channel, never a fault") {
    val ts = Scan.all(Json.scan)("{\"x\": @@ 12 tru}").tokens
    assertEquals(ts.count(_.channel == Channel.Error), 3)   // two @, one 'tru'
    assertEquals(ts.filter(_.channel == Channel.Error).map(_.lexeme).toList,
      List("@", "@", "tru"))
  }

  test("spans are exact across lines") {
    val ts = Scan.all(Json.scan)(sample).tokens
    val b = ts.find(t => t.kind == K.Str && t.lexeme == "\"b\"").get
    assertEquals((b.span.line, b.span.column), (1, 1))
    assertEquals(sample.substring(b.span.offset, b.span.offset + b.span.length), "\"b\"")
  }

  test("flush finishes the tail: an unterminated string is a token") {
    val ts = Scan.all(Json.scan)("{\"oops").tokens
    assertEquals(ts.last.kind, K.Str)
    assertEquals(ts.last.channel, Channel.Error)
    assertEquals(ts.last.lexeme, "\"oops")
  }

  test("the scanner is a Stage: chars stream in, tokens stream out, lazily") {
    def chars(s: String, i: Int = 0): Unit ! Writer % Char =
      if i >= s.length then pure(())
      else Writer.tell(s.charAt(i)).flatMap(_ => chars(s, i + 1))
    val tokens = through(chars(sample))(Scan.stage(Json.scan)).toLazyList.toList
    assertEquals(tokens.map(_.lexeme).mkString, sample)
    // laziness: two tokens from an endless character source
    def ones: Unit ! Writer % Char =
      Writer.tell('1').flatMap(_ => Writer.tell(',')).flatMap(_ => ones)
    val first = through(ones)(Scan.stage(Json.scan)).toLazyList.take(2).toList
    assertEquals(first.map(_.kind), List(K.Num, K.Comma))
  }

  test("incremental relex: the damage is relexed, the tail is reused") {
    val oldInput = "{\"a\": 111,\n \"b\": [true, false],\n \"c\": \"zzz\"}"
    val newInput = "{\"a\": 12345,\n \"b\": [true, false],\n \"c\": \"zzz\"}"
    class Counting extends Scan[K, Json.S]:
      var steps = 0
      def init = Json.scan.init
      def step(s: Json.S, c: Char) = { steps += 1; Json.scan.step(s, c) }
      def flush(s: Json.S) = Json.scan.flush(s)
      override def key(s: Json.S) = Json.scan.key(s)
      override def rebase(s: Json.S, d: Int, l: Int) = Json.scan.rebase(s, d, l)
    val probe = Counting()
    val old = Scan.all(Json.scan)(oldInput, snapshotEvery = 8)
    val relexed = Scan.relex(probe)(old, oldInput, newInput,
      editStart = 6, editEndOld = 9, editEndNew = 11, snapshotEvery = 8)
    val full = Scan.all(Json.scan)(newInput, snapshotEvery = 8)
    assertEquals(relexed.tokens, full.tokens)
    assert(probe.steps < newInput.length / 2,
      s"reconvergence did not happen: ${probe.steps} of ${newInput.length} steps")
  }

  test("no newline after the edit means a full (still correct) relex") {
    val oldInput = "{\"a\": 1, \"b\": 2}"
    val newInput = "{\"a\": 99, \"b\": 2}"
    val old = Scan.all(Json.scan)(oldInput, snapshotEvery = 4)
    val relexed = Scan.relex(Json.scan)(old, oldInput, newInput, 6, 7, 8, 4)
    assertEquals(relexed.tokens, Scan.all(Json.scan)(newInput, 4).tokens)
  }

  test("chunked lexing agrees with element-wise; boundary tokens emitted once") {
    // long tokens guarantee chunk-boundary crossings at small sizes;
    // some garbage keeps the Error channel in play
    val input = "{\"a long string token\": 123456789, \n \"b\": [true, nu ll]}"
    val expected = Scan.all(Json.scan)(input).tokens.toSeq
    for size <- List(1, 2, 3, 5, 7, 64) do
      val chunked = Scan.chunks(Json.scan)(Chunks.fromIterator(input.iterator, size))
      assertEquals(Chunks.fold(chunked), expected, s"chunk size $size")
  }
}
