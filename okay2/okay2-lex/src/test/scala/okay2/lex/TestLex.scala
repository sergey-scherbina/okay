package okay2.lex

import okay2._
import okay2.Stream.FeedOps
import okay2.stream.Chunks
import okay2.stream.Pipe.into
import Json.K

/** okay-lex's TestLex: the total streaming scanner — lossless, exact
 * spans, incremental, chunked — on okay2 */
class TestLex extends munit.FunSuite {

  val sample = "{\"a\": [1, 2.5e3, true],\n \"b\": null}"

  def chars(s: String, i: Int = 0): Unit ! Writer[Char] =
    if (i >= s.length) pure[Writer[Char], Unit](())
    else Writer.tell(s.charAt(i)).flatMap(_ => chars(s, i + 1))

  test("lossless: the concatenated lexemes of all channels are the input") {
    assertEquals(Scan.all(Json.scan)(sample).tokens.map(_.lexeme).mkString, sample)
    val garbage = "{\"x\": @@ 12 tru}"
    assertEquals(Scan.all(Json.scan)(garbage).tokens.map(_.lexeme).mkString, garbage)
  }

  test("totality: garbage lands on the Error channel, never a fault") {
    val ts = Scan.all(Json.scan)("{\"x\": @@ 12 tru}").tokens
    assertEquals(ts.count(_.channel == Channel.Error), 3)
    assertEquals(ts.filter(_.channel == Channel.Error).map(_.lexeme).toList, List("@", "@", "tru"))
  }

  test("spans are exact across lines") {
    val ts = Scan.all(Json.scan)(sample).tokens
    val b = ts.find(t => t.kind == K.Str && t.lexeme == "\"b\"").get
    assertEquals((b.span.line, b.span.column), (1, 1))
    assertEquals(sample.substring(b.span.offset, b.span.offset + b.span.length), "\"b\"")
  }

  test("flush finishes the tail: an unterminated string is a token") {
    val ts = Scan.all(Json.scan)("{\"oops").tokens
    assertEquals(ts.last.kind, K.Str: K)
    assertEquals(ts.last.channel, Channel.Error: Channel)
    assertEquals(ts.last.lexeme, "\"oops")
  }

  test("the scanner is a Stage: chars stream in, tokens stream out, lazily") {
    val tokens = new FeedOps(into(chars(sample))(Scan.stage(Json.scan))).toLazyList.toList
    assertEquals(tokens.map(_.lexeme).mkString, sample)
    def ones: Unit ! Writer[Char] = Writer.tell('1').flatMap(_ => Writer.tell(',')).flatMap(_ => ones)
    val first = new FeedOps(into(ones)(Scan.stage(Json.scan))).toLazyList.take(2).toList
    assertEquals(first.map(_.kind), List[K](K.Num, K.Comma))
  }

  test("incremental relex: the damage is relexed, the tail is reused") {
    val oldInput = "{\"a\": 111,\n \"b\": [true, false],\n \"c\": \"zzz\"}"
    val newInput = "{\"a\": 12345,\n \"b\": [true, false],\n \"c\": \"zzz\"}"
    class Counting extends Scan[K, Json.S] {
      var steps = 0
      def init: Json.S = Json.scan.init
      def step(s: Json.S, c: Char): (Json.S, Vector[Token[K]]) = { steps += 1; Json.scan.step(s, c) }
      def flush(s: Json.S): Vector[Token[K]] = Json.scan.flush(s)
      override def key(s: Json.S): Any = Json.scan.key(s)
      override def rebase(s: Json.S, d: Int, l: Int): Json.S = Json.scan.rebase(s, d, l)
    }
    val probe = new Counting
    val old = Scan.all(Json.scan)(oldInput, snapshotEvery = 8)
    val relexed = Scan.relex(probe)(old, oldInput, newInput, editStart = 6, editEndOld = 9, editEndNew = 11, snapshotEvery = 8)
    assertEquals(relexed.tokens, Scan.all(Json.scan)(newInput, snapshotEvery = 8).tokens)
    assert(probe.steps < newInput.length / 2, s"reconvergence did not happen: ${probe.steps} of ${newInput.length} steps")
  }

  test("no newline after the edit means a full (still correct) relex") {
    val oldInput = "{\"a\": 1, \"b\": 2}"
    val newInput = "{\"a\": 99, \"b\": 2}"
    val old = Scan.all(Json.scan)(oldInput, snapshotEvery = 4)
    assertEquals(Scan.relex(Json.scan)(old, oldInput, newInput, 6, 7, 8, 4).tokens, Scan.all(Json.scan)(newInput, 4).tokens)
  }

  test("chunked lexing agrees with element-wise; boundary tokens emitted once") {
    val input = "{\"a long string token\": 123456789, \n \"b\": [true, nu ll]}"
    val expected = Scan.all(Json.scan)(input).tokens.toSeq
    for (size <- List(1, 2, 3, 5, 7, 64)) {
      val chunked = Scan.chunks(Json.scan)(Chunks.fromIterator(input.iterator, size))
      assertEquals(Chunks.fold(chunked)(Fold.collect[Token[K]]), expected, s"chunk size $size")
    }
  }

  test("fold answers what the tokens answer, and aggregate says it with an algebra") {
    for (input <- List(sample, "{\"a\": [1, 2, 3]}", "", "   ", "@@ tru")) {
      val all = Scan.all(Json.scan)(input).tokens
      assertEquals(Scan.fold(Json.scan)(input)(0)((n, _) => n + 1), all.length, input)
      assertEquals(Scan.fold(Json.scan)(input)("")((acc, t) => acc + t.lexeme), all.map(_.lexeme).mkString, input)
      assertEquals(Scan.aggregate(Json.scan)(input)(Aggregator.count[Token[K]]), all.length.toLong, input)
    }
  }

  test("the sink road lexes what the pair road lexes") {
    class Delegating extends Scan[K, Json.S] {
      def init: Json.S = Json.scan.init
      def step(s: Json.S, c: Char): (Json.S, Vector[Token[K]]) = Json.scan.step(s, c)
      def flush(s: Json.S): Vector[Token[K]] = Json.scan.flush(s)
    }
    def pairRoad[St](sc: Scan[K, St])(in: String): Vector[Token[K]] = {
      var s = sc.init
      val out = Vector.newBuilder[Token[K]]
      in.foreach { c => val (s2, ts) = sc.step(s, c); out ++= ts; s = s2 }
      out ++= sc.flush(s)
      out.result()
    }
    val inputs = List(sample, "{\"oops", "{\"x\": @@ 12 tru}", "true,null 1e-3", "{\"a\\\"b\": \"c\"}", "", "   ", "{}")
    for (in <- inputs) {
      val expected = pairRoad(Json.scan)(in)
      assertEquals(pairRoad(new Delegating)(in), expected, in)
      assertEquals(Scan.all(Json.scan)(in).tokens, expected, in)
      assertEquals(Scan.all(new Delegating)(in).tokens, expected, in)
      assertEquals(Chunks.fold(Scan.chunks(new Delegating)(Chunks.fromIterator(in.iterator, 4)))(Fold.collect[Token[K]]), expected.toSeq, in)
    }
  }
}
