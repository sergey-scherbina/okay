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

  test("fold answers what the tokens answer, and aggregate says it with an algebra") {
    // the law: folding as the tokens are produced is folding the
    // tokens. Asserted on the driver that DOES materialise, so a
    // divergence in either shows here.
    for input <- List(sample, "{\"a\": [1, 2, 3]}", "", "   ", "@@ tru") do
      val all = Scan.all(Json.scan)(input).tokens
      assertEquals(Scan.fold(Json.scan)(input)(0)((n, _) => n + 1), all.length, input)
      assertEquals(Scan.fold(Json.scan)(input)("")((acc, t) => acc + t.lexeme),
        all.map(_.lexeme).mkString, input)
      assertEquals(Scan.fold(Json.scan)(input)(0)((n, t) =>
        if t.channel == Channel.Syntax then n + 1 else n),
        all.count(_.channel == Channel.Syntax), input)
      // and the same through an Aggregator, which is where the
      // aggregation algebra meets lexing
      assertEquals(Scan.aggregate(Json.scan)(input)(okay.Aggregator.count[Token[K]]),
        all.length.toLong, input)
  }

  test("the sink road lexes what the pair road lexes") {
    // `stepInto` is ADDITIVE: the drivers read it, `step` stays, and
    // the two must not drift. `Delegating` implements only `step`, so
    // it runs on the trait's default — which is the compatibility
    // promise every scanner outside okay-lex is relying on (Yaml,
    // Markdown, Xml, okay-rag's code scanner). Json overrides it.
    class Delegating extends Scan[K, Json.S]:
      def init = Json.scan.init
      def step(s: Json.S, c: Char) = Json.scan.step(s, c)
      def flush(s: Json.S) = Json.scan.flush(s)

    // `St`, not `S`: a type parameter named S here shadows Json.S
    def pairRoad[St](sc: Scan[K, St])(in: String): Vector[Token[K]] =
      var s = sc.init
      val out = Vector.newBuilder[Token[K]]
      in.foreach { c =>
        val (s2, ts) = sc.step(s, c)
        out ++= ts
        s = s2
      }
      out ++= sc.flush(s)
      out.result()

    // an unterminated string (flush), garbage (the Error channel), an
    // escaped quote (the in-string arm), and a word ending AT a
    // structural character — the one arm that emits two tokens for
    // one character, and the one whose order the lossless law reads
    val inputs = List(sample, "{\"oops", "{\"x\": @@ 12 tru}", "true,null 1e-3",
                      "{\"a\\\"b\": \"c\"}", "", "   ", "{}")
    for in <- inputs do
      val expected = pairRoad(Json.scan)(in)
      assertEquals(pairRoad(new Delegating)(in), expected, in)
      assertEquals(Scan.all(Json.scan)(in).tokens, expected, in)       // sink, overridden
      assertEquals(Scan.all(new Delegating)(in).tokens, expected, in)  // sink, the default
      assertEquals(Chunks.fold(Scan.chunks(new Delegating)(
        Chunks.fromIterator(in.iterator, 4))), expected.toSeq, in)
  }
}
