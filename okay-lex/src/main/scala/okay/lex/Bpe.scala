package okay.lex



/**
 * Byte-pair encoding as a Scan (specs/llm.md): the LLM world's
 * tokenizer implements the SAME streaming interface as every lexer in
 * okay-lex — incremental, span-exact, snapshot-friendly. Words (runs
 * of non-whitespace) BPE-merge independently, exactly like the
 * reference pre-tokenizer discipline, so the scan emits a word's
 * tokens the moment the word ends and never revisits it. Whitespace
 * comes out on the Trivia channel — the stream stays lossless.
 *
 * The dictionary is the classic merges table: a pair of symbols to
 * its rank, lowest rank merged first.
 */
final case class Bpe(ranks: Map[(String, String), Int]) extends Scan[String, Bpe.S] {

  import Bpe.S

  def init: S = S("", Bpe.P(0, 0, 0), Bpe.P(0, 0, 0))

  override def key(s: S): Any = s.buf

  override def rebase(s: S, offsetDelta: Int, lineDelta: Int): S =
    def shift(p: Bpe.P) = Bpe.P(p.off + offsetDelta, p.line + lineDelta, p.col)
    S(s.buf, shift(s.start), shift(s.at))

  /**
   * Merge one word: lowest-ranked adjacent pair first, to fixpoint.
   *
   * Still the quadratic scan the shape of BPE asks for — up to one
   * pass per merge, and a merge can shorten the word by one — but with
   * the constant taken out of it. The previous version built a Vector
   * of every adjacent pair, filtered it into a second Vector, and then
   * called `minByOption(ranks)`, so each pair cost TWO map lookups
   * (once for `contains`, once for the comparison) and two tuple
   * allocations, on every pass. This finds the same minimum in one
   * pass with one `get` per pair and nothing allocated.
   *
   * The symbols live in an `Array[String]` for the same reason: the
   * merge rebuilt a `Vector` through a builder each time round.
   *
   * A heap keyed by rank with positions tracked would make it linear,
   * and is not worth it here: words are short, so k is around ten and
   * the constant is the whole cost.
   */
  def encode(word: String): Vector[String] =
    if word.length < 2 then
      if word.isEmpty then Vector.empty else Vector(word)
    else
      val syms = new Array[String](word.length)
      var k = 0
      while k < word.length do
        syms(k) = word.charAt(k).toString
        k += 1
      var n = word.length
      var done = false
      while !done do
        // one pass, one lookup per pair, no intermediate collections
        var bestRank = Int.MaxValue
        var bestI = -1
        var i = 0
        while i < n - 1 do
          ranks.get((syms(i), syms(i + 1))) match
            case Some(r) if r < bestRank => bestRank = r; bestI = i
            case _ => ()
          i += 1
        if bestI < 0 then done = true
        else
          val a = syms(bestI); val b = syms(bestI + 1)
          var w = 0
          var j = 0
          while j < n do
            if j < n - 1 && syms(j) == a && syms(j + 1) == b then
              syms(w) = a + b; w += 1; j += 2
            else
              syms(w) = syms(j); w += 1; j += 1
          n = w
          done = n < 2
      Vector.tabulate(n)(syms)

  private def word(s: S): Vector[Token[String]] =
    if s.buf.isEmpty then Vector.empty
    else
      var off = s.start.off
      var col = s.start.col
      encode(s.buf).map { sym =>
        val t = Token(sym, sym, Span(off, s.start.line, col, sym.length))
        off += sym.length
        col += sym.length
        t
      }

  def step(s: S, c: Char): (S, Vector[Token[String]]) =
    val next = s.at + c
    if c.isWhitespace then
      val ws = Token(c.toString, c.toString,
        Span(s.at.off, s.at.line, s.at.col, 1), Channel.Trivia)
      (S("", next, next), word(s) :+ ws)
    else if s.buf.isEmpty then (S(c.toString, s.at, next), Vector.empty)
    else (s.copy(buf = s.buf + c, at = next), Vector.empty)

  def flush(s: S): Vector[Token[String]] = word(s)
}

object Bpe:
  final case class P(off: Int, line: Int, col: Int):
    def +(c: Char): P =
      if c == '\n' then P(off + 1, line + 1, 0) else P(off + 1, line, col + 1)

  final case class S(buf: String, start: P, at: P)

  /** ranks from an ordered merge list (first merge = rank 0) */
  def apply(merges: Seq[(String, String)]): Bpe =
    Bpe(merges.zipWithIndex.toMap)
