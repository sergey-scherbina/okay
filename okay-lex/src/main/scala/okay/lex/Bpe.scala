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

  /** merge one word: lowest-ranked adjacent pair first, to fixpoint */
  def encode(word: String): Vector[String] =
    var syms = word.map(_.toString).toVector
    var done = syms.length < 2
    while !done do
      val best = syms.indices.dropRight(1)
        .map(i => (syms(i), syms(i + 1)))
        .filter(ranks.contains)
        .minByOption(ranks)
      best match
        case None => done = true
        case Some(pair) =>
          val merged = Vector.newBuilder[String]
          var i = 0
          while i < syms.length do
            if i < syms.length - 1 && (syms(i), syms(i + 1)) == pair then
              merged += syms(i) + syms(i + 1)
              i += 2
            else
              merged += syms(i)
              i += 1
          syms = merged.result()
          done = syms.length < 2
    syms

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
