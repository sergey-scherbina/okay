package okay.codec

import okay.lex.{Scan, Span, Token}
import okay.parse.{Cst, Instr, Parse}

/**
 * The Markdown dialect — the REFRAMING prover of specs/codecs.md.
 * Markdown emphasis does not nest: `*a _b* c_` closes the star while
 * the underscore is still open. The uniml answer (adoption-agency in
 * miniature): close the crossing inner frames without a token, close
 * the target with its token, REOPEN the inner frames — the tree
 * stays well-nested, every marker token is kept (lossless), nothing
 * ever faults; whatever stays open at the end is the builder's
 * "unclosed" error node, an error AS DATA.
 *
 * Deliberately small: headings (#... to end of line), paragraphs
 * (one line), emphasis * and _, code spans ` (no nesting inside).
 */
object Markdown {

  enum K:
    case Hash, Star, Under, Tick, Newline, Text

  type T = Token[K]

  // ---------------------------------------------------------------- scan

  final case class P(off: Int, line: Int, col: Int):
    def +(c: Char): P =
      if c == '\n' then P(off + 1, line + 1, 0) else P(off + 1, line, col + 1)

  final case class S(buf: String, start: P, at: P)

  val scan: Scan[K, S] = new Scan[K, S]:
    def init: S = S("", P(0, 0, 0), P(0, 0, 0))

    override def key(s: S): Any = s.buf

    override def rebase(s: S, offsetDelta: Int, lineDelta: Int): S =
      def shift(p: P) = P(p.off + offsetDelta, p.line + lineDelta, p.col)
      S(s.buf, shift(s.start), shift(s.at))

    private def one(k: K, c: Char, at: P): Token[K] =
      Token(k, c.toString, Span(at.off, at.line, at.col, 1))

    private def flushed(s: S): Vector[Token[K]] =
      if s.buf.isEmpty then Vector.empty
      else Vector(Token(K.Text, s.buf,
        Span(s.start.off, s.start.line, s.start.col, s.buf.length)))

    def step(s: S, c: Char): (S, Vector[Token[K]]) =
      val special = c match
        case '#' => Some(K.Hash)
        case '*' => Some(K.Star)
        case '_' => Some(K.Under)
        case '`' => Some(K.Tick)
        case '\n' => Some(K.Newline)
        case _ => None
      special match
        case Some(k) =>
          val next = s.at + c
          (S("", next, next), flushed(s) :+ one(k, c, s.at))
        case None =>
          if s.buf.isEmpty then (S(c.toString, s.at, s.at + c), Vector.empty)
          else (s.copy(buf = s.buf + c, at = s.at + c), Vector.empty)

    def flush(s: S): Vector[Token[K]] = flushed(s)

  // ---------------------------------------------------------------- drive

  /** which node an emphasis marker opens */
  private def kind(k: K): String = k match
    case K.Star => "em"
    case K.Under => "u-em"
    case K.Tick => "code"
    case _ => "?"

  private final case class D(stack: List[K], para: Boolean, heading: Boolean)

  /**
   * The instruction stream: a fold over the tokens with an explicit
   * frame stack. Total — every token lands in the tree; the crossing
   * close is the reframe (close inner frames tokenless, close the
   * target with its token, reopen the inner frames).
   */
  def instructions(tokens: IterableOnce[T]): Vector[Instr[K]] =
    val out = Vector.newBuilder[Instr[K]]
    var d = D(Nil, para = false, heading = false)

    def openPara(): Unit =
      if !d.para && !d.heading then
        out += Instr.Open("para", None)
        d = d.copy(para = true)

    def closeParagraph(tok: Option[T]): Unit =
      if d.para then
        d.stack.foreach(_ => out += Instr.Close(None))   // unterminated emphasis
        tok.foreach(t => out += Instr.Emit(t))
        out += Instr.Close(None)
        d = D(Nil, para = false, heading = false)
      else tok.foreach(t => out += Instr.Emit(t))

    def emphasis(k: K, t: T): Unit =
      openPara()
      if d.stack.headOption.contains(K.Tick) && k != K.Tick then
        out += Instr.Emit(t)   // inside a code span markers are literal
      else if d.stack.contains(k) then
        val inner = d.stack.takeWhile(_ != k)
        inner.foreach(_ => out += Instr.Close(None))     // the crossing closes
        out += Instr.Close(Some(t))                      // the target, with its token
        inner.reverse.foreach(i => out += Instr.Open(kind(i), None))   // the reframe
        d = d.copy(stack = inner ::: d.stack.dropWhile(_ != k).tail)
      else
        out += Instr.Open(kind(k), Some(t))
        d = d.copy(stack = k :: d.stack)

    tokens.iterator.foreach { t =>
      t.kind match
        case K.Hash =>
          if d.heading || d.para then { openPara(); out += Instr.Emit(t) }
          else
            out += Instr.Open("heading", Some(t))
            d = d.copy(heading = true)
        case K.Newline =>
          if d.heading then
            out += Instr.Close(Some(t))
            d = D(Nil, para = false, heading = false)
          else closeParagraph(Some(t))
        case K.Star | K.Under | K.Tick => emphasis(t.kind, t)
        case K.Text =>
          openPara()
          out += Instr.Emit(t)
        case _ => out += Instr.Bad(Some(t), "unexpected")
    }
    out.result()

  /** text to CST: total, lossless, reframed; unclosed frames become
   * the builder's error nodes */
  def parse(input: String): Cst[K] =
    var s = scan.init
    val toks = Vector.newBuilder[T]
    input.foreach { c =>
      val (s2, ts) = scan.step(s, c)
      toks ++= ts
      s = s2
    }
    toks ++= scan.flush(s)
    Parse.toCst(instructions(toks.result()))
}
