package okay2.codec

import okay2.lex.{Scan, ScanInto, Span, Token}
import okay2.parse.{Cst, Instr, Parse}
import scala.collection.mutable.Growable

/**
 * The Markdown dialect (okay-codec's Markdown.scala) — the REFRAMING
 * prover. Markdown emphasis does not nest: `*a _b* c_` closes the star
 * while the underscore is still open. The answer (adoption-agency in
 * miniature): close the crossing inner frames without a token, close
 * the target with its token, REOPEN the inner frames — the tree stays
 * well-nested, every marker token is kept (lossless), nothing ever
 * faults; whatever stays open at the end is the builder's "unclosed"
 * error node, an error AS DATA.
 *
 * Deliberately small: headings (#... to end of line), paragraphs (one
 * line), emphasis * and _, code spans ` (no nesting inside).
 */
object Markdown {

  sealed trait K
  object K {
    case object Hash extends K
    case object Star extends K
    case object Under extends K
    case object Tick extends K
    case object Newline extends K
    case object Text extends K
  }

  type T = Token[K]

  // ---------------------------------------------------------------- scan

  final case class P(off: Int, line: Int, col: Int) {
    def +(c: Char): P = if (c == '\n') P(off + 1, line + 1, 0) else P(off + 1, line, col + 1)
  }

  final case class S(buf: String, start: P, at: P)

  val scan: Scan[K, S] = new ScanInto[K, S] {
    def init: S = S("", P(0, 0, 0), P(0, 0, 0))

    override def key(s: S): Any = s.buf

    override def rebase(s: S, offsetDelta: Int, lineDelta: Int): S = {
      def shift(p: P) = P(p.off + offsetDelta, p.line + lineDelta, p.col)
      S(s.buf, shift(s.start), shift(s.at))
    }

    private def one(k: K, c: Char, at: P): T = Token(k, c.toString, Span(at.off, at.line, at.col, 1))

    private def flushed(s: S): Vector[T] =
      if (s.buf.isEmpty) Vector.empty
      else Vector(Token(K.Text, s.buf, Span(s.start.off, s.start.line, s.start.col, s.buf.length)))

    override def stepInto(s: S, c: Char, out: Growable[T]): S = {
      val special: Option[K] = c match {
        case '#' => Some(K.Hash)
        case '*' => Some(K.Star)
        case '_' => Some(K.Under)
        case '`' => Some(K.Tick)
        case '\n' => Some(K.Newline)
        case _ => None
      }
      special match {
        case Some(k) =>
          val next = s.at + c
          if (s.buf.nonEmpty)
            out += Token(K.Text, s.buf, Span(s.start.off, s.start.line, s.start.col, s.buf.length))
          out += one(k, c, s.at)
          S("", next, next)
        case None =>
          if (s.buf.isEmpty) S(c.toString, s.at, s.at + c)
          else s.copy(buf = s.buf + c, at = s.at + c)
      }
    }

    def flush(s: S): Vector[T] = flushed(s)
  }

  // ---------------------------------------------------------------- drive

  /** which node an emphasis marker opens */
  private def kind(k: K): String = k match {
    case K.Star => "em"
    case K.Under => "u-em"
    case K.Tick => "code"
    case _ => "?"
  }

  private final case class D(stack: List[K], para: Boolean, heading: Boolean)

  /**
   * The instruction stream: a fold over the tokens with an explicit
   * frame stack. Total — every token lands in the tree; the crossing
   * close is the reframe (close inner frames tokenless, close the
   * target with its token, reopen the inner frames).
   */
  def instructions(tokens: IterableOnce[T]): Vector[Instr[K]] = {
    val out = Vector.newBuilder[Instr[K]]
    var d = D(Nil, para = false, heading = false)

    def openPara(): Unit =
      if (!d.para && !d.heading) {
        out += Instr.Open[K]("para", None)
        d = d.copy(para = true)
      }

    def closeParagraph(tok: Option[T]): Unit =
      if (d.para) {
        d.stack.foreach(_ => out += Instr.Close[K](None)) // unterminated emphasis
        tok.foreach(t => out += Instr.Emit(t))
        out += Instr.Close[K](None)
        d = D(Nil, para = false, heading = false)
      } else tok.foreach(t => out += Instr.Emit(t))

    def emphasis(k: K, t: T): Unit = {
      openPara()
      if (d.stack.headOption.contains(K.Tick) && k != K.Tick)
        out += Instr.Emit(t): Unit // inside a code span markers are literal
      else if (d.stack.contains(k)) {
        val inner = d.stack.takeWhile(_ != k)
        inner.foreach(_ => out += Instr.Close[K](None)) // the crossing closes
        out += Instr.Close(Some(t)) // the target, with its token
        inner.reverse.foreach(i => out += Instr.Open[K](kind(i), None)) // the reframe
        d = d.copy(stack = inner ::: d.stack.dropWhile(_ != k).tail)
      } else {
        out += Instr.Open(kind(k), Some(t))
        d = d.copy(stack = k :: d.stack)
      }
    }

    tokens.iterator.foreach { t =>
      t.kind match {
        case K.Hash =>
          if (d.heading || d.para) { openPara(); out += Instr.Emit(t) }
          else {
            out += Instr.Open("heading", Some(t))
            d = d.copy(heading = true)
          }
        case K.Newline =>
          if (d.heading) {
            out += Instr.Close(Some(t))
            d = D(Nil, para = false, heading = false)
          } else closeParagraph(Some(t))
        case K.Star | K.Under | K.Tick => emphasis(t.kind, t)
        case K.Text =>
          openPara()
          out += Instr.Emit(t)
      }
    }
    out.result()
  }

  /** text to CST: total, lossless, reframed; unclosed frames become the
   * builder's error nodes */
  def parse(input: String): Cst[K] = {
    var s = scan.init
    val toks = Vector.newBuilder[T]
    var i = 0
    while (i < input.length) {
      s = scan.stepInto(s, input.charAt(i), toks)
      i += 1
    }
    toks ++= scan.flush(s)
    Parse.toCst(instructions(toks.result()))
  }
}
