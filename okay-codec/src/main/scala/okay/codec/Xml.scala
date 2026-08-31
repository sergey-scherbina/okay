package okay.codec

import okay.lex.{Channel, Scan, Span, Token}
import okay.parse.{Cst, Instr, Parse}

/**
 * The XML/HTML dialect — the NESTING prover. JSON nests by
 * punctuation, YAML by indentation, Markdown does not nest at all
 * (hence reframing); this one nests by NAMED tags, which is the case
 * where a close can be wrong: mismatched, missing, or belonging to an
 * ancestor rather than the open node.
 *
 * Total as always. `</b>` with no `<b>` open is an error leaf, a
 * `</a>` that closes past an unclosed `<b>` closes both and says so,
 * void elements (`<br>`, `<img>`) never open a frame, and an
 * unterminated tag at end of input is still a token. Lossless: tags,
 * attributes, text, comments and CDATA are all kept, so
 * `render(parse(s)) == s` for every string.
 */
object Xml {

  enum K:
    case Open, Close, SelfClose, Name, Attr, Text, Comment, Cdata, Ws

  type T = Token[K]

  /** HTML elements that never have a closing tag */
  val void: Set[String] = Set("area", "base", "br", "col", "embed", "hr",
    "img", "input", "link", "meta", "param", "source", "track", "wbr")

  final case class P(off: Int, line: Int, col: Int):
    def +(c: Char): P =
      if c == '\n' then P(off + 1, line + 1, 0) else P(off + 1, line, col + 1)

  enum Mode:
    case Text, InTag, InComment, InCdata
    case InQuote(quote: Char)

  final case class S(mode: Mode, buf: String, start: P, at: P)

  val scan: Scan[K, S] = new Scan[K, S]:
    def init: S = S(Mode.Text, "", P(0, 0, 0), P(0, 0, 0))

    override def key(s: S): Any = (s.mode.ordinal, s.buf)

    override def rebase(s: S, offsetDelta: Int, lineDelta: Int): S =
      def shift(p: P) = P(p.off + offsetDelta, p.line + lineDelta, p.col)
      val m = s.mode match
        case Mode.InQuote(q) => Mode.InQuote(q)
        case other => other
      S(m, s.buf, shift(s.start), shift(s.at))

    private def kindOf(buf: String): K =
      if buf.startsWith("<!--") then K.Comment
      else if buf.startsWith("<![CDATA[") then K.Cdata
      else if buf.startsWith("</") then K.Close
      else if buf.endsWith("/>") then K.SelfClose
      else if buf.startsWith("<") then K.Open
      else if buf.forall(_.isWhitespace) then K.Ws
      else K.Text

    private def tok(s: S, k: K): Vector[T] =
      if s.buf.isEmpty then Vector.empty
      else
        val ch = k match
          case K.Comment => Channel.Comment
          case K.Ws => Channel.Trivia
          case _ => Channel.Syntax
        Vector(Token(k, s.buf,
          Span(s.start.off, s.start.line, s.start.col, s.buf.length), ch))

    private def flushed(s: S): Vector[T] = s.mode match
      case Mode.Text => tok(s, kindOf(s.buf))
      // an unterminated tag, comment or CDATA is still a token
      case _ => tok(s, kindOf(s.buf))

    def step(s: S, c: Char): (S, Vector[T]) =
      val next = s.at + c
      def keep = s.copy(buf = s.buf + c, at = next)
      def begin(m: Mode) = S(m, c.toString, s.at, next)

      s.mode match
        case Mode.InComment =>
          val b = s.buf + c
          if b.endsWith("-->") then
            (S(Mode.Text, "", next, next), tok(s.copy(buf = b), K.Comment))
          else (keep, Vector.empty)

        case Mode.InCdata =>
          val b = s.buf + c
          if b.endsWith("]]>") then
            (S(Mode.Text, "", next, next), tok(s.copy(buf = b), K.Cdata))
          else (keep, Vector.empty)

        case Mode.InQuote(q) =>
          if c == q then (keep.copy(mode = Mode.InTag), Vector.empty)
          else (keep, Vector.empty)

        case Mode.InTag =>
          if c == '"' || c == '\'' then (keep.copy(mode = Mode.InQuote(c)), Vector.empty)
          else if c == '>' then
            val b = s.buf + c
            val m = if b.startsWith("<!--") then Mode.InComment else Mode.Text
            (S(Mode.Text, "", next, next), tok(s.copy(buf = b), kindOf(b)))
          else
            val b = s.buf + c
            // the shape is decided as it is read: a comment or CDATA
            // swallows everything up to its own terminator
            if b == "<!--" then (S(Mode.InComment, b, s.start, next), Vector.empty)
            else if b == "<![CDATA[" then (S(Mode.InCdata, b, s.start, next), Vector.empty)
            else (keep, Vector.empty)

        case Mode.Text =>
          if c == '<' then
            val closing = flushed(s)
            (S(Mode.InTag, "<", s.at, next), closing)
          else if s.buf.isEmpty then (begin(Mode.Text), Vector.empty)
          // text and whitespace are different tokens, so a run breaks
          // where the character class does
          else if s.buf.forall(_.isWhitespace) != c.isWhitespace then
            (begin(Mode.Text), flushed(s))
          else (keep, Vector.empty)

    def flush(s: S): Vector[T] = flushed(s)

  // ---------------------------------------------------------------- drive

  /** the element name inside a tag token */
  def nameOf(lexeme: String): String =
    lexeme.dropWhile(c => c == '<' || c == '/')
      .takeWhile(c => c.isLetterOrDigit || c == '-' || c == '_' || c == ':')
      .toLowerCase

  /** the driver's state: the names of the elements still open */
  final case class D(open: List[String])

  val initD: D = D(Nil)

  /**
   * Instructions from tags. A close that does not match the innermost
   * open element closes everything up to the matching ancestor (and
   * says so on the error channel); a close with no matching ancestor
   * is an error leaf on its own. Void elements never open.
   */
  val step: Parse.Step[K, D] = (d, t) =>
    val out = Vector.newBuilder[Instr[K]]
    t.kind match
      case K.Open =>
        val n = nameOf(t.lexeme)
        if void(n) then
          out += Instr.Open(n, Some(t))
          out += Instr.Close(None)
          (d, out.result())
        else
          out += Instr.Open(n, Some(t))
          (D(n :: d.open), out.result())

      case K.SelfClose =>
        out += Instr.Open(nameOf(t.lexeme), Some(t))
        out += Instr.Close(None)
        (d, out.result())

      case K.Close =>
        val n = nameOf(t.lexeme)
        if !d.open.contains(n) then
          out += Instr.Bad(Some(t), s"</$n> closes nothing")
          (d, out.result())
        else
          // close the unclosed ones first, marked, then the match
          val inner = d.open.takeWhile(_ != n)
          for u <- inner do
            out += Instr.Bad(None, s"<$u> was never closed")
            out += Instr.Close(None)
          out += Instr.Close(Some(t))
          (D(d.open.dropWhile(_ != n).tail), out.result())

      case _ => (d, Vector(Instr.Emit(t)))

  /** text to CST: total, lossless, nesting by name */
  /** nothing is deferred by this driver, but the type argument has
   * to be spelled: an empty default infers Instr[Nothing] */
  val finish: D => Vector[Instr[K]] = _ => Vector.empty[Instr[K]]

  def cst(input: String): Cst[K] =
    Parse.fullWith(scan, step, initD, finish)(input).tree

  /** render = the lossless law */
  def render(c: Cst[K]): String = Cst.lexemes(c)

  /** a parsed session, for incremental reparse */
  def parse(input: String, snapshotEvery: Int = 64): Parse.Parsed[K, S, D] =
    Parse.fullWith(scan, step, initD, finish)(input, snapshotEvery)

  def reparse(old: Parse.Parsed[K, S, D], oldText: String, newText: String,
              editStart: Int, editEndOld: Int, editEndNew: Int,
              snapshotEvery: Int = 64): Parse.Parsed[K, S, D] =
    Parse.reparseWith(scan, step, initD, finish)(
      old, oldText, newText, editStart, editEndOld, editEndNew, snapshotEvery)

  // ---------------------------------------------------------------- project

  /** the text an element contains, trivia and markup dropped */
  def text(c: Cst[K]): String = c match
    case Cst.Node(_, kids) => kids.map(text).mkString
    case Cst.Leaf(t) if t.kind == K.Text || t.kind == K.Ws => t.lexeme
    case _ => ""

  /** every element of a given name, in document order */
  def elements(c: Cst[K], name: String): Vector[Cst[K]] = c match
    case n @ Cst.Node(k, kids) =>
      (if k == name then Vector(n) else Vector.empty) ++
        kids.flatMap(elements(_, name))
    case _ => Vector.empty
}
