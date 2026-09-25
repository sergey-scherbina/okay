package okay.codec

import okay.lex.{Channel, Scan, ScanInto, Span, Token}
import okay.parse.{Cst, Instr, Parse}
import scala.collection.mutable.Growable

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

  // extends ScanInto rather than Scan (scan-into-the-other-scanners):
  // step never recurses into itself here (unlike Yaml/Code, left for
  // later), so the conversion is mechanical — writing straight onto
  // the sink instead of building a (S, Vector[Token[K]]) pair avoids
  // both the Tuple2 per character and the Vector per finished token
  // (docs/benchmarks.md §10, the same move Markdown already made).
  val scan: Scan[K, S] = new ScanInto[K, S]:
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

    private def tokInto(s: S, k: K, out: Growable[T]): Unit =
      if s.buf.nonEmpty then
        val ch = k match
          case K.Comment => Channel.Comment
          case K.Ws => Channel.Trivia
          case _ => Channel.Syntax
        out += Token(k, s.buf,
          Span(s.start.off, s.start.line, s.start.col, s.buf.length), ch)

    private def flushedInto(s: S, out: Growable[T]): Unit =
      // an unterminated tag, comment or CDATA is still a token
      tokInto(s, kindOf(s.buf), out)

    override def stepInto(s: S, c: Char, out: Growable[T]): S =
      val next = s.at + c
      def keep = s.copy(buf = s.buf + c, at = next)
      def begin(m: Mode) = S(m, c.toString, s.at, next)

      s.mode match
        case Mode.InComment =>
          val b = s.buf + c
          if b.endsWith("-->") then
            tokInto(s.copy(buf = b), K.Comment, out)
            S(Mode.Text, "", next, next)
          else keep

        case Mode.InCdata =>
          val b = s.buf + c
          if b.endsWith("]]>") then
            tokInto(s.copy(buf = b), K.Cdata, out)
            S(Mode.Text, "", next, next)
          else keep

        case Mode.InQuote(q) =>
          if c == q then keep.copy(mode = Mode.InTag) else keep

        case Mode.InTag =>
          if c == '"' || c == '\'' then keep.copy(mode = Mode.InQuote(c))
          else if c == '>' then
            // no comment check here: a `<!--` has already switched to
            // InComment in the branch below, so by the time a `>` is
            // seen in InTag the tag is an ordinary one. (There WAS a
            // check, computing a mode and discarding it — dead since
            // the switch moved, and the compiler was saying so.)
            val b = s.buf + c
            tokInto(s.copy(buf = b), kindOf(b), out)
            S(Mode.Text, "", next, next)
          else
            val b = s.buf + c
            // the shape is decided as it is read: a comment or CDATA
            // swallows everything up to its own terminator
            if b == "<!--" then S(Mode.InComment, b, s.start, next)
            else if b == "<![CDATA[" then S(Mode.InCdata, b, s.start, next)
            else keep

        case Mode.Text =>
          if c == '<' then
            flushedInto(s, out)
            S(Mode.InTag, "<", s.at, next)
          else if s.buf.isEmpty then begin(Mode.Text)
          // text and whitespace are different tokens, so a run breaks
          // where the character class does
          else if s.buf.forall(_.isWhitespace) != c.isWhitespace then
            flushedInto(s, out)
            begin(Mode.Text)
          else keep

    def flush(s: S): Vector[T] =
      val sink = new Scan.Sink[K]
      flushedInto(s, sink)
      sink.result()

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

  /** every node, pre-order, on an EXPLICIT stack: the builder builds a
   * document as deep as its tags nest, and a walk that recursed per
   * level (`kids.map(text)`, `kids.flatMap(elements(_, name))`) threw
   * StackOverflowError on one it had just built — 20 000 levels
   * (xml-projection-stack-safe, found porting to okay2, 2026-09-25) */
  private def preorder(c: Cst[K])(visit: Cst[K] => Unit): Unit =
    var stack: List[Cst[K]] = c :: Nil
    while stack.nonEmpty do
      val here = stack.head
      stack = stack.tail
      visit(here)
      here match
        case Cst.Node(_, kids) => stack = kids.foldRight(stack)(_ :: _)
        case _ => ()

  /** the text an element contains, trivia kept, markup dropped */
  def text(c: Cst[K]): String =
    val out = new StringBuilder
    preorder(c) {
      case Cst.Leaf(t) if t.kind == K.Text || t.kind == K.Ws => out ++= t.lexeme
      case _ => ()
    }
    out.result()

  /** every element of a given name, in document order */
  def elements(c: Cst[K], name: String): Vector[Cst[K]] =
    val out = Vector.newBuilder[Cst[K]]
    preorder(c) {
      case n @ Cst.Node(k, _) if k == name => out += n
      case _ => ()
    }
    out.result()
}
