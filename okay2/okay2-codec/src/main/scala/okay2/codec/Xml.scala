package okay2.codec

import scala.collection.mutable.Growable
import okay2.lex.{Channel, Scan, ScanInto, Span, Token}
import okay2.parse.{Cst, Instr, Parse}

/**
 * The XML/HTML dialect (okay-codec's Xml.scala), the NESTING prover.
 * JSON nests by punctuation, YAML by indentation; this one nests by
 * NAMED tags, which is the case where a close can be wrong: mismatched,
 * missing, or belonging to an ancestor rather than the open node.
 *
 * Total as always: `</b>` with no `<b>` open is an error leaf, a `</a>`
 * that closes past an unclosed `<b>` closes both and says so, void
 * elements (`<br>`, `<img>`) never open a frame, and an unterminated tag
 * at end of input is still a token. Lossless: `render(cst(s)) == s` for
 * every string.
 */
object Xml {

  sealed trait K
  object K {
    case object Open extends K
    case object Close extends K
    case object SelfClose extends K
    case object Name extends K
    case object Attr extends K
    case object Text extends K
    case object Comment extends K
    case object Cdata extends K
    case object Ws extends K
  }

  type T = Token[K]

  /** HTML elements that never have a closing tag */
  val void: Set[String] = Set("area", "base", "br", "col", "embed", "hr",
    "img", "input", "link", "meta", "param", "source", "track", "wbr")

  final case class P(off: Int, line: Int, col: Int) {
    def +(c: Char): P = if (c == '\n') P(off + 1, line + 1, 0) else P(off + 1, line, col + 1)
  }

  sealed trait Mode
  object Mode {
    case object Text extends Mode
    case object InTag extends Mode
    case object InComment extends Mode
    case object InCdata extends Mode
    final case class InQuote(quote: Char) extends Mode
  }

  final case class S(mode: Mode, buf: String, start: P, at: P)

  val scan: Scan[K, S] = new ScanInto[K, S] {
    def init: S = S(Mode.Text, "", P(0, 0, 0), P(0, 0, 0))

    override def key(s: S): Any = (s.mode, s.buf)

    override def rebase(s: S, offsetDelta: Int, lineDelta: Int): S = {
      def shift(p: P) = P(p.off + offsetDelta, p.line + lineDelta, p.col)
      S(s.mode, s.buf, shift(s.start), shift(s.at))
    }

    private def kindOf(buf: String): K =
      if (buf.startsWith("<!--")) K.Comment
      else if (buf.startsWith("<![CDATA[")) K.Cdata
      else if (buf.startsWith("</")) K.Close
      else if (buf.endsWith("/>")) K.SelfClose
      else if (buf.startsWith("<")) K.Open
      else if (buf.forall(_.isWhitespace)) K.Ws
      else K.Text

    private def tokInto(s: S, k: K, out: Growable[T]): Unit =
      if (s.buf.nonEmpty) {
        val ch = k match {
          case K.Comment => Channel.Comment
          case K.Ws => Channel.Trivia
          case _ => Channel.Syntax
        }
        out += Token(k, s.buf, Span(s.start.off, s.start.line, s.start.col, s.buf.length), ch)
      }

    /** an unterminated tag, comment or CDATA is still a token */
    private def flushedInto(s: S, out: Growable[T]): Unit = tokInto(s, kindOf(s.buf), out)

    override def stepInto(s: S, c: Char, out: Growable[T]): S = {
      val next = s.at + c
      def keep = s.copy(buf = s.buf + c, at = next)
      def begin(m: Mode) = S(m, c.toString, s.at, next)

      s.mode match {
        case Mode.InComment =>
          val b = s.buf + c
          if (b.endsWith("-->")) {
            tokInto(s.copy(buf = b), K.Comment, out)
            S(Mode.Text, "", next, next)
          } else keep

        case Mode.InCdata =>
          val b = s.buf + c
          if (b.endsWith("]]>")) {
            tokInto(s.copy(buf = b), K.Cdata, out)
            S(Mode.Text, "", next, next)
          } else keep

        case Mode.InQuote(q) =>
          if (c == q) keep.copy(mode = Mode.InTag) else keep

        case Mode.InTag =>
          if (c == '"' || c == '\'') keep.copy(mode = Mode.InQuote(c))
          else if (c == '>') {
            val b = s.buf + c
            tokInto(s.copy(buf = b), kindOf(b), out)
            S(Mode.Text, "", next, next)
          } else {
            val b = s.buf + c
            // decided as it is read: a comment or CDATA swallows
            // everything up to its own terminator
            if (b == "<!--") S(Mode.InComment, b, s.start, next)
            else if (b == "<![CDATA[") S(Mode.InCdata, b, s.start, next)
            else keep
          }

        case Mode.Text =>
          if (c == '<') {
            flushedInto(s, out)
            S(Mode.InTag, "<", s.at, next)
          } else if (s.buf.isEmpty) begin(Mode.Text)
          // text and whitespace are different tokens: a run breaks
          // where the character class does
          else if (s.buf.forall(_.isWhitespace) != c.isWhitespace) {
            flushedInto(s, out)
            begin(Mode.Text)
          } else keep
      }
    }

    def flush(s: S): Vector[T] = {
      val sink = new Scan.Sink[K]
      flushedInto(s, sink)
      sink.result()
    }
  }

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
   * says so on the error channel); a close with no matching ancestor is
   * an error leaf on its own. Void elements never open.
   */
  val step: Parse.Step[K, D] = (d, t) => t.kind match {
    case K.Open =>
      val n = nameOf(t.lexeme)
      if (void(n)) (d, Vector(Instr.Open[K](n, Some(t)), Instr.Close[K](None)))
      else (D(n :: d.open), Vector(Instr.Open[K](n, Some(t))))

    case K.SelfClose =>
      (d, Vector(Instr.Open[K](nameOf(t.lexeme), Some(t)), Instr.Close[K](None)))

    case K.Close =>
      val n = nameOf(t.lexeme)
      if (!d.open.contains(n)) (d, Vector(Instr.Bad[K](Some(t), s"</$n> closes nothing")))
      else {
        // close the unclosed ones first, marked, then the match
        val out = Vector.newBuilder[Instr[K]]
        for (u <- d.open.takeWhile(_ != n)) {
          out += Instr.Bad[K](None, s"<$u> was never closed")
          out += Instr.Close[K](None)
        }
        out += Instr.Close[K](Some(t))
        (D(d.open.dropWhile(_ != n).tail), out.result())
      }

    case _ => (d, Vector(Instr.Emit[K](t)))
  }

  /** nothing is deferred by this driver */
  val finish: D => Vector[Instr[K]] = _ => Vector.empty[Instr[K]]

  /** text to CST: total, lossless, nesting by name */
  def cst(input: String): Cst[K] = Parse.fullWith(scan, step, initD, finish)(input).tree

  /** the lossless law */
  def render(c: Cst[K]): String = Cst.lexemes(c)

  /** a parsed session, for incremental reparse */
  def parse(input: String, snapshotEvery: Int = 64): Parse.Parsed[K, S, D] =
    Parse.fullWith(scan, step, initD, finish)(input, snapshotEvery)

  def reparse(old: Parse.Parsed[K, S, D], oldText: String, newText: String,
              editStart: Int, editEndOld: Int, editEndNew: Int,
              snapshotEvery: Int = 64): Parse.Parsed[K, S, D] =
    Parse.reparseWith(scan, step, initD, finish)(old, oldText, newText, editStart, editEndOld, editEndNew, snapshotEvery)

  /** every node, pre-order, on an EXPLICIT stack: a document nests as
   * deep as its tags, and the builder builds any depth, so a projection
   * must walk any depth too */
  private def preorder(c: Cst[K])(visit: Cst[K] => Unit): Unit = {
    var stack: List[Cst[K]] = c :: Nil
    while (stack.nonEmpty) {
      val here = stack.head
      stack = stack.tail
      visit(here)
      here match {
        case Cst.Node(_, kids) => stack = kids.foldRight(stack)(_ :: _)
        case _ => ()
      }
    }
  }

  /** the text an element contains, trivia kept, markup dropped */
  def text(c: Cst[K]): String = {
    val out = new StringBuilder
    preorder(c) {
      case Cst.Leaf(t) if t.kind == K.Text || t.kind == K.Ws => out ++= t.lexeme
      case _ => ()
    }
    out.result()
  }

  /** every element of a given name, in document order */
  def elements(c: Cst[K], name: String): Vector[Cst[K]] = {
    val out = Vector.newBuilder[Cst[K]]
    preorder(c) {
      case n @ Cst.Node(k, _) if k == name => out += n
      case _ => ()
    }
    out.result()
  }
}
