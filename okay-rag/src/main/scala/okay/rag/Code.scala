package okay.rag

import okay.lex.{Channel, Scan, Span, Token}
import okay.parse.{Cst, Instr, Parse}

/**
 * Source code as a corpus (specs/rag.md, P10f). The grammar is
 * deliberately a DEFINITION-BOUNDARY grammar, not a language: braces,
 * comments, strings and a handful of keywords are enough to cut a
 * file into whole definitions with their doc comments and to name
 * them. That is only a sane thing to build because our parser is
 * TOTAL — an imperfect grammar degrades into error nodes and
 * ordinary leaves instead of failing, so precision can be sharpened
 * later without a rewrite. A parser generator offers no such
 * gradient.
 *
 * The scanner is brace-family (Scala, Java, JS/TS, C-like); an
 * indent-family sibling would reuse the YAML indent stack. Comments
 * and whitespace ride their own channels, so the lossless law holds
 * and every segment still quotes its file exactly.
 */
object Code {

  enum K:
    case Keyword, Ident, Doc, Comment, Str, Open, Close, Punct, Ws, Newline

  type T = Token[K]

  /** the words that begin a definition worth indexing */
  val definers: Set[String] =
    Set("def", "val", "var", "class", "object", "trait", "enum", "type",
      "given", "case", "interface", "struct", "func", "function", "fn", "let")

  final case class P(off: Int, line: Int, col: Int):
    def +(c: Char): P =
      if c == '\n' then P(off + 1, line + 1, 0) else P(off + 1, line, col + 1)

  enum Mode:
    case Base, InIdent, InWs
    case InStr(quote: Char, esc: Boolean)
    case InLine                      // // to end of line
    case InBlock(star: Boolean, doc: Boolean)   // /* ... */ and /** ... */
    case SawSlash(at: P)             // one-char lookahead: / // /*

  final case class S(mode: Mode, buf: String, start: P, at: P)

  val scan: Scan[K, S] = new Scan[K, S]:
    def init: S = S(Mode.Base, "", P(0, 0, 0), P(0, 0, 0))

    override def key(s: S): Any = (s.mode.ordinal, s.buf)

    override def rebase(s: S, offsetDelta: Int, lineDelta: Int): S =
      def shift(p: P) = P(p.off + offsetDelta, p.line + lineDelta, p.col)
      val m = s.mode match
        case Mode.SawSlash(p) => Mode.SawSlash(shift(p))
        case other => other
      S(m, s.buf, shift(s.start), shift(s.at))

    private def tok(k: K, s: S, ch: Channel = Channel.Syntax): Vector[T] =
      if s.buf.isEmpty then Vector.empty
      else Vector(Token(k, s.buf,
        Span(s.start.off, s.start.line, s.start.col, s.buf.length), ch))

    private def one(k: K, c: Char, at: P, ch: Channel = Channel.Syntax): T =
      Token(k, c.toString, Span(at.off, at.line, at.col, 1), ch)

    private def flushed(s: S): Vector[T] = s.mode match
      case Mode.InIdent =>
        if definers(s.buf) then tok(K.Keyword, s) else tok(K.Ident, s)
      case Mode.InWs => tok(K.Ws, s, Channel.Trivia)
      case Mode.InStr(_, _) => tok(K.Str, s)              // unterminated: a token
      case Mode.InLine => tok(K.Comment, s, Channel.Comment)
      case Mode.InBlock(_, doc) =>
        tok(if doc then K.Doc else K.Comment, s, Channel.Comment)
      case Mode.SawSlash(at) =>
        Vector(Token(K.Punct, "/", Span(at.off, at.line, at.col, 1)))
      case Mode.Base => Vector.empty

    def step(s: S, c: Char): (S, Vector[T]) =
      val next = s.at + c
      def fresh(m: Mode) = S(m, "", next, next)
      def begin(m: Mode) = S(m, c.toString, s.at, next)
      def keep(m: Mode) = S(m, s.buf + c, s.start, next)

      s.mode match
        case Mode.InLine =>
          if c == '\n' then
            (fresh(Mode.Base), flushed(s) :+ one(K.Newline, c, s.at, Channel.Trivia))
          else (keep(Mode.InLine), Vector.empty)

        case Mode.InBlock(star, doc) =>
          if star && c == '/' then (fresh(Mode.Base), flushed(keep(Mode.InBlock(false, doc))))
          // the third character decides: /** is documentation
          else if s.buf == "/*" && c == '*' then
            (keep(Mode.InBlock(true, doc = true)), Vector.empty)
          else (keep(Mode.InBlock(c == '*', doc)), Vector.empty)

        case Mode.InStr(q, esc) =>
          if esc then (keep(Mode.InStr(q, false)), Vector.empty)
          else if c == '\\' then (keep(Mode.InStr(q, true)), Vector.empty)
          else if c == q then (fresh(Mode.Base), tok(K.Str, keep(Mode.InStr(q, false))))
          else (keep(Mode.InStr(q, false)), Vector.empty)

        case Mode.SawSlash(at) =>
          val slashTok = Token(K.Punct, "/", Span(at.off, at.line, at.col, 1))
          if c == '/' then (S(Mode.InLine, "//", at, next), Vector.empty)
          else if c == '*' then (S(Mode.InBlock(false, doc = false), "/*", at, next), Vector.empty)
          else
            val (s2, ts) = step(S(Mode.Base, "", s.at, s.at), c)
            (s2, slashTok +: ts)

        case mode =>   // Base, InIdent, InWs
          val closing = flushed(s)
          def emit(t: T) = (fresh(Mode.Base), closing :+ t)
          c match
            case '\n' => (fresh(Mode.Base), closing :+ one(K.Newline, c, s.at, Channel.Trivia))
            case '/' => (S(Mode.SawSlash(s.at), "", s.at, next), closing)
            case '"' | '\'' => (S(Mode.InStr(c, false), c.toString, s.at, next), closing)
            case '{' | '(' | '[' => emit(one(K.Open, c, s.at))
            case '}' | ')' | ']' => emit(one(K.Close, c, s.at))
            case _ if c.isWhitespace =>
              if mode == Mode.InWs then (keep(Mode.InWs), Vector.empty)
              else (begin(Mode.InWs), closing)
            case _ if c.isLetterOrDigit || c == '_' || c == '$' =>
              if mode == Mode.InIdent then (keep(Mode.InIdent), Vector.empty)
              else (begin(Mode.InIdent), closing)
            case _ => emit(one(K.Punct, c, s.at))

    def flush(s: S): Vector[T] = flushed(s)

  // ---------------------------------------------------------------- drive

  /**
   * The driver's state: how deep in braces we are, the depths at
   * which definitions are still open, and whether a doc comment is
   * waiting to be adopted by the definition that follows it.
   *
   * A definition opens at its keyword — or, when a doc comment came
   * first, BEFORE that comment, so the comment lands inside the
   * definition where a reader expects it. It closes when the braces
   * return to the depth it opened at and a body has been seen, or
   * when the next definition at the same depth begins (the one-liner
   * case), or at the end of file (the builder closes the leftovers).
   */
  final case class D(depth: Int, open: List[(Int, Boolean)], pendingDoc: Vector[T]):
    def openDepths: List[Int] = open.map(_._1)

  val initD: D = D(0, Nil, Vector.empty)

  val step: Parse.Step[K, D] = (d, t) =>
    val out = Vector.newBuilder[Instr[K]]

    def closeWhile(p: ((Int, Boolean)) => Boolean, st: D): D =
      var s = st
      while s.open.headOption.exists(p) do
        out += Instr.Close(None)
        s = s.copy(open = s.open.tail)
      s

    t.kind match
      case K.Doc =>
        // hold the doc comment: it belongs to what comes next
        (d.copy(pendingDoc = d.pendingDoc :+ t), Vector.empty)

      case K.Keyword =>
        // a definition at this depth ends any definition already open
        // at the same depth (the one-liner case)
        val closed = closeWhile(_._1 >= d.depth, d)
        out += Instr.Open("def", None)
        closed.pendingDoc.foreach(dt => out += Instr.Emit(dt))
        out += Instr.Emit(t)
        (closed.copy(open = (d.depth, false) :: closed.open,
          pendingDoc = Vector.empty), out.result())

      case K.Open =>
        d.pendingDoc.foreach(dt => out += Instr.Emit(dt))
        out += Instr.Emit(t)
        // the innermost open definition now has a body
        val marked = d.open match
          case (dep, _) :: rest if dep == d.depth => (dep, true) :: rest
          case other => other
        (D(d.depth + 1, marked, Vector.empty), out.result())

      case K.Close =>
        val depth = math.max(0, d.depth - 1)
        d.pendingDoc.foreach(dt => out += Instr.Emit(dt))
        out += Instr.Emit(t)
        // definitions that owned this brace are finished
        var s = D(depth, d.open, Vector.empty)
        while s.open.headOption.exists((dep, body) => body && dep >= depth) do
          out += Instr.Close(None)
          s = s.copy(open = s.open.tail)
        (s, out.result())

      // trivia between a doc comment and its definition is held too,
      // so the comment stays attached across the newline
      case K.Ws | K.Newline if d.pendingDoc.nonEmpty =>
        (d.copy(pendingDoc = d.pendingDoc :+ t), Vector.empty)

      case _ =>
        d.pendingDoc.foreach(dt => out += Instr.Emit(dt))
        out += Instr.Emit(t)
        (d.copy(pendingDoc = Vector.empty), out.result())

  /** whatever the driver still holds at end of input (a doc comment
   * with no definition after it) — nothing deferred may be lost */
  val finish: D => Vector[Instr[K]] = d => d.pendingDoc.map(Instr.Emit(_))

  /** parse a source file into definition-shaped nodes */
  def parse(text: String, snapshotEvery: Int = 64): Parse.Parsed[K, S, D] =
    Parse.fullWith(scan, step, initD, finish)(text, snapshotEvery)

  /** reparse after an edit: only the damage is re-driven */
  def reparse(old: Parse.Parsed[K, S, D], oldText: String, newText: String,
              editStart: Int, editEndOld: Int, editEndNew: Int,
              snapshotEvery: Int = 64): Parse.Parsed[K, S, D] =
    Parse.reparseWith(scan, step, initD, finish)(
      old, oldText, newText, editStart, editEndOld, editEndNew, snapshotEvery)
}
