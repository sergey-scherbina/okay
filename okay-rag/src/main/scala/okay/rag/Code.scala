package okay.rag

import okay.lex.{Channel, Scan, ScanInto, Span, Token}
import okay.parse.{Instr, Parse}
import scala.collection.mutable.Growable

/**
 * Source code as a corpus (specs/rag.md, P10f), for any language a
 * `Language` describes. The grammar is deliberately a
 * DEFINITION-BOUNDARY grammar, not a language: comments, strings and
 * a handful of keywords are enough to cut a file into whole
 * definitions with their doc comments and to name them.
 *
 * That is only a sane thing to build because our parser is TOTAL —
 * an imperfect description degrades into ordinary leaves instead of
 * failing, so a new language costs five lines in `Language` and can
 * be sharpened later without a rewrite. A parser generator offers no
 * such gradient.
 *
 * Both layouts are here: braces (C, Java, Scala, JS, TS, Rust, Go)
 * and indentation (Python), which is the same distinction okay-codec
 * met between JSON and YAML — and the indent driver is the YAML
 * indent stack again, one level up.
 */
object Code {

  enum K:
    case Keyword, Ident, Doc, Comment, Str, Open, Close, Punct, Ws, Newline

  type T = Token[K]

  /** the default: the language this library is written in */
  val definers: Set[String] = Language.scala.definers

  final case class P(off: Int, line: Int, col: Int):
    def +(c: Char): P =
      if c == '\n' then P(off + 1, line + 1, 0) else P(off + 1, line, col + 1)

  enum Mode:
    case Base, InIdent, InWs
    case InStr(quote: Char, esc: Boolean, triple: Boolean)
    /** counting the quotes that opened a string: one is plain, three
     * is a triple-quoted block, two is an empty string already over */
    case Quoting(quote: Char, count: Int)
    case InLine
    case InBlock(doc: Boolean)
    /** a multi-character comment marker needs one character of
     * lookahead: `/` may begin a line comment, a block comment, or
     * be division */
    case Pending(at: P, first: Char)

  final case class S(mode: Mode, buf: String, start: P, at: P)

  // ---------------------------------------------------------------- scan

  // extends ScanInto rather than Scan (scan-into-the-other-scanners):
  // like Yaml, step recurses into itself — Quoting's fall-through to
  // an empty string followed by a fresh char, and Pending's fall-
  // through to Base once a two-char marker did not match — so this
  // is real conversion work. Every recursive step(...) call becomes
  // stepInto(...) writing into the same sink, in the SAME order the
  // pair built (a token this branch owns, THEN whatever the
  // recursive call emits — `tok(...) ++ ts` and `punct +: ts` both
  // read left-to-right, which `out +=` then `stepInto(...)` matches).
  def scanner(lang: Language): Scan[K, S] = new ScanInto[K, S]:

    private val blockStart = lang.blockComment.map(_._1)
    private val blockEnd = lang.blockComment.map(_._2)
    private val twoCharStarts: Set[String] =
      (if lang.lineComment.length == 2 then Set(lang.lineComment) else Set.empty) ++
        blockStart.filter(_.length == 2).toSet

    def init: S = S(Mode.Base, "", P(0, 0, 0), P(0, 0, 0))

    override def key(s: S): Any = (s.mode.ordinal, s.buf)

    override def rebase(s: S, offsetDelta: Int, lineDelta: Int): S =
      def shift(p: P) = P(p.off + offsetDelta, p.line + lineDelta, p.col)
      val m = s.mode match
        case Mode.Pending(p, c) => Mode.Pending(shift(p), c)
        case other => other
      S(m, s.buf, shift(s.start), shift(s.at))

    private def tokInto(k: K, s: S, out: Growable[T], ch: Channel = Channel.Syntax): Unit =
      if s.buf.nonEmpty then out += Token(k, s.buf,
        Span(s.start.off, s.start.line, s.start.col, s.buf.length), ch)

    private def oneInto(k: K, c: Char, at: P, out: Growable[T], ch: Channel): Unit =
      out += Token(k, c.toString, Span(at.off, at.line, at.col, 1), ch)

    private def isDoc(buf: String): Boolean =
      lang.docPrefix.exists(buf.startsWith)

    private def flushedInto(s: S, out: Growable[T]): Unit = s.mode match
      case Mode.InIdent =>
        if lang.definers(s.buf) then tokInto(K.Keyword, s, out) else tokInto(K.Ident, s, out)
      case Mode.InWs => tokInto(K.Ws, s, out, Channel.Trivia)
      case Mode.InStr(_, _, _) => tokInto(K.Str, s, out)     // unterminated: still a token
      case Mode.Quoting(_, _) => tokInto(K.Str, s, out)      // a bare quote at the end
      // a doc comment can be a LINE comment: Rust's `///`, Go's
      // convention. Only the block form was checked before, so Rust
      // doc comments were never adopted by their definitions.
      case Mode.InLine =>
        tokInto(if isDoc(s.buf) then K.Doc else K.Comment, s, out, Channel.Comment)
      case Mode.InBlock(doc) =>
        tokInto(if doc then K.Doc else K.Comment, s, out, Channel.Comment)
      case Mode.Pending(at, c) =>
        out += Token(K.Punct, c.toString, Span(at.off, at.line, at.col, 1))
      case Mode.Base => ()

    override def stepInto(s: S, c: Char, out: Growable[T]): S =
      val next = s.at + c
      def fresh(m: Mode) = S(m, "", next, next)
      def begin(m: Mode) = S(m, c.toString, s.at, next)
      def keep(m: Mode) = S(m, s.buf + c, s.start, next)

      s.mode match
        case Mode.InLine =>
          if c == '\n' then
            flushedInto(s, out); oneInto(K.Newline, c, s.at, out, Channel.Trivia)
            fresh(Mode.Base)
          else keep(Mode.InLine)

        case Mode.InBlock(doc) =>
          val b = s.buf + c
          if blockEnd.exists(b.endsWith) then
            tokInto(if doc || isDoc(b) then K.Doc else K.Comment, s.copy(buf = b), out, Channel.Comment)
            fresh(Mode.Base)
          else keep(Mode.InBlock(doc || isDoc(b)))

        case Mode.InStr(q, esc, triple) =>
          if esc then keep(Mode.InStr(q, false, triple))
          // A triple-quoted string ends at the first three quotes,
          // backslash or not. That is exactly Scala's rule (there are
          // no escapes inside a triple) and only approximates Python's
          // — deliberately, because the two ways of being wrong are
          // not symmetric: closing one string early costs a few
          // mis-shaped leaves, while honouring `\"""` in a language
          // that does not would swallow the rest of the file.
          else if c == '\\' && !triple then keep(Mode.InStr(q, true, triple))
          else if c == q then
            val b = s.buf + c
            // a triple only closes on three, a plain one on the first
            if !triple then
              tokInto(K.Str, s.copy(buf = b), out); fresh(Mode.Base)
            else if b.endsWith(q.toString * 3) && b.length >= 6 then
              tokInto(K.Str, s.copy(buf = b), out); fresh(Mode.Base)
            else keep(Mode.InStr(q, false, triple))
          else keep(Mode.InStr(q, false, triple))

        case Mode.Quoting(q, n) =>
          if c == q then
            if n == 2 then keep(Mode.InStr(q, false, triple = true))
            else keep(Mode.Quoting(q, n + 1))
          else if n == 2 then
            // two quotes and no third: that was an empty string — its
            // token comes FIRST, then the fresh character is re-read
            tokInto(K.Str, s, out)
            stepInto(S(Mode.Base, "", next, s.at), c, out)
          else
            // one quote: an ordinary string, and c belongs to it
            stepInto(s.copy(mode = Mode.InStr(q, false, triple = false)), c, out)

        case Mode.Pending(at, first) =>
          val two = s"$first$c"
          if two == lang.lineComment then S(Mode.InLine, two, at, next)
          else if blockStart.contains(two) then S(Mode.InBlock(doc = false), two, at, next)
          else
            // the pending char was ordinary punctuation on its own —
            // emit it, THEN re-read c fresh, in that order
            out += Token(K.Punct, first.toString, Span(at.off, at.line, at.col, 1))
            stepInto(S(Mode.Base, "", s.at, s.at), c, out)

        case mode =>   // Base, InIdent, InWs
          // `closing` was flushed ONLY on the branches that actually
          // finish something — the two continuing-state `keep`
          // branches below (still InWs, still InIdent) must NOT flush
          // the in-progress token, so `flushedInto` sits where the
          // original tuple used `closing`, never before the match
          def closing(): Unit = flushedInto(s, out)
          def emit(t: T): S = { closing(); out += t; fresh(Mode.Base) }
          c match
            case '\n' =>
              closing(); oneInto(K.Newline, c, s.at, out, Channel.Trivia); fresh(Mode.Base)
            case _ if lang.lineComment.length == 1 && c == lang.lineComment.head =>
              closing(); S(Mode.InLine, c.toString, s.at, next)
            case _ if twoCharStarts.exists(_.head == c) =>
              closing(); S(Mode.Pending(s.at, c), "", s.at, next)
            case _ if lang.quotes(c) =>
              closing()
              if lang.triple then S(Mode.Quoting(c, 1), c.toString, s.at, next)
              else S(Mode.InStr(c, false, triple = false), c.toString, s.at, next)
            case '{' | '(' | '[' => emit(Token(K.Open, c.toString, Span(s.at.off, s.at.line, s.at.col, 1)))
            case '}' | ')' | ']' => emit(Token(K.Close, c.toString, Span(s.at.off, s.at.line, s.at.col, 1)))
            case _ if c.isWhitespace =>
              // still InWs: NO flush, the run continues
              if mode == Mode.InWs then keep(Mode.InWs) else { closing(); begin(Mode.InWs) }
            case _ if c.isLetterOrDigit || c == '_' || c == '$' =>
              // still InIdent: NO flush, the run continues
              if mode == Mode.InIdent then keep(Mode.InIdent) else { closing(); begin(Mode.InIdent) }
            case _ => emit(Token(K.Punct, c.toString, Span(s.at.off, s.at.line, s.at.col, 1)))

    def flush(s: S): Vector[T] = s.mode match
      case Mode.Pending(at, first) =>
        Vector(Token(K.Punct, first.toString, Span(at.off, at.line, at.col, 1)))
      case _ =>
        val sink = new Scan.Sink[K]
        flushedInto(s, sink)
        sink.result()

  /** the default scanner: the language this library is written in */
  val scan: Scan[K, S] = scanner(Language.scala)

  // ---------------------------------------------------------------- drive

  /**
   * The driver's state. For a brace language, `depth` and the depths
   * at which definitions are open; for an indent language, `columns`
   * holds the columns instead. `pendingDoc` is a doc comment waiting
   * to be adopted by the definition that follows it, and `lineStart`
   * says whether the next token opens a line (which is what an
   * indentation layout has to know).
   */
  /**
   * `bare` says the open definition has seen nothing but its own
   * keywords, modifiers and trivia — so the next keyword continues it
   * (`public static final class C`) instead of opening a sibling.
   * `sawIdent` is what keeps that from swallowing real siblings: an
   * identifier is allowed (it may be a modifier), but a NEWLINE after
   * one ends the chain, which is exactly what separates
   * `final class C` from an enum's `case A` / `case B`.
   */
  final case class D(depth: Int, open: List[(Int, Boolean)],
                     pendingDoc: Vector[T], lineStart: Boolean = true,
                     bare: Boolean = false, sawIdent: Boolean = false)

  val initD: D = D(0, Nil, Vector.empty)

  def driver(lang: Language): Parse.Step[K, D] = lang.layout match
    case Layout.Braces => braces
    case Layout.Indent => indent

  /** braces: a definition owns the block its keyword opened */
  private val braces: Parse.Step[K, D] = (d, t) =>
    val out = Vector.newBuilder[Instr[K]]

    t.kind match
      case K.Doc =>
        (d.copy(pendingDoc = d.pendingDoc :+ t), Vector.empty)

      // A MODIFIER CHAIN, not a new definition: nothing but keywords
      // and trivia has been seen since this one opened, so `final case
      // class C`, `public static void f` and `export function g` stay
      // ONE node. Without this the doc comment lands on an unnamed
      // node ahead of the definition it describes — which is where
      // `Member7`'s doc comment went, and how this was found.
      case K.Keyword if d.bare && d.open.headOption.exists(_._1 == d.depth) =>
        d.pendingDoc.foreach(dt => out += Instr.Emit(dt))
        out += Instr.Emit(t)
        (d.copy(pendingDoc = Vector.empty), out.result())

      case K.Keyword =>
        var s = d
        while s.open.headOption.exists(_._1 >= d.depth) do
          out += Instr.Close(None)
          s = s.copy(open = s.open.tail)
        out += Instr.Open("def", None)
        s.pendingDoc.foreach(dt => out += Instr.Emit(dt))
        out += Instr.Emit(t)
        (s.copy(open = (d.depth, false) :: s.open, pendingDoc = Vector.empty,
          bare = true, sawIdent = false), out.result())

      case K.Open =>
        d.pendingDoc.foreach(dt => out += Instr.Emit(dt))
        out += Instr.Emit(t)
        // Only a BLOCK is a definition's body. A parameter list is
        // not: `class Greeter(name: String) { … }` and Go's
        // `func (g Greeter) Hello() { … }` would otherwise end at the
        // `)`, throwing away the body and, in Go, the name.
        val marked = d.open match
          case (dep, _) :: rest if dep == d.depth && t.lexeme == "{" =>
            (dep, true) :: rest
          case other => other
        (D(d.depth + 1, marked, Vector.empty), out.result())

      case K.Close =>
        val depth = math.max(0, d.depth - 1)
        d.pendingDoc.foreach(dt => out += Instr.Emit(dt))
        out += Instr.Emit(t)
        var s = D(depth, d.open, Vector.empty)
        // a block just ended: everything opened INSIDE it ends with
        // it, brace-less definitions included, and so does the
        // definition whose own block this was
        while s.open.headOption.exists((dep, body) => dep > depth || (body && dep == depth)) do
          out += Instr.Close(None)
          s = s.copy(open = s.open.tail)
        (s, out.result())

      case K.Ws | K.Newline if d.pendingDoc.nonEmpty =>
        (d.copy(pendingDoc = d.pendingDoc :+ t), Vector.empty)

      // Modifiers and annotations standing between a doc comment and
      // the definition it belongs to — `/** … */ final case class C`,
      // `@tailrec def f`, `/** … */ pub struct S`. They are held WITH
      // the doc, so the whole run lands inside the node that opens
      // next; emitting them straight through would leave the doc
      // outside the definition it documents, which is where
      // `final case class`'s doc comment was going.
      case K.Ident | K.Punct if d.pendingDoc.nonEmpty &&
        d.pendingDoc.count(_.channel == Channel.Syntax) < 8 =>
        (d.copy(pendingDoc = d.pendingDoc :+ t), Vector.empty)

      case _ =>
        d.pendingDoc.foreach(dt => out += Instr.Emit(dt))
        out += Instr.Emit(t)
        val (stillBare, ident) = t.kind match
          case K.Newline => (d.bare && !d.sawIdent, false)   // ends a chain
          case K.Ident => (d.bare, true)                     // maybe a modifier
          case _ if t.channel != Channel.Syntax => (d.bare, d.sawIdent)
          case _ => (false, false)                           // punctuation ends it
        (d.copy(pendingDoc = Vector.empty, bare = stillBare, sawIdent = ident),
          out.result())

  /**
   * Indentation: a definition owns everything indented under it, so
   * the first token of a line at column c closes every definition
   * opened at a column >= c. The same stack YAML uses, one level up.
   */
  private val indent: Parse.Step[K, D] = (d, t) =>
    val out = Vector.newBuilder[Instr[K]]

    def dedent(to: Int, s: D): D =
      var st = s
      while st.open.headOption.exists(_._1 >= to) do
        out += Instr.Close(None)
        st = st.copy(open = st.open.tail)
      st

    t.kind match
      case K.Newline =>
        d.pendingDoc.foreach(dt => out += Instr.Emit(dt))
        out += Instr.Emit(t)
        (d.copy(pendingDoc = Vector.empty, lineStart = true), out.result())

      // leading whitespace is not yet the line's first TOKEN
      case K.Ws if d.lineStart =>
        out += Instr.Emit(t)
        (d, out.result())

      // `async def f` is one definition, as `final case class C` is
      case K.Keyword if d.bare && !d.lineStart && d.open.nonEmpty =>
        d.pendingDoc.foreach(dt => out += Instr.Emit(dt))
        out += Instr.Emit(t)
        (d.copy(pendingDoc = Vector.empty), out.result())

      case K.Keyword =>
        val col = t.span.column
        val s = if d.lineStart then dedent(col, d) else d
        out += Instr.Open("def", None)
        s.pendingDoc.foreach(dt => out += Instr.Emit(dt))
        out += Instr.Emit(t)
        (s.copy(open = (col, true) :: s.open, pendingDoc = Vector.empty,
          lineStart = false, bare = true), out.result())

      case _ =>
        val s = if d.lineStart && t.kind != K.Ws then dedent(t.span.column, d) else d
        s.pendingDoc.foreach(dt => out += Instr.Emit(dt))
        out += Instr.Emit(t)
        (s.copy(pendingDoc = Vector.empty, lineStart = false,
          bare = s.bare && t.channel != Channel.Syntax), out.result())

  /** the default driver: braces, as Scala uses */
  val step: Parse.Step[K, D] = braces

  /** whatever the driver still holds at end of input */
  val finish: D => Vector[Instr[K]] = d => d.pendingDoc.map(Instr.Emit(_))

  // ---------------------------------------------------------------- api

  /** parse a source file into definition-shaped nodes */
  def parse(text: String, snapshotEvery: Int = 64,
            lang: Language = Language.scala): Parse.Parsed[K, S, D] =
    Parse.fullWith(scanner(lang), driver(lang), initD, finish)(text, snapshotEvery)

  /** parse as the language the path names; prose falls back to `text` */
  def parseFile(path: String, text: String,
                snapshotEvery: Int = 64): Parse.Parsed[K, S, D] =
    parse(text, snapshotEvery, Language.of(path).getOrElse(Language.text))

  /** parse a source as the language its own id names — what every
   * caller downstream of a `Source` should use */
  def source(src: Source, snapshotEvery: Int = 64): Parse.Parsed[K, S, D] =
    parseFile(src.id, src.text, snapshotEvery)

  /** reparse after an edit: only the damage is re-driven */
  def reparse(old: Parse.Parsed[K, S, D], oldText: String, newText: String,
              editStart: Int, editEndOld: Int, editEndNew: Int,
              snapshotEvery: Int = 64,
              lang: Language = Language.scala): Parse.Parsed[K, S, D] =
    Parse.reparseWith(scanner(lang), driver(lang), initD, finish)(
      old, oldText, newText, editStart, editEndOld, editEndNew, snapshotEvery)
}
