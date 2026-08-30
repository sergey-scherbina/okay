package okay.codec

import okay.lex.{Channel, Scan, Span, Token}
import okay.parse.{Cst, Instr, Parse}

/**
 * The YAML dialect — the INDENTATION prover of specs/codecs.md:
 * structure lives in leading whitespace, so the instruction fold
 * carries an indent stack and dedents close frames. Total and
 * lossless like every dialect here (comments and indentation are
 * tokens in the tree; damage is an error leaf), and the semantic
 * projection lands in the SAME `Json` values — one decode algebra
 * serves JSON, CBOR and YAML alike.
 *
 * Deliberate v1 subset: block mappings (`key: value`), block
 * sequences (`- item`), nesting by indentation, plain and
 * double-quoted scalars, comments. Flow styles (`[..]`/`{..}`),
 * anchors, tags, block scalars (`|`/`>`) and multi-document streams
 * are out of scope — a weird line degrades to an error leaf, never a
 * fault.
 */
object Yaml {

  enum K:
    case Indent, Dash, Colon, Scalar, Quoted, Comment, Newline, Ws

  type T = Token[K]

  // ---------------------------------------------------------------- scan

  final case class P(off: Int, line: Int, col: Int):
    def +(c: Char): P =
      if c == '\n' then P(off + 1, line + 1, 0) else P(off + 1, line, col + 1)

  enum Mode:
    case LineStart, Plain, InComment
    case InQuote(esc: Boolean)
    /** one-char lookahead: `- item` is a Dash, `-5` is a scalar */
    case PendingDash(at: P)
    /** one-char lookahead: `key: v` is a Colon, `http://x` is not */
    case PendingColon(at: P)

  final case class S(mode: Mode, buf: String, start: P, at: P)

  val scan: Scan[K, S] = new Scan[K, S]:
    def init: S = S(Mode.LineStart, "", P(0, 0, 0), P(0, 0, 0))

    override def key(s: S): Any = (s.mode.ordinal, s.buf)

    override def rebase(s: S, offsetDelta: Int, lineDelta: Int): S =
      def shift(p: P) = P(p.off + offsetDelta, p.line + lineDelta, p.col)
      val m = s.mode match
        case Mode.PendingDash(p) => Mode.PendingDash(shift(p))
        case Mode.PendingColon(p) => Mode.PendingColon(shift(p))
        case other => other
      S(m, s.buf, shift(s.start), shift(s.at))

    private def tok(k: K, s: S, channel: Channel = Channel.Syntax): Vector[T] =
      if s.buf.isEmpty then Vector.empty
      else Vector(Token(k, s.buf,
        Span(s.start.off, s.start.line, s.start.col, s.buf.length), channel))

    private def one(k: K, c: Char, at: P, channel: Channel = Channel.Syntax): T =
      Token(k, c.toString, Span(at.off, at.line, at.col, 1), channel)

    private def flushed(s: S): Vector[T] = s.mode match
      case Mode.LineStart => tok(K.Indent, s, Channel.Trivia)
      case Mode.Plain => tok(K.Scalar, s)
      case Mode.PendingDash(_) => tok(K.Scalar, s)     // a lone trailing '-'
      case Mode.PendingColon(_) => tok(K.Scalar, s)    // 'a:' at EOF: scalar
      case Mode.InQuote(_) => tok(K.Quoted, s)         // unterminated: still a token
      case Mode.InComment => tok(K.Comment, s, Channel.Comment)

    def step(s: S, c: Char): (S, Vector[T]) =
      val next = s.at + c
      def fresh(m: Mode) = S(m, "", next, next)
      def keep(m: Mode) = S(m, s.buf + c, if s.buf.isEmpty then s.at else s.start, next)

      s.mode match
        case Mode.InComment =>
          if c == '\n' then (fresh(Mode.LineStart),
            flushed(s) :+ one(K.Newline, c, s.at, Channel.Trivia))
          else (keep(Mode.InComment), Vector.empty)
        case Mode.InQuote(esc) =>
          if esc then (keep(Mode.InQuote(false)), Vector.empty)
          else if c == '\\' then (keep(Mode.InQuote(true)), Vector.empty)
          else if c == '"' then
            val done = s.copy(buf = s.buf + c)
            (fresh(Mode.Plain), tok(K.Quoted, done))
          else (keep(Mode.InQuote(false)), Vector.empty)
        case Mode.PendingDash(dashAt) =>
          if c == ' ' then (fresh(Mode.Plain),
            Vector(one(K.Dash, '-', dashAt), one(K.Ws, c, s.at, Channel.Trivia)))
          else if c == '\n' then (fresh(Mode.LineStart),
            Vector(one(K.Dash, '-', dashAt), one(K.Newline, c, s.at, Channel.Trivia)))
          else step(S(Mode.Plain, "-", dashAt, s.at), c) match
            case (s2, ts) => (s2, ts)   // the dash was a scalar's first char
        case Mode.PendingColon(colonAt) =>
          if c == ' ' || c == '\n' then
            val scalarPart = s.copy(buf = s.buf.dropRight(1))
            val colon = one(K.Colon, ':', colonAt)
            if c == ' ' then (fresh(Mode.Plain),
              tok(K.Scalar, scalarPart) :+ colon :+ one(K.Ws, c, s.at, Channel.Trivia))
            else (fresh(Mode.LineStart),
              tok(K.Scalar, scalarPart) :+ colon :+ one(K.Newline, c, s.at, Channel.Trivia))
          else step(s.copy(mode = Mode.Plain), c)   // ':' stays in the scalar
        case mode =>   // LineStart or Plain
          c match
            case '\n' => (fresh(Mode.LineStart),
              flushed(s) :+ one(K.Newline, c, s.at, Channel.Trivia))
            case '#' => (S(Mode.InComment, "#", s.at, next), flushed(s))
            case '"' => (S(Mode.InQuote(false), "\"", s.at, next), flushed(s))
            case ':' if mode == Mode.Plain =>
              (keep(Mode.PendingColon(s.at)), Vector.empty)
            case ':' => (fresh(Mode.Plain), flushed(s) :+ one(K.Colon, c, s.at))
            case '-' if mode == Mode.LineStart || s.buf.isEmpty =>
              (S(Mode.PendingDash(s.at), "", s.at, next), flushed(s))
            case ' ' if mode == Mode.LineStart => (keep(Mode.LineStart), Vector.empty)
            case ' ' if s.buf.isEmpty =>
              (fresh(Mode.Plain), Vector(one(K.Ws, c, s.at, Channel.Trivia)))
            case _ if mode == Mode.LineStart =>
              (S(Mode.Plain, c.toString, s.at, next), flushed(s))
            case _ => (keep(Mode.Plain), Vector.empty)

    def flush(s: S): Vector[T] = s.mode match
      case Mode.PendingDash(dashAt) =>
        Vector(Token(K.Scalar, "-", Span(dashAt.off, dashAt.line, dashAt.col, 1)))
      case _ => flushed(s)

  // ---------------------------------------------------------------- drive

  private final case class Frame(indent: Int, kind: String)

  /**
   * The instruction fold: an indent stack, dedents close frames, a
   * scalar followed by a colon was a key. `- ` opens or continues a
   * sequence; content after the dash re-anchors the indent at its
   * own column, which is what makes `- key: value` nest correctly.
   */
  def instructions(tokens: IterableOnce[T]): Vector[Instr[K]] =
    val out = Vector.newBuilder[Instr[K]]
    var stack = List.empty[Frame]
    var lineIndent = 0
    var afterDash = false
    var pending: Option[T] = None        // a scalar that may turn out a key
    var pendingValue = false             // are we in value position on this line?

    def close(): Unit =
      out += Instr.Close(None)
      stack = stack.tail

    def dedentTo(ind: Int): Unit =
      while stack.nonEmpty &&
        (stack.head.indent > ind ||
          (stack.head.kind == "pair" && stack.head.indent >= ind)) do close()

    def open(kind: String, ind: Int): Unit =
      out += Instr.Open(kind, None)
      stack = Frame(ind, kind) :: stack

    def flushPendingAsValue(): Unit =
      pending.foreach(t => out += Instr.Emit(t))
      pending = None

    def content(t: T): Unit = t.kind match
      case K.Dash =>
        flushPendingAsValue()
        val ind = if afterDash then t.span.column else lineIndent
        dedentTo(ind)
        if !stack.headOption.exists(f => f.kind == "seq" && f.indent == ind) then
          open("seq", ind)
        out += Instr.Emit(t)
        afterDash = true
        pendingValue = false
      case K.Scalar | K.Quoted =>
        flushPendingAsValue()
        if pendingValue then
          // value position: emit in place, close an inline pair
          out += Instr.Emit(t)
          if stack.headOption.exists(_.kind == "pair") then close()
          pendingValue = false
        else
          pending = Some(t)   // key or bare value — the next token tells
      case K.Colon =>
        pending match
          case Some(key) =>
            val ind = if afterDash then key.span.column else lineIndent
            afterDash = false
            dedentTo(ind)
            if !stack.headOption.exists(f => f.kind == "map" && f.indent == ind) then
              open("map", ind)
            open("pair", ind)
            out += Instr.Emit(key)
            out += Instr.Emit(t)
            pending = None
            pendingValue = true
          case None =>
            out += Instr.Bad(Some(t), "colon with no key")
      case _ => out += Instr.Emit(t)   // unreachable for content kinds

    tokens.iterator.foreach { t =>
      t.kind match
        case K.Indent =>
          lineIndent = t.lexeme.length
          out += Instr.Emit(t)
        case K.Newline =>
          flushPendingAsValue()
          out += Instr.Emit(t)
          lineIndent = 0
          afterDash = false
          pendingValue = false   // a value for an open pair now means a nested block
        case K.Ws | K.Comment =>
          flushPendingAsValue()
          out += Instr.Emit(t)
        case _ => content(t)
    }
    flushPendingAsValue()
    while stack.nonEmpty do close()
    out.result()

  /** text to CST: total, lossless, indented */
  def cst(input: String): Cst[K] =
    var s = scan.init
    val toks = Vector.newBuilder[T]
    input.foreach { c =>
      val (s2, ts) = scan.step(s, c)
      toks ++= ts
      s = s2
    }
    toks ++= scan.flush(s)
    Parse.toCst(instructions(toks.result()))

  /** render = the lossless law */
  def render(c: Cst[K]): String = Cst.lexemes(c)

  // ---------------------------------------------------------------- project

  private def unquote(lexeme: String): String =
    val inner = lexeme.stripPrefix("\"").stripSuffix("\"")
    val b = new StringBuilder
    var i = 0
    while i < inner.length do
      val c = inner.charAt(i)
      if c == '\\' && i + 1 < inner.length then
        inner.charAt(i + 1) match
          case 'n' => b.append('\n'); i += 2
          case 't' => b.append('\t'); i += 2
          case 'r' => b.append('\r'); i += 2
          case x => b.append(x); i += 2
      else { b.append(c); i += 1 }
    b.toString

  private def scalar(t: T): Json = t.kind match
    case K.Quoted => Json.JStr(unquote(t.lexeme))
    case _ =>
      val s = t.lexeme.trim
      s match
        case "null" | "~" | "" => Json.JNull
        case "true" => Json.JBool(true)
        case "false" => Json.JBool(false)
        case _ => s.toDoubleOption.fold(Json.JStr(s))(Json.JNum(_))

  /** the semantic values among a node's children */
  private def values(c: Cst[K]): Vector[Json] = c match
    case Cst.Node("map", kids) =>
      Vector(Json.JObj(kids.collect {
        case p @ Cst.Node("pair", _) => pair(p)
      }.flatten))
    case Cst.Node("seq", kids) => Vector(Json.JArr(kids.flatMap(values)))
    case Cst.Node(_, kids) => kids.flatMap(values)
    case Cst.Leaf(t) => t.kind match
      case K.Scalar | K.Quoted => Vector(scalar(t))
      case _ => Vector.empty
    case Cst.Err(t, m) => Vector(Json.JErr(m + t.fold("")(x => s" at '${x.lexeme}'")))

  private def pair(p: Cst[K]): Option[(String, Json)] = p match
    case Cst.Node(_, kids) =>
      val key = kids.collectFirst {
        case Cst.Leaf(t) if t.kind == K.Scalar => t.lexeme.trim
        case Cst.Leaf(t) if t.kind == K.Quoted => unquote(t.lexeme)
      }
      val value = kids.dropWhile {
        case Cst.Leaf(t) => t.kind != K.Colon
        case _ => true
      }.drop(1).flatMap(values).headOption.getOrElse(Json.JNull)
      key.map((_, value))
    case _ => None

  /** the total pipeline: any string yields a Json (JErr for damage) */
  def parse(input: String): Json =
    values(cst(input)).headOption.getOrElse(Json.JErr("empty input"))

  /** YAML text to a value through the SAME decode algebra as JSON */
  def read[A](input: String)(using s: Schema[A]): Either[String, A] =
    Json.decode(s)(parse(input))
}
