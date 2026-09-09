package okay.lex

/**
 * The proving dialect: a total JSON scanner. Every character lands in
 * a token — structure on Syntax, whitespace on Trivia, anything
 * unrecognizable on Error — and the concatenated lexemes of ALL
 * channels reproduce the input byte for byte (the lossless law).
 * Unterminated strings and malformed words become Error-channel
 * tokens at flush, never exceptions.
 */
object Json {

  enum K:
    case LBrace, RBrace, LBracket, RBracket, Colon, Comma
    case Str, Num, Bool, Null, Ws, Bad

  enum Mode:
    case Base
    case InStr(esc: Boolean)
    case InNum
    case InWord
    case InWs

  /**
   * The scanner's state, FLAT (lexer-state-allocation, 2026-09-09).
   *
   * It used to carry two `P(off, line, col)` case classes, and `step`
   * built a fresh one per character (`s.cur + c`) — 32 bytes each, on
   * a path that already allocates a new `S` and the `Tuple2` `step`
   * answers. Lexing measured ~180 B per input CHARACTER on both
   * paths (docs/benchmarks.md §10); this removes the positions'
   * share of it without changing what anything observes.
   */
  final case class S(mode: Mode, buf: String,
                     startOff: Int, startLine: Int, startCol: Int,
                     curOff: Int, curLine: Int, curCol: Int)

  val scan: Scan[K, S] = new Scan[K, S]:
    def init: S = S(Mode.Base, "", 0, 0, 0, 0, 0, 0)

    override def key(s: S): Any = (s.mode, s.buf)

    override def rebase(s: S, offsetDelta: Int, lineDelta: Int): S =
      s.copy(startOff = s.startOff + offsetDelta, startLine = s.startLine + lineDelta,
             curOff = s.curOff + offsetDelta, curLine = s.curLine + lineDelta)

    /** the state with `cur` advanced over c and c appended */
    private def eat(s: S, c: Char): S =
      if c == '\n' then S(s.mode, s.buf + c, s.startOff, s.startLine, s.startCol,
                          s.curOff + 1, s.curLine + 1, 0)
      else S(s.mode, s.buf + c, s.startOff, s.startLine, s.startCol,
             s.curOff + 1, s.curLine, s.curCol + 1)

    /** a fresh state starting AT the current position, having eaten c */
    private def start(mode: Mode, buf: String, s: S, c: Char): S =
      if c == '\n' then S(mode, buf, s.curOff, s.curLine, s.curCol, s.curOff + 1, s.curLine + 1, 0)
      else S(mode, buf, s.curOff, s.curLine, s.curCol, s.curOff + 1, s.curLine, s.curCol + 1)

    /** Base at the position AFTER c */
    private def based(s: S, c: Char): S =
      if c == '\n' then S(Mode.Base, "", s.curOff + 1, s.curLine + 1, 0, s.curOff + 1, s.curLine + 1, 0)
      else S(Mode.Base, "", s.curOff + 1, s.curLine, s.curCol + 1, s.curOff + 1, s.curLine, s.curCol + 1)

    private def tok(kind: K, s: S,
                    channel: Channel = Channel.Syntax): Token[K] =
      Token(kind, s.buf, Span(s.startOff, s.startLine, s.startCol, s.buf.length), channel)

    private def one(kind: K, c: Char, s: S): Token[K] =
      Token(kind, c.toString, Span(s.curOff, s.curLine, s.curCol, 1))

    /** finish the pending token, if any */
    private def finish(s: S): Vector[Token[K]] = s.mode match
      case Mode.Base => Vector.empty
      case Mode.InStr(_) => Vector(tok(K.Str, s, channel = Channel.Error)) // unterminated
      case Mode.InNum => Vector(tok(K.Num, s))
      case Mode.InWs => Vector(tok(K.Ws, s, channel = Channel.Trivia))
      case Mode.InWord => s.buf match
        case "true" | "false" => Vector(tok(K.Bool, s))
        case "null" => Vector(tok(K.Null, s))
        case _ => Vector(tok(K.Bad, s, channel = Channel.Error))

    def step(s: S, c: Char): (S, Vector[Token[K]]) = s.mode match
      case Mode.InStr(esc) =>
        val s2 = eat(s, c)
        if esc then (s2.copy(mode = Mode.InStr(false)), Vector.empty)
        else if c == '\\' then (s2.copy(mode = Mode.InStr(true)), Vector.empty)
        else if c == '"' then
          (S(Mode.Base, "", s2.curOff, s2.curLine, s2.curCol,
             s2.curOff, s2.curLine, s2.curCol), Vector(tok(K.Str, s2)))
        else (s2, Vector.empty)

      case Mode.InNum if c.isDigit || "+-.eE".contains(c) => (eat(s, c), Vector.empty)

      case Mode.InWord if c.isLetter => (eat(s, c), Vector.empty)

      case Mode.InWs if c == ' ' || c == '\t' || c == '\n' || c == '\r' =>
        (eat(s, c), Vector.empty)

      case _ =>
        // the pending token (if any) ends here; c starts fresh in Base
        val done = finish(s)
        val next = c match
          case '{' | '}' | '[' | ']' | ':' | ',' =>
            val kind = c match
              case '{' => K.LBrace
              case '}' => K.RBrace
              case '[' => K.LBracket
              case ']' => K.RBracket
              case ':' => K.Colon
              case _ => K.Comma
            return (based(s, c), done :+ one(kind, c, s))
          case '"' => start(Mode.InStr(false), "\"", s, c)
          case d if d.isDigit || d == '-' => start(Mode.InNum, c.toString, s, c)
          case l if l.isLetter => start(Mode.InWord, c.toString, s, c)
          case w if w == ' ' || w == '\t' || w == '\n' || w == '\r' =>
            start(Mode.InWs, c.toString, s, c)
          case _ =>
            return (based(s, c), done :+ one(K.Bad, c, s).copy(channel = Channel.Error))
        (next, done)

    def flush(s: S): Vector[Token[K]] = finish(s)
}
