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

  /** an absolute position */
  final case class P(off: Int, line: Int, col: Int):
    def +(c: Char): P =
      if c == '\n' then P(off + 1, line + 1, 0) else P(off + 1, line, col + 1)

  enum Mode:
    case Base
    case InStr(esc: Boolean)
    case InNum
    case InWord
    case InWs

  final case class S(mode: Mode, buf: String, start: P, cur: P)

  val scan: Scan[K, S] = new Scan[K, S]:
    def init: S = S(Mode.Base, "", P(0, 0, 0), P(0, 0, 0))

    override def key(s: S): Any = (s.mode, s.buf)

    override def rebase(s: S, offsetDelta: Int, lineDelta: Int): S =
      def shift(p: P) = P(p.off + offsetDelta, p.line + lineDelta, p.col)
      s.copy(start = shift(s.start), cur = shift(s.cur))

    private def tok(kind: K, s: S, extra: Int = 0,
                    channel: Channel = Channel.Syntax): Token[K] =
      Token(kind, s.buf, Span(s.start.off, s.start.line, s.start.col,
        s.buf.length), channel)

    private def one(kind: K, c: Char, at: P): Token[K] =
      Token(kind, c.toString, Span(at.off, at.line, at.col, 1))

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
        val s2 = s.copy(buf = s.buf + c, cur = s.cur + c)
        if esc then (s2.copy(mode = Mode.InStr(false)), Vector.empty)
        else if c == '\\' then (s2.copy(mode = Mode.InStr(true)), Vector.empty)
        else if c == '"' then
          (S(Mode.Base, "", s2.cur, s2.cur), Vector(tok(K.Str, s2)))
        else (s2, Vector.empty)

      case Mode.InNum if c.isDigit || "+-.eE".contains(c) =>
        (s.copy(buf = s.buf + c, cur = s.cur + c), Vector.empty)

      case Mode.InWord if c.isLetter =>
        (s.copy(buf = s.buf + c, cur = s.cur + c), Vector.empty)

      case Mode.InWs if c == ' ' || c == '\t' || c == '\n' || c == '\r' =>
        (s.copy(buf = s.buf + c, cur = s.cur + c), Vector.empty)

      case _ =>
        // the pending token (if any) ends here; c starts fresh in Base
        val done = finish(s)
        val at = s.cur
        val next = c match
          case '{' | '}' | '[' | ']' | ':' | ',' =>
            val kind = c match
              case '{' => K.LBrace
              case '}' => K.RBrace
              case '[' => K.LBracket
              case ']' => K.RBracket
              case ':' => K.Colon
              case _ => K.Comma
            return (S(Mode.Base, "", at + c, at + c), done :+ one(kind, c, at))
          case '"' => S(Mode.InStr(false), "\"", at, at + c)
          case d if d.isDigit || d == '-' => S(Mode.InNum, c.toString, at, at + c)
          case l if l.isLetter => S(Mode.InWord, c.toString, at, at + c)
          case w if w == ' ' || w == '\t' || w == '\n' || w == '\r' =>
            S(Mode.InWs, c.toString, at, at + c)
          case _ =>
            return (S(Mode.Base, "", at + c, at + c),
              done :+ one(K.Bad, c, at).copy(channel = Channel.Error))
        (next, done)

    def flush(s: S): Vector[Token[K]] = finish(s)
}
