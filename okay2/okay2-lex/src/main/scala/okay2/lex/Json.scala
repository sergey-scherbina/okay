package okay2.lex

import scala.collection.mutable.Growable

/**
 * The proving dialect: a total JSON scanner — okay-lex's Json.scala.
 * Every character lands in a token — structure on Syntax, whitespace on
 * Trivia, anything unrecognizable on Error — and the concatenated
 * lexemes of ALL channels reproduce the input byte for byte (the
 * lossless law). Unterminated strings and malformed words become
 * Error-channel tokens at flush, never exceptions.
 */
object Json {

  sealed trait K
  object K {
    case object LBrace extends K
    case object RBrace extends K
    case object LBracket extends K
    case object RBracket extends K
    case object Colon extends K
    case object Comma extends K
    case object Str extends K
    case object Num extends K
    case object Bool extends K
    case object Null extends K
    case object Ws extends K
    case object Bad extends K
  }

  sealed trait Mode
  object Mode {
    case object Base extends Mode
    final case class InStr(esc: Boolean) extends Mode
    case object InNum extends Mode
    case object InWord extends Mode
    case object InWs extends Mode
  }

  /** the scanner's state, FLAT: positions as ints, not a case class
   * built per character (lexer-state-allocation in the Scala 3 core) */
  final case class S(mode: Mode, buf: String,
                     startOff: Int, startLine: Int, startCol: Int,
                     curOff: Int, curLine: Int, curCol: Int)

  val scan: Scan[K, S] = new ScanInto[K, S] {
    def init: S = S(Mode.Base, "", 0, 0, 0, 0, 0, 0)

    override def key(s: S): Any = (s.mode, s.buf)

    override def rebase(s: S, offsetDelta: Int, lineDelta: Int): S =
      s.copy(startOff = s.startOff + offsetDelta, startLine = s.startLine + lineDelta,
             curOff = s.curOff + offsetDelta, curLine = s.curLine + lineDelta)

    /** the state with `cur` advanced over c and c appended */
    private def eat(s: S, c: Char): S =
      if (c == '\n') S(s.mode, s.buf + c, s.startOff, s.startLine, s.startCol, s.curOff + 1, s.curLine + 1, 0)
      else S(s.mode, s.buf + c, s.startOff, s.startLine, s.startCol, s.curOff + 1, s.curLine, s.curCol + 1)

    /** a fresh state starting AT the current position, having eaten c */
    private def start(mode: Mode, buf: String, s: S, c: Char): S =
      if (c == '\n') S(mode, buf, s.curOff, s.curLine, s.curCol, s.curOff + 1, s.curLine + 1, 0)
      else S(mode, buf, s.curOff, s.curLine, s.curCol, s.curOff + 1, s.curLine, s.curCol + 1)

    /** Base at the position AFTER c */
    private def based(s: S, c: Char): S =
      if (c == '\n') S(Mode.Base, "", s.curOff + 1, s.curLine + 1, 0, s.curOff + 1, s.curLine + 1, 0)
      else S(Mode.Base, "", s.curOff + 1, s.curLine, s.curCol + 1, s.curOff + 1, s.curLine, s.curCol + 1)

    private def tok(kind: K, s: S, channel: Channel = Channel.Syntax): Token[K] =
      Token(kind, s.buf, Span(s.startOff, s.startLine, s.startCol, s.buf.length), channel)

    private def one(kind: K, c: Char, s: S, channel: Channel = Channel.Syntax): Token[K] =
      Token(kind, c.toString, Span(s.curOff, s.curLine, s.curCol, 1), channel)

    /** finish the pending token, if any, INTO the sink */
    private def finishInto(s: S, out: Growable[Token[K]]): Unit = s.mode match {
      case Mode.Base => ()
      case Mode.InStr(_) => out += tok(K.Str, s, channel = Channel.Error) // unterminated
      case Mode.InNum => out += tok(K.Num, s)
      case Mode.InWs => out += tok(K.Ws, s, channel = Channel.Trivia)
      case Mode.InWord => s.buf match {
        case "true" | "false" => out += tok(K.Bool, s)
        case "null" => out += tok(K.Null, s)
        case _ => out += tok(K.Bad, s, channel = Channel.Error)
      }
    }

    private def isWs(c: Char): Boolean = c == ' ' || c == '\t' || c == '\n' || c == '\r'

    override def stepInto(s: S, c: Char, out: Growable[Token[K]]): S = s.mode match {
      case Mode.InStr(esc) =>
        val s2 = eat(s, c)
        if (esc) s2.copy(mode = Mode.InStr(false))
        else if (c == '\\') s2.copy(mode = Mode.InStr(true))
        else if (c == '"') {
          out += tok(K.Str, s2)
          S(Mode.Base, "", s2.curOff, s2.curLine, s2.curCol, s2.curOff, s2.curLine, s2.curCol)
        } else s2

      case Mode.InNum if c.isDigit || "+-.eE".contains(c) => eat(s, c)

      case Mode.InWord if c.isLetter => eat(s, c)

      case Mode.InWs if isWs(c) => eat(s, c)

      case _ =>
        // the pending token (if any) ends here; c starts fresh in Base
        finishInto(s, out)
        c match {
          case '{' | '}' | '[' | ']' | ':' | ',' =>
            val kind: K = c match {
              case '{' => K.LBrace
              case '}' => K.RBrace
              case '[' => K.LBracket
              case ']' => K.RBracket
              case ':' => K.Colon
              case _ => K.Comma
            }
            out += one(kind, c, s)
            based(s, c)
          case '"' => start(Mode.InStr(false), "\"", s, c)
          case d if d.isDigit || d == '-' => start(Mode.InNum, c.toString, s, c)
          case l if l.isLetter => start(Mode.InWord, c.toString, s, c)
          case w if isWs(w) => start(Mode.InWs, c.toString, s, c)
          case _ =>
            out += one(K.Bad, c, s, channel = Channel.Error)
            based(s, c)
        }
    }

    /** the Base arm stays allocation-free: `relex` asks "is anything
     * half-built?" once per character */
    def flush(s: S): Vector[Token[K]] = s.mode match {
      case Mode.Base => Vector.empty
      case _ =>
        val sink = new Scan.Sink[K]
        finishInto(s, sink)
        sink.result()
    }
  }
}
