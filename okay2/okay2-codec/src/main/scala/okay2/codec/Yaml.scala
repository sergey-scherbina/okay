package okay2.codec

import okay2.lex.{Channel, Scan, ScanInto, Span, Token}
import okay2.parse.{Cst, Instr, Parse}
import scala.collection.mutable.Growable
import scala.annotation.tailrec

/**
 * The YAML dialect (okay-codec's Yaml.scala) — the INDENTATION prover:
 * structure lives in leading whitespace, so the instruction fold
 * carries an indent stack and dedents close frames. Total and lossless
 * like every dialect here (comments and indentation are tokens in the
 * tree; damage is an error leaf), and the semantic projection lands in
 * the SAME `Json` values — one decode algebra serves JSON, CBOR and
 * YAML alike.
 *
 * Deliberate v1 subset: block mappings (`key: value`), block sequences
 * (`- item`), nesting by indentation, plain and double-quoted scalars,
 * comments. Flow styles, anchors, tags, block scalars and
 * multi-document streams are out of scope — a weird line degrades to
 * an error leaf, never a fault.
 */
object Yaml {

  sealed trait K
  object K {
    case object Indent extends K
    case object Dash extends K
    case object Colon extends K
    case object Scalar extends K
    case object Quoted extends K
    case object Comment extends K
    case object Newline extends K
    case object Ws extends K
  }

  type T = Token[K]

  // ---------------------------------------------------------------- scan

  final case class P(off: Int, line: Int, col: Int) {
    def +(c: Char): P = if (c == '\n') P(off + 1, line + 1, 0) else P(off + 1, line, col + 1)
  }

  sealed trait Mode
  object Mode {
    case object LineStart extends Mode
    case object Plain extends Mode
    case object InComment extends Mode
    final case class InQuote(esc: Boolean) extends Mode
    /** one-char lookahead: `- item` is a Dash, `-5` is a scalar */
    final case class PendingDash(at: P) extends Mode
    /** one-char lookahead: `key: v` is a Colon, `http://x` is not */
    final case class PendingColon(at: P) extends Mode
  }

  final case class S(mode: Mode, buf: String, start: P, at: P)

  /** the mode without its positions, as Scala 3's enum ordinal: what a
   * relex compares, so a shifted position is still the same state */
  private def ordinal(m: Mode): Int = m match {
    case Mode.LineStart => 0
    case Mode.Plain => 1
    case Mode.InComment => 2
    case Mode.InQuote(_) => 3
    case Mode.PendingDash(_) => 4
    case Mode.PendingColon(_) => 5
  }

  val scan: Scan[K, S] = new ScanInto[K, S] {
    def init: S = S(Mode.LineStart, "", P(0, 0, 0), P(0, 0, 0))

    override def key(s: S): Any = (ordinal(s.mode), s.buf)

    override def rebase(s: S, offsetDelta: Int, lineDelta: Int): S = {
      def shift(p: P) = P(p.off + offsetDelta, p.line + lineDelta, p.col)
      val m = s.mode match {
        case Mode.PendingDash(p) => Mode.PendingDash(shift(p))
        case Mode.PendingColon(p) => Mode.PendingColon(shift(p))
        case other => other
      }
      S(m, s.buf, shift(s.start), shift(s.at))
    }

    private def tokInto(k: K, s: S, out: Growable[T], channel: Channel = Channel.Syntax): Unit =
      if (s.buf.nonEmpty) out += Token(k, s.buf, Span(s.start.off, s.start.line, s.start.col, s.buf.length), channel): Unit

    private def oneInto(k: K, c: Char, at: P, out: Growable[T], channel: Channel = Channel.Syntax): Unit =
      out += Token(k, c.toString, Span(at.off, at.line, at.col, 1), channel): Unit

    private def flushedInto(s: S, out: Growable[T]): Unit = s.mode match {
      case Mode.LineStart => tokInto(K.Indent, s, out, Channel.Trivia)
      case Mode.Plain => tokInto(K.Scalar, s, out)
      case Mode.PendingDash(_) => tokInto(K.Scalar, s, out) // a lone trailing '-'
      case Mode.PendingColon(_) => tokInto(K.Scalar, s, out) // 'a:' at EOF: scalar
      case Mode.InQuote(_) => tokInto(K.Quoted, s, out) // unterminated: still a token
      case Mode.InComment => tokInto(K.Comment, s, out, Channel.Comment)
    }

    @tailrec override def stepInto(s: S, c: Char, out: Growable[T]): S = {
      val next = s.at + c
      def fresh(m: Mode) = S(m, "", next, next)
      def keep(m: Mode) = S(m, s.buf + c, if (s.buf.isEmpty) s.at else s.start, next)

      s.mode match {
        case Mode.InComment =>
          if (c == '\n') {
            flushedInto(s, out); oneInto(K.Newline, c, s.at, out, Channel.Trivia)
            fresh(Mode.LineStart)
          } else keep(Mode.InComment)
        case Mode.InQuote(esc) =>
          if (esc) keep(Mode.InQuote(false))
          else if (c == '\\') keep(Mode.InQuote(true))
          else if (c == '"') {
            tokInto(K.Quoted, s.copy(buf = s.buf + c), out)
            fresh(Mode.Plain)
          } else keep(Mode.InQuote(false))
        case Mode.PendingDash(dashAt) =>
          if (c == ' ') {
            oneInto(K.Dash, '-', dashAt, out); oneInto(K.Ws, c, s.at, out, Channel.Trivia)
            fresh(Mode.Plain)
          } else if (c == '\n') {
            oneInto(K.Dash, '-', dashAt, out); oneInto(K.Newline, c, s.at, out, Channel.Trivia)
            fresh(Mode.LineStart)
          }
          // the dash was a scalar's first char — the SAME character is
          // re-processed under Plain
          else stepInto(S(Mode.Plain, "-", dashAt, s.at), c, out)
        case Mode.PendingColon(colonAt) =>
          if (c == ' ' || c == '\n') {
            tokInto(K.Scalar, s.copy(buf = s.buf.dropRight(1)), out)
            oneInto(K.Colon, ':', colonAt, out)
            if (c == ' ') {
              oneInto(K.Ws, c, s.at, out, Channel.Trivia)
              fresh(Mode.Plain)
            } else {
              oneInto(K.Newline, c, s.at, out, Channel.Trivia)
              fresh(Mode.LineStart)
            }
          } else stepInto(s.copy(mode = Mode.Plain), c, out) // ':' stays in the scalar
        case mode => // LineStart or Plain
          c match {
            case '\n' =>
              flushedInto(s, out); oneInto(K.Newline, c, s.at, out, Channel.Trivia)
              fresh(Mode.LineStart)
            case '#' => flushedInto(s, out); S(Mode.InComment, "#", s.at, next)
            case '"' => flushedInto(s, out); S(Mode.InQuote(false), "\"", s.at, next)
            case ':' if mode == Mode.Plain => keep(Mode.PendingColon(s.at))
            case ':' => flushedInto(s, out); oneInto(K.Colon, c, s.at, out); fresh(Mode.Plain)
            case '-' if mode == Mode.LineStart || s.buf.isEmpty =>
              flushedInto(s, out); S(Mode.PendingDash(s.at), "", s.at, next)
            case ' ' if mode == Mode.LineStart => keep(Mode.LineStart)
            case ' ' if s.buf.isEmpty =>
              oneInto(K.Ws, c, s.at, out, Channel.Trivia); fresh(Mode.Plain)
            case _ if mode == Mode.LineStart =>
              flushedInto(s, out); S(Mode.Plain, c.toString, s.at, next)
            case _ => keep(Mode.Plain)
          }
      }
    }

    def flush(s: S): Vector[T] = s.mode match {
      case Mode.PendingDash(dashAt) =>
        Vector(Token(K.Scalar, "-", Span(dashAt.off, dashAt.line, dashAt.col, 1)))
      case _ =>
        val sink = new Scan.Sink[K]
        flushedInto(s, sink)
        sink.result()
    }
  }

  // ---------------------------------------------------------------- drive

  private final case class Frame(indent: Int, kind: String)

  /**
   * The instruction fold: an indent stack, dedents close frames, a
   * scalar followed by a colon was a key. `- ` opens or continues a
   * sequence; content after the dash re-anchors the indent at its own
   * column, which is what makes `- key: value` nest correctly.
   */
  def instructions(tokens: IterableOnce[T]): Vector[Instr[K]] = {
    val out = Vector.newBuilder[Instr[K]]
    var stack = List.empty[Frame]
    var lineIndent = 0
    var afterDash = false
    var pending: Option[T] = None // a scalar that may turn out a key
    var pendingValue = false // are we in value position on this line?

    def close(): Unit = {
      out += Instr.Close[K](None)
      stack = stack.tail
    }

    def dedentTo(ind: Int): Unit =
      while (stack.nonEmpty &&
        (stack.head.indent > ind || (stack.head.kind == "pair" && stack.head.indent >= ind))) close()

    def open(kind: String, ind: Int): Unit = {
      out += Instr.Open[K](kind, None)
      stack = Frame(ind, kind) :: stack
    }

    def flushPendingAsValue(): Unit = {
      pending.foreach(t => out += Instr.Emit(t))
      pending = None
    }

    def content(t: T): Unit = t.kind match {
      case K.Dash =>
        flushPendingAsValue()
        val ind = if (afterDash) t.span.column else lineIndent
        dedentTo(ind)
        if (!stack.headOption.exists(f => f.kind == "seq" && f.indent == ind)) open("seq", ind)
        out += Instr.Emit(t)
        afterDash = true
        pendingValue = false
      case K.Scalar | K.Quoted =>
        flushPendingAsValue()
        if (pendingValue) {
          // value position: emit in place, close an inline pair
          out += Instr.Emit(t)
          if (stack.headOption.exists(_.kind == "pair")) close()
          pendingValue = false
        } else pending = Some(t) // key or bare value — the next token tells
      case K.Colon =>
        pending match {
          case Some(key) =>
            val ind = if (afterDash) key.span.column else lineIndent
            afterDash = false
            dedentTo(ind)
            if (!stack.headOption.exists(f => f.kind == "map" && f.indent == ind)) open("map", ind)
            open("pair", ind)
            out += Instr.Emit(key)
            out += Instr.Emit(t)
            pending = None
            pendingValue = true
          case None =>
            out += Instr.Bad(Some(t), "colon with no key")
        }
      case _ => out += Instr.Emit(t): Unit // unreachable for content kinds
    }

    tokens.iterator.foreach { t =>
      t.kind match {
        case K.Indent =>
          lineIndent = t.lexeme.length
          out += Instr.Emit(t)
        case K.Newline =>
          flushPendingAsValue()
          out += Instr.Emit(t)
          lineIndent = 0
          afterDash = false
          pendingValue = false // a value for an open pair now means a nested block
        case K.Ws | K.Comment =>
          flushPendingAsValue()
          out += Instr.Emit(t)
        case _ => content(t)
      }
    }
    flushPendingAsValue()
    while (stack.nonEmpty) close()
    out.result()
  }

  /** text to CST: total, lossless, indented */
  def cst(input: String): Cst[K] = {
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

  /** render = the lossless law */
  def render(c: Cst[K]): String = Cst.lexemes(c)

  // ---------------------------------------------------------------- project

  private def unquote(lexeme: String): String = {
    val inner = lexeme.stripPrefix("\"").stripSuffix("\"")
    val b = new StringBuilder
    var i = 0
    while (i < inner.length) {
      val c = inner.charAt(i)
      if (c == '\\' && i + 1 < inner.length) {
        inner.charAt(i + 1) match {
          case 'n' => b.append('\n')
          case 't' => b.append('\t')
          case 'r' => b.append('\r')
          case x => b.append(x)
        }
        i += 2
      } else { b.append(c); i += 1 }
    }
    b.toString
  }

  private def scalar(t: T): Json = t.kind match {
    case K.Quoted => Json.JStr(unquote(t.lexeme))
    case _ =>
      t.lexeme.trim match {
        case "null" | "~" | "" => Json.JNull
        case "true" => Json.JBool(true)
        case "false" => Json.JBool(false)
        case s => s.toDoubleOption.fold[Json](Json.JStr(s))(Json.JNum(_))
      }
  }

  /** a step of `values`' walk */
  private sealed trait Task
  private object Task {
    final case class Eval(c: Cst[K]) extends Task
    /** the last n results, concatenated */
    final case class Flatten(n: Int) extends Task
    /** the last n results, concatenated, as one array */
    final case class Arr(n: Int) extends Task
    /** one result per pair, each the pair's value candidates */
    final case class Obj(keys: Vector[Option[String]]) extends Task
  }

  /**
   * The semantic values among a node's children, on an EXPLICIT stack:
   * YAML nests by indentation, so `cst` builds a tree as deep as its
   * input, and a projection that recursed per level would overflow on
   * one the builder had just built. Post-order in two stacks: `work`
   * holds what is still to do, `done` each evaluated node's values,
   * newest first.
   */
  private def values(root: Cst[K]): Vector[Json] = {
    var work: List[Task] = List(Task.Eval(root))
    var done: List[Vector[Json]] = Nil
    def take(n: Int): Vector[Vector[Json]] = {
      val out = done.take(n).reverse.toVector
      done = done.drop(n)
      out
    }
    while (work.nonEmpty) {
      val t = work.head
      work = work.tail
      t match {
        case Task.Eval(c) => c match {
          case Cst.Node("map", kids) =>
            // each pair: its key read here, its value the first value
            // among the children after the colon
            val pairs = kids.collect { case Cst.Node("pair", pk) => pk }
            var w: List[Task] = Task.Obj(pairs.map(keyOf)) :: work
            for (pk <- pairs.reverseIterator) {
              val after = afterColon(pk)
              w = Task.Flatten(after.length) :: w
              for (k <- after.reverseIterator) w = Task.Eval(k) :: w
            }
            work = w
          case Cst.Node("seq", kids) =>
            work = kids.foldRight[List[Task]](Task.Arr(kids.length) :: work)((k, w) => Task.Eval(k) :: w)
          case Cst.Node(_, kids) =>
            work = kids.foldRight[List[Task]](Task.Flatten(kids.length) :: work)((k, w) => Task.Eval(k) :: w)
          case Cst.Leaf(tok) => tok.kind match {
            case K.Scalar | K.Quoted => done = Vector(scalar(tok)) :: done
            case _ => done = Vector.empty[Json] :: done
          }
          case Cst.Err(tok, m) => done = Vector[Json](Json.JErr(m + tok.fold("")(x => s" at '${x.lexeme}'"))) :: done
        }
        case Task.Flatten(n) => done = take(n).flatten :: done
        case Task.Arr(n) => done = Vector[Json](Json.JArr(take(n).flatten)) :: done
        case Task.Obj(keys) =>
          val vals = take(keys.length)
          done = Vector[Json](Json.JObj(keys.zip(vals).flatMap { case (k, v) =>
            k.map(name => (name, v.headOption.getOrElse(Json.JNull)))
          })) :: done
      }
    }
    done.headOption.getOrElse(Vector.empty)
  }

  /** a pair's key: its first scalar, unquoted */
  private def keyOf(kids: Vector[Cst[K]]): Option[String] =
    kids.collectFirst {
      case Cst.Leaf(t) if t.kind == K.Scalar => t.lexeme.trim
      case Cst.Leaf(t) if t.kind == K.Quoted => unquote(t.lexeme)
    }

  /** a pair's children after its colon: where its value is */
  private def afterColon(kids: Vector[Cst[K]]): Vector[Cst[K]] =
    kids.dropWhile {
      case Cst.Leaf(t) => t.kind != K.Colon
      case _ => true
    }.drop(1)

  /** the total pipeline: any string yields a Json (JErr for damage) */
  def parse(input: String): Json =
    values(cst(input)).headOption.getOrElse(Json.JErr("empty input"))

  /** YAML text to a value through the SAME decode algebra as JSON */
  def read[A](input: String)(implicit s: Schema[A]): Either[String, A] =
    Json.decode(s)(parse(input))
}
