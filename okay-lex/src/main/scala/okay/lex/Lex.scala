package okay.lex

import okay.{!, %, +, Chunk, Chunks, Stage, Writer, pure}
import okay.given

/** an exact source position; length in chars */
final case class Span(offset: Int, line: Int, column: Int, length: Int)

/** what kind of material a token is; errors are a CHANNEL, not a fault */
enum Channel:
  case Syntax, Trivia, Comment, Embedded, Error

final case class Token[+K](kind: K, lexeme: String, span: Span,
                           channel: Channel = Channel.Syntax)

/**
 * A lexer as a pure step function (specs/streaming-lex.md): the state
 * S is a VALUE — it crosses chunk boundaries, snapshots for
 * incremental relexing, and never hides mutation. TOTAL by design:
 * every character goes somewhere, the unrecognizable into
 * Error-channel tokens; flush finishes whatever the state holds at
 * the end of input (an unterminated string still becomes a token).
 */
trait Scan[K, S]:
  def init: S

  /** consume one character; emit zero or more finished tokens */
  def step(s: S, c: Char): (S, Vector[Token[K]])

  /** end of input: finish the tail */
  def flush(s: S): Vector[Token[K]]

  /** a position-erased fingerprint of the state, for reconvergence
   * (override when S carries absolute positions) */
  def key(s: S): Any = s

  /** shift the positions inside a state (override when S carries them;
   * the column never shifts — reconvergence happens past a newline) */
  def rebase(s: S, offsetDelta: Int, lineDelta: Int): S = s

object Scan {

  /** the scanner as a pipeline Stage: awaits chars, tells tokens,
   * answers its final state */
  def stage[K, S](sc: Scan[K, S]): Stage[Char, Token[K], S] =
    def tellAll(ts: Vector[Token[K]], then_ : => Stage[Char, Token[K], S]): Stage[Char, Token[K], S] =
      ts.foldRight(then_)((t, rest) => Stage.tell[Char, Token[K]](t).flatMap(_ => rest))

    def go(s: S): Stage[Char, Token[K], S] =
      Stage.await[Char, Token[K]].flatMap {
        case Some(c) =>
          val (s2, ts) = sc.step(s, c)
          tellAll(ts, go(s2))
        case None => tellAll(sc.flush(s), pure(s))
      }

    go(sc.init)

  /** everything lexed at once, with the snapshots incremental
   * relexing resumes from */
  final case class Lexed[K, S](tokens: Vector[Token[K]],
                               snapshots: Vector[(Int, S)], state: S)

  /** lex a whole string, snapshotting the state every snapshotEvery chars */
  def all[K, S](sc: Scan[K, S])(input: String, snapshotEvery: Int = 64): Lexed[K, S] =
    var s = sc.init
    val tokens = Vector.newBuilder[Token[K]]
    val snaps = Vector.newBuilder[(Int, S)]
    var i = 0
    while i < input.length do
      if i % snapshotEvery == 0 then snaps += ((i, s))
      val (s2, ts) = sc.step(s, input.charAt(i))
      tokens ++= ts
      s = s2
      i += 1
    tokens ++= sc.flush(s)
    Lexed(tokens.result(), snaps.result(), s)

  /**
   * Incremental relexing: resume from the nearest snapshot at or
   * before the edit, lex forward, and RECONVERGE — once past the edit
   * and past the next newline, a state equal to the old run's state
   * at the corresponding old offset means everything beyond is the
   * old tokens, reused with shifted spans (offset by the length
   * delta, line by the newline delta; the newline requirement keeps
   * columns exact). No convergence found relexes to the end — never
   * wrong, at worst not incremental.
   */
  def relex[K, S](sc: Scan[K, S])(old: Lexed[K, S], oldInput: String, newInput: String,
                                  editStart: Int, editEndOld: Int, editEndNew: Int,
                                  snapshotEvery: Int = 64): Lexed[K, S] =
    val delta = newInput.length - oldInput.length
    val lineDelta =
      newInput.substring(editStart, editEndNew).count(_ == '\n') -
        oldInput.substring(editStart, editEndOld).count(_ == '\n')
    val base = old.snapshots.filter(_._1 <= editStart).lastOption.getOrElse((0, sc.init))
    val nlAfterOld = oldInput.indexOf('\n', editEndOld)
    val oldStates = old.snapshots.toMap

    // tokens strictly before the resume point are kept as they are
    val keep = old.tokens.takeWhile(t => t.span.offset + t.span.length <= base._1)

    var s = base._2
    val fresh = Vector.newBuilder[Token[K]]
    val snaps = Vector.newBuilder[(Int, S)]
    snaps ++= old.snapshots.takeWhile(_._1 < base._1)
    var i = base._1
    while i < newInput.length do
      val oldOff = i - delta
      if i % snapshotEvery == 0 then snaps += ((i, s))
      if nlAfterOld >= 0 && oldOff > nlAfterOld
        && oldStates.get(oldOff).exists(st => sc.key(st) == sc.key(s)) then
        // reconverged: reuse the old tail, spans shifted
        val tail = old.tokens.dropWhile(t => t.span.offset + t.span.length <= oldOff).map(t =>
          t.copy(span = t.span.copy(offset = t.span.offset + delta,
            line = t.span.line + lineDelta)))
        return Lexed(keep ++ fresh.result() ++ tail,
          snaps.result() ++ old.snapshots.dropWhile(_._1 < oldOff)
            .map((o, st) => (o + delta, sc.rebase(st, delta, lineDelta))),
          old.state)
      val (s2, ts) = sc.step(s, newInput.charAt(i))
      fresh ++= ts
      s = s2
      i += 1
    fresh ++= sc.flush(s)
    Lexed(keep ++ fresh.result(), snaps.result(), s)
}
