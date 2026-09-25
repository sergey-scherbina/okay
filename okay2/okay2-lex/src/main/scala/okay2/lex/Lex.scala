package okay2.lex

import scala.annotation.unused
import scala.collection.mutable.Growable
import okay2._
import okay2.stream.{ChunkBuf, Chunks, Stage}

/** an exact source position; length in chars */
final case class Span(offset: Int, line: Int, column: Int, length: Int)

/** what kind of material a token is; errors are a CHANNEL, not a fault */
sealed trait Channel
object Channel {
  case object Syntax extends Channel
  case object Trivia extends Channel
  case object Comment extends Channel
  case object Embedded extends Channel
  case object Error extends Channel
}

final case class Token[+K](kind: K, lexeme: String, span: Span, channel: Channel = Channel.Syntax)

/**
 * A lexer as a pure step function — okay-lex's Lex.scala (okay2-lex-parse;
 * specs/streaming-lex.md): the state S is a VALUE — it crosses chunk
 * boundaries, snapshots for incremental relexing, and never hides
 * mutation. TOTAL by design: every character goes somewhere, the
 * unrecognizable into Error-channel tokens; flush finishes whatever the
 * state holds at the end of input.
 */
trait Scan[K, S] {
  def init: S

  /** consume one character; emit zero or more finished tokens */
  def step(s: S, c: Char): (S, Vector[Token[K]])

  /** the same step, WRITING what it finished into a sink: no `Tuple2`
   * per character, no `Vector` per token (−29% in the Scala 3 core,
   * docs/benchmarks.md §10). Additive: the default delegates to `step` */
  def stepInto(s: S, c: Char, out: Growable[Token[K]]): S = {
    val (s2, ts) = step(s, c)
    out ++= ts
    s2
  }

  /** end of input: finish the tail */
  def flush(s: S): Vector[Token[K]]

  /** a position-erased fingerprint of the state, for reconvergence */
  def key(s: S): Any = s

  /** shift the positions inside a state (override when S carries them) */
  def rebase(s: S, @unused offsetDelta: Int, @unused lineDelta: Int): S = s
}

/** a scanner written on the sink road: implement `stepInto`, and `step`
 * comes for free — FINAL, so two mutually delegating defaults cannot
 * be written */
trait ScanInto[K, S] extends Scan[K, S] {
  override def stepInto(s: S, c: Char, out: Growable[Token[K]]): S

  final def step(s: S, c: Char): (S, Vector[Token[K]]) = {
    val sink = new Scan.Sink[K]
    val s2 = stepInto(s, c, sink)
    (s2, sink.result())
  }
}

object Scan {

  /** what a `step` written on `stepInto` collects into */
  final class Sink[K] extends Growable[Token[K]] {
    private var acc: Vector[Token[K]] = Vector.empty
    def addOne(t: Token[K]): this.type = { acc = acc :+ t; this }
    def clear(): Unit = acc = Vector.empty
    def result(): Vector[Token[K]] = acc
  }

  /** the scanner as a pipeline Stage: awaits chars, tells tokens,
   * answers its final state */
  def stage[K, S](sc: Scan[K, S]): Stage[Char, Token[K], S] = {
    def tellAll(ts: Vector[Token[K]], then_ : => Stage[Char, Token[K], S]): Stage[Char, Token[K], S] =
      ts.foldRight(then_)((t, rest) => Stage.tell[Char, Token[K]](t).flatMap(_ => rest))
    Stage.transduce[Char, Token[K], S](sc.init)((s, c) => {
      val (s2, ts) = sc.step(s, c)
      tellAll(ts, pure(s2))
    }, s => tellAll(sc.flush(s), pure(s)))
  }

  /** the chunked path: a chunk of chars in, a chunk of tokens out, one
   * tight loop per chunk — the state crosses chunk boundaries as a value
   * and a token spanning chunks is emitted exactly once */
  def chunks[K, S](sc: Scan[K, S])(chars: Chunks[Char]): Chunks[Token[K]] = {
    def emit(ts: Vector[Token[K]], rest: => Chunks[Token[K]]): Chunks[Token[K]] =
      if (ts.isEmpty) rest
      else Writer.tell(ChunkBuf.of(ts)).flatMap(_ => rest)

    def go(s: S, rest: Chunks[Char]): Chunks[Token[K]] = Chunks.defer {
      Chunks.pull(rest) match {
        case Some((c, r)) =>
          val out = Vector.newBuilder[Token[K]]
          var st = s
          var i = 0
          c match {
            case cs: scala.collection.immutable.ArraySeq.ofChar =>
              val arr = cs.unsafeArray
              while (i < arr.length) { st = sc.stepInto(st, arr(i), out); i += 1 }
            case _ =>
              while (i < c.length) { st = sc.stepInto(st, c(i), out); i += 1 }
          }
          emit(out.result(), go(st, r))
        case None => emit(sc.flush(s), Chunks.end[Token[K]])
      }
    }

    go(sc.init, chars)
  }

  /** a sink that FOLDS instead of collecting */
  private final class Folding[K, R](z: R, f: (R, Token[K]) => R) extends Growable[Token[K]] {
    private var acc: R = z
    def addOne(t: Token[K]): this.type = { acc = f(acc, t); this }
    def clear(): Unit = acc = z
    def result: R = acc
  }

  /** every token folded AS IT IS PRODUCED, with nothing materialised */
  def fold[K, S, R](sc: Scan[K, S])(input: String)(z: R)(f: (R, Token[K]) => R): R = {
    val sink = new Folding[K, R](z, f)
    var s = sc.init
    var i = 0
    while (i < input.length) { s = sc.stepInto(s, input.charAt(i), sink); i += 1 }
    sink.addAll(sc.flush(s))
    sink.result
  }

  /** the same, said with a named aggregation algebra */
  def aggregate[K, S, Acc, Out](sc: Scan[K, S])(input: String)(agg: Aggregator[Token[K], Acc, Out]): Out =
    agg.present(fold(sc)(input)(agg.init)(agg.add))

  /** everything lexed at once, with the snapshots relexing resumes from */
  final case class Lexed[K, S](tokens: Vector[Token[K]], snapshots: Vector[(Int, S)], state: S)

  /** lex a whole string, snapshotting the state every snapshotEvery chars */
  def all[K, S](sc: Scan[K, S])(input: String, snapshotEvery: Int = 64): Lexed[K, S] = {
    var s = sc.init
    val tokens = Vector.newBuilder[Token[K]]
    val snaps = Vector.newBuilder[(Int, S)]
    var i = 0
    while (i < input.length) {
      if (i % snapshotEvery == 0) snaps += ((i, s))
      s = sc.stepInto(s, input.charAt(i), tokens)
      i += 1
    }
    tokens ++= sc.flush(s)
    Lexed(tokens.result(), snaps.result(), s)
  }

  /**
   * Incremental relexing: resume from the nearest snapshot at or before
   * the edit, lex forward, and RECONVERGE — once past the edit and past
   * the next newline, a state equal to the old run's at the
   * corresponding old offset means everything beyond is the old tokens,
   * reused with shifted spans. No convergence relexes to the end —
   * never wrong, at worst not incremental. The two joins a pending token
   * can straddle (a duplicate at the resume cut, a loss at the
   * reconvergence) are closed with `flush`, as in the Scala 3 core.
   */
  def relex[K, S](sc: Scan[K, S])(old: Lexed[K, S], oldInput: String, newInput: String,
                                  editStart: Int, editEndOld: Int, editEndNew: Int,
                                  snapshotEvery: Int = 64): Lexed[K, S] = {
    val delta = newInput.length - oldInput.length
    val lineDelta =
      newInput.substring(editStart, editEndNew).count(_ == '\n') -
        oldInput.substring(editStart, editEndOld).count(_ == '\n')
    val base = old.snapshots.filter(_._1 <= editStart).lastOption.getOrElse((0, sc.init))
    val nlAfterOld = oldInput.indexOf('\n', editEndOld)
    val oldStates = old.snapshots.toMap

    val cut = sc.flush(base._2).map(_.span.offset).minOption.getOrElse(base._1)
    val keep = old.tokens.takeWhile(t => t.span.offset + t.span.length <= cut)

    var s = base._2
    val fresh = Vector.newBuilder[Token[K]]
    val snaps = Vector.newBuilder[(Int, S)]
    snaps ++= old.snapshots.takeWhile(_._1 < base._1)
    var i = base._1
    var done: Lexed[K, S] = null
    while (done == null && i < newInput.length) {
      val oldOff = i - delta
      if (i % snapshotEvery == 0) snaps += ((i, s))
      if (nlAfterOld >= 0 && oldOff > nlAfterOld && sc.flush(s).isEmpty
        && oldStates.get(oldOff).exists(st => sc.key(st) == sc.key(s))) {
        val tail = old.tokens.dropWhile(t => t.span.offset + t.span.length <= oldOff).map(t =>
          t.copy(span = t.span.copy(offset = t.span.offset + delta, line = t.span.line + lineDelta)))
        done = Lexed(keep ++ fresh.result() ++ tail,
          snaps.result() ++ old.snapshots.dropWhile(_._1 < oldOff)
            .map { case (o, st) => (o + delta, sc.rebase(st, delta, lineDelta)) },
          old.state)
      } else {
        s = sc.stepInto(s, newInput.charAt(i), fresh)
        i += 1
      }
    }
    if (done != null) done
    else {
      fresh ++= sc.flush(s)
      Lexed(keep ++ fresh.result(), snaps.result(), s)
    }
  }
}
