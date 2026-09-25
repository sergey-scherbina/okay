package okay.rag

import okay.lex.{Scan, Span, Token}
import okay.parse.Cst

/**
 * Documents into retrievable segments (specs/rag.md, P10a) — and the
 * whole point of doing it here rather than with a regex: we split the
 * TREE, not the string.
 *
 * Two consequences no text splitter can offer. First, PROVENANCE BY
 * CONSTRUCTION: every token carries an exact span and the CST is
 * lossless, so a segment is a byte range into the original and
 * `source.substring(span)` IS the segment's text — a citation cannot
 * drift. Second, boundaries land on STRUCTURE (a heading's section, a
 * JSON member, a YAML block) with the structural PATH carried along
 * as free metadata, instead of wherever a character count happened to
 * fall.
 */

/** an original document, kept whole — segments point into it */
final case class Source(id: String, text: String)

/**
 * A retrievable piece: the exact byte range it came from, its text
 * (equal to that range, by law), and the structural path that
 * located it — `["heading", "para"]` or `["object", "pair"]`.
 */
final case class Segment(source: String, span: Span, text: String,
                         path: Seq[String]):
  /** the law, as a method: a segment quotes its source exactly */
  def quotes(src: Source): Boolean =
    src.id == source &&
      span.offset + span.length <= src.text.length &&
      src.text.substring(span.offset, span.offset + span.length) == text

/**
 * A corpus: the sources a retriever's segments point into. This is
 * what makes a passage LINEAGE rather than a copy — the prompt
 * carries a projection, and anything more is a substring away, with
 * no second retrieval and no bigger prompt paid for up front.
 */
final case class Corpus(sources: Map[String, Source]):
  def add(s: Source): Corpus = Corpus(sources.updated(s.id, s))

  /** the segment as it stands, re-read from the source (a check that
   * the index has not drifted from the file) */
  def current(seg: Segment): Option[String] =
    sources.get(seg.source).filter(seg.quotes).map(_.text)

  /**
   * More of the document around a passage: `by` characters on each
   * side, clipped to the source and snapped outward to line
   * boundaries so the widened text starts and ends where a reader
   * would expect. The span stays exact, so the widened passage still
   * quotes its source.
   */
  def widen(seg: Segment, by: Int): Option[Segment] =
    sources.get(seg.source).map { src =>
      val text = src.text
      val from = math.max(0, seg.span.offset - by)
      val to = math.min(text.length, seg.span.offset + seg.span.length + by)
      // snapping to line boundaries must never move the window OFF
      // the passage: with a leading newline in the source, the snap
      // walked the start past the segment's own beginning, so a
      // widened passage no longer contained the passage it grew from
      // (found by a generated case, not by a chosen one)
      val snapped = text.lastIndexOf('\n', math.max(0, from - 1)) + 1
      val start = math.min(snapped, seg.span.offset)
      val snappedEnd = text.indexOf('\n', math.max(start, to - 1)) match
        case -1 => text.length
        case i => i + 1
      val end = math.max(snappedEnd, seg.span.offset + seg.span.length)
      val line = text.take(start).count(_ == '\n')
      val column = start - (text.lastIndexOf('\n', math.max(0, start - 1)) + 1)
      Segment(seg.source, okay.lex.Span(start, line, column, end - start),
        text.substring(start, end), seg.path)
    }

  /** the whole document a passage came from */
  def whole(seg: Segment): Option[Segment] =
    sources.get(seg.source).map(src =>
      Segment(src.id, okay.lex.Span(0, 0, 0, src.text.length), src.text, seg.path))

object Corpus:
  def of(sources: Seq[Source]): Corpus =
    Corpus(sources.map(s => (s.id, s)).toMap)

object Split {

  /** the tokens of a subtree, in order (trivia included — lossless), on
   * an EXPLICIT stack: a code tree nests as deep as its source, and a
   * per-level `kids.flatMap(tokens)` overflowed on one the builder had
   * just built (cst-walks-remaining, TestCodeDepth, 2026-09-25) */
  private[rag] def tokens[K](c: Cst[K]): Vector[Token[K]] =
    val out = Vector.newBuilder[Token[K]]
    var stack: List[Cst[K]] = c :: Nil
    while stack.nonEmpty do
      val here = stack.head
      stack = stack.tail
      here match
        case Cst.Node(_, kids) => stack = kids.foldRight(stack)(_ :: _)
        case Cst.Leaf(t) => out += t
        case Cst.Err(t, _) => t.foreach(out += _)
    out.result()

  /** the span covering a run of tokens: from the first offset to the
   * end of the last — exact, because every token's span is */
  private def cover[K](ts: Seq[Token[K]]): Option[Span] =
    if ts.isEmpty then None
    else
      val first = ts.minBy(_.span.offset).span
      val last = ts.maxBy(t => t.span.offset + t.span.length).span
      Some(Span(first.offset, first.line, first.column,
        (last.offset + last.length) - first.offset))

  private def kindOf[K](c: Cst[K]): String = c match
    case Cst.Node(k, _) => k
    case Cst.Leaf(_) => "leaf"
    case Cst.Err(_, _) => "error"

  /**
   * Split a parsed document into segments no larger than the budget,
   * cutting on structure: siblings are packed together greedily while
   * they fit, and a subtree too big for the budget is entered rather
   * than chopped. A leaf that alone exceeds the budget is emitted
   * whole (a segment may exceed the budget only when the document's
   * own atoms do — reported honestly rather than cut mid-token).
   *
   * `size` measures a segment's text: character counts for a rough
   * pass, a `Bpe` Scan for real token budgets — the same function the
   * agent's context compactor takes.
   */
  def structural[K](src: Source, cst: Cst[K], budget: Int)
                   (size: String => Int): Seq[Segment] =
    def textOf(ts: Seq[Token[K]]): String = ts.map(_.lexeme).mkString

    def emit(ts: Seq[Token[K]], path: Seq[String]): Seq[Segment] =
      cover(ts).toSeq.map(sp => Segment(src.id, sp, textOf(ts), path))

    // A node too big for the budget packs consecutive children while
    // they fit and ENTERS a child that does not fit on its own. The
    // entering is a frame on an explicit stack, not a call, since a code
    // tree nests as deep as its source (cst-walks-remaining): each frame
    // is a node being packed, its path, the next child, and what it has
    // emitted and is still holding.
    final class Frame(val path: Seq[String], val kids: Vector[Cst[K]]):
      var i = 0
      val out = Vector.newBuilder[Segment]
      var run = Vector.empty[Token[K]]
      def flush(): Unit =
        if run.nonEmpty then
          out ++= emit(run, path)
          run = Vector.empty

    val ts = tokens(cst)
    if ts.isEmpty then Seq.empty
    else if size(textOf(ts)) <= budget then emit(ts, Seq(kindOf(cst)))
    else cst match
      case Cst.Node(_, kids0) =>
        var stack: List[Frame] = Frame(Seq(kindOf(cst)), kids0) :: Nil
        var result: Seq[Segment] = Seq.empty
        while stack.nonEmpty do
          val f = stack.head
          if f.i < f.kids.length then
            val kid = f.kids(f.i)
            f.i += 1
            val kt = tokens(kid)
            if kt.isEmpty then ()
            else if size(textOf(f.run ++ kt)) <= budget then f.run = f.run ++ kt
            else
              f.flush()
              if size(textOf(kt)) <= budget then f.run = kt
              else kid match
                case Cst.Node(_, kk) => stack = Frame(f.path :+ kindOf(kid), kk) :: stack
                case _ => f.out ++= emit(kt, f.path :+ kindOf(kid))   // an atom bigger than the budget
          else
            f.flush()
            stack = stack.tail
            val done = f.out.result()
            if stack.isEmpty then result = done else stack.head.out ++= done
        result
      case _ => emit(ts, Seq(kindOf(cst)))   // an atom bigger than the budget

  /**
   * The other splitter: fixed windows over a TOKEN stream, with
   * overlap — the shape everyone ships, but exact here because the
   * tokens are the model's own (a `Bpe` Scan) and the spans are the
   * lexer's. Overlap is expressed the way the context window is: a
   * sliding window that subtracts as it advances.
   */
  def windows[K, S](src: Source, scan: Scan[K, S], budget: Int, overlap: Int = 0)
  : Seq[Segment] =
    require(overlap < budget, "overlap must be smaller than the budget")
    val all = Scan.all(scan)(src.text).tokens
    if all.isEmpty then Seq.empty
    else
      val step = budget - overlap
      Iterator.iterate(0)(_ + step)
        .takeWhile(_ < all.length)
        .map { start =>
          val ts = all.slice(start, start + budget)
          val sp = cover(ts).get
          Segment(src.id, sp, ts.map(_.lexeme).mkString, Seq("window"))
        }.toSeq

  /** every character of the source is covered by at least one segment */
  def covers(src: Source, segs: Seq[Segment]): Boolean =
    val marked = Array.fill(src.text.length)(false)
    for s <- segs; i <- s.span.offset until (s.span.offset + s.span.length) do
      if i >= 0 && i < marked.length then marked(i) = true
    marked.forall(identity)
}
