package okay.parse

import okay.{!, %, Fold, Stage, pure}
import okay.given
import okay.lex.{Channel, Token}

/**
 * Streaming, error-tolerant parsing (specs/streaming-parse.md): a
 * TOTAL function from token streams to trees. Any input — truncated
 * or damaged — yields a tree; what did not parse is IN the tree as
 * error nodes with the offending tokens, and Throws never appears.
 * The one instruction language below is the meeting point of both
 * surfaces (a hand-written driver and the combinators): whoever emits
 * Instr, the same total builder folds it into the same lossless CST.
 */
enum Instr[K]:
  /** enter a node (the token, if any, is kept — lossless) */
  case Open(kind: String, tok: Option[Token[K]])

  /** attach one token to the current node */
  case Emit(tok: Token[K])

  /** leave the current node (with its closing token, if any) */
  case Close(tok: Option[Token[K]])

  /** an error leaf: the offending token (if any) and why */
  case Bad(tok: Option[Token[K]], message: String)

/** the lossless concrete syntax tree */
enum Cst[K]:
  case Node(kind: String, children: Vector[Cst[K]])
  case Leaf(tok: Token[K])
  case Err(tok: Option[Token[K]], message: String)

object Cst:
  /** the lossless law: every kept token's lexeme, in order */
  def lexemes[K](c: Cst[K]): String = c match
    case Cst.Node(_, cs) => cs.map(lexemes).mkString
    case Cst.Leaf(t) => t.lexeme
    case Cst.Err(t, _) => t.fold("")(_.lexeme)

  /** the diagnostics are IN the tree: collect them */
  def errors[K](c: Cst[K]): Vector[(Option[Token[K]], String)] = c match
    case Cst.Node(_, cs) => cs.flatMap(errors)
    case Cst.Err(t, m) => Vector((t, m))
    case _ => Vector.empty

  /** shift every span in a subtree (the absolute-span tax on reuse
   * after a length-changing edit; a length-preserving edit reuses by
   * reference instead — see Parse.reparse) */
  def rebase[K](c: Cst[K], offsetDelta: Int, lineDelta: Int): Cst[K] =
    if offsetDelta == 0 && lineDelta == 0 then c
    else
      def tok(t: Token[K]): Token[K] =
        t.copy(span = t.span.copy(offset = t.span.offset + offsetDelta,
          line = t.span.line + lineDelta))
      c match
        case Cst.Node(k, cs) => Cst.Node(k, cs.map(rebase(_, offsetDelta, lineDelta)))
        case Cst.Leaf(t) => Cst.Leaf(tok(t))
        case Cst.Err(t, m) => Cst.Err(t.map(tok), m)

object Parse {

  /** the parser side of the pipeline: tokens await in, instructions tell out */
  type Driver[K, A] = Stage[Token[K], Instr[K], A]

  final case class Building[K](stack: List[(String, Option[Token[K]], Vector[Cst[K]])],
                                       done: Vector[Cst[K]])

  /**
   * The TOTAL builder: any instruction stream folds into a tree — a
   * Close with nothing open becomes an error leaf, and open nodes
   * left at the end are closed by present with an unclosed-marker,
   * so a truncated stream (the LLM case) is a tree with holes, never
   * a fault.
   */
  def build[K]: Fold[Instr[K], Building[K]] =
    Fold(Building[K](Nil, Vector.empty)) { (b, i) =>
      i match
        case Instr.Open(kind, tok) =>
          Building((kind, tok, tok.map(Cst.Leaf(_)).toVector) :: b.stack, b.done)
        case Instr.Emit(tok) => attach(b, Cst.Leaf(tok))
        case Instr.Bad(tok, msg) => attach(b, Cst.Err(tok, msg))
        case Instr.Close(tok) => b.stack match
          case (kind, _, kids) :: rest =>
            val node = Cst.Node(kind, kids ++ tok.map(Cst.Leaf(_)))
            Building(rest, b.done).pipe(attach(_, node))
          case Nil => attach(b, Cst.Err(tok, "nothing to close"))
    }

  extension [A](a: A) private def pipe[B](f: A => B): B = f(a)

  private def attach[K](b: Building[K], c: Cst[K]): Building[K] = b.stack match
    case (kind, tok, kids) :: rest => Building((kind, tok, kids :+ c) :: rest, b.done)
    case Nil => Building(Nil, b.done :+ c)

  /** finish: close what is still open (holes, marked), one root */
  def present[K](b: Building[K]): Cst[K] =
    val closed = b.stack.foldLeft(b): (acc, _) =>
      acc.stack match
        case (kind, _, kids) :: rest =>
          attach(Building(rest, acc.done),
            Cst.Node(kind, kids :+ Cst.Err(None, "unclosed")))
        case Nil => acc
    Cst.Node("root", closed.done)

  /** fold a finished instruction sequence into the tree */
  def toCst[K](instrs: IterableOnce[Instr[K]]): Cst[K] =
    present(instrs.iterator.foldLeft(build[K].init)(build[K].add))

  // ------------------------------------------------------------------
  // incremental reparse (specs/streaming-parse.md): node-boundary
  // snapshots over lex reconvergence

  import okay.lex.Scan

  /**
   * A parsed session: the lexed layer (with its own snapshots), the
   * tree, and builder snapshots — (tokenIndex, Building) taken at
   * ROOT-LEVEL node boundaries (stack depth <= 1). Building is
   * persistent, so a snapshot is a pointer, not a copy.
   */
  final case class Parsed[K, S](lexed: Scan.Lexed[K, S], tree: Cst[K],
                                snaps: Vector[(Int, Building[K])])

  /**
   * Parse a whole input with a PER-TOKEN driver (a token maps to its
   * instructions with no cross-token state — the contract that makes
   * token-level reconvergence sound; the stateful part of parsing is
   * the builder, and the builder is the thing snapshotted).
   */
  def full[K, S](sc: Scan[K, S], step: Token[K] => Vector[Instr[K]])
                (input: String, snapshotEvery: Int = 64): Parsed[K, S] =
    val lexed = Scan.all(sc)(input, snapshotEvery)
    val snaps = Vector.newBuilder[(Int, Building[K])]
    var b = build[K].init
    var i = 0
    while i < lexed.tokens.length do
      if b.stack.length <= 1 then snaps += ((i, b))
      b = step(lexed.tokens(i)).foldLeft(b)(build[K].add)
      i += 1
    Parsed(lexed, present(b), snaps.result())

  /**
   * Reparse after an edit: relex (okay-lex reconvergence), resume the
   * builder from the nearest node-boundary snapshot before the
   * damage, drive forward, and SPLICE once the token stream is the
   * old stream again at a matching boundary — the old tree's
   * remaining subtrees are reused: by REFERENCE when the edit
   * preserved offsets and lines, else rebased (the absolute-span tax;
   * relative spans are the future refinement). Driver and builder
   * work is O(damage); no convergence found reparses to the end —
   * never wrong, at worst not incremental.
   */
  def reparse[K, S](sc: Scan[K, S], step: Token[K] => Vector[Instr[K]])
                   (old: Parsed[K, S], oldInput: String, newInput: String,
                    editStart: Int, editEndOld: Int, editEndNew: Int,
                    snapshotEvery: Int = 64): Parsed[K, S] =
    val lexed = Scan.relex(sc)(old.lexed, oldInput, newInput,
      editStart, editEndOld, editEndNew, snapshotEvery)
    val toks = lexed.tokens
    val oldToks = old.lexed.tokens
    val delta = newInput.length - oldInput.length
    val lineDelta =
      newInput.substring(editStart, editEndNew).count(_ == '\n') -
        oldInput.substring(editStart, editEndOld).count(_ == '\n')
    val tokenDelta = toks.length - oldToks.length

    inline def shifted(t: Token[K]): Token[K] =
      t.copy(span = t.span.copy(offset = t.span.offset + delta, line = t.span.line + lineDelta))

    // the common prefix (untouched tokens) and the common suffix
    // (the relex-reused tail, spans already shifted)
    var p = 0
    while p < toks.length && p < oldToks.length && toks(p) == oldToks(p) do p += 1
    var l = 0
    while l < toks.length - p && l < oldToks.length - p
      && toks(toks.length - 1 - l) == shifted(oldToks(oldToks.length - 1 - l)) do l += 1

    val oldSnaps = old.snaps.toMap
    val oldRootKids = old.tree match
      case Cst.Node(_, kids) => kids
      case other => Vector(other)

    val (start, b0) = old.snaps.filter(_._1 <= p).lastOption.getOrElse((0, build[K].init))
    val snaps = Vector.newBuilder[(Int, Building[K])]
    snaps ++= old.snaps.takeWhile(_._1 < start)

    var b = b0
    var i = start
    while i < toks.length do
      if b.stack.length <= 1 then
        snaps += ((i, b))
        // reconverged? the rest of the token stream is the old one,
        // and the old run stood at a matching boundary here
        if i >= toks.length - l then
          val o = i - tokenDelta
          oldSnaps.get(o) match
            case Some(bo) if bo.stack.length == b.stack.length
              && bo.stack.map(_._1) == b.stack.map(_._1) =>
              val suffixTop = oldRootKids.drop(bo.done.length)
              val kids = (b.stack, bo.stack) match
                case (Nil, Nil) =>
                  b.done ++ suffixTop.map(Cst.rebase(_, delta, lineDelta))
                case ((k, _, kidsN) :: Nil, (_, _, kidsO) :: Nil) =>
                  val frame = suffixTop.headOption match
                    case Some(Cst.Node(fk, fkids)) if fk == k =>
                      Cst.Node(k, kidsN ++ fkids.drop(kidsO.length)
                        .map(Cst.rebase(_, delta, lineDelta)))
                    case _ => Cst.Node(k, kidsN)   // shape drifted: keep the new side
                  b.done ++ (frame +: suffixTop.drop(1).map(Cst.rebase(_, delta, lineDelta)))
                case _ => b.done   // unreachable: lengths matched above
              return Parsed(lexed, Cst.Node("root", kids), snaps.result())
            case _ => ()
      b = step(toks(i)).foldLeft(b)(build[K].add)
      i += 1

    Parsed(lexed, present(b), snaps.result())
}
