package okay2.parse

import okay2.Fold
import okay2.lex.{Scan, Token}
import okay2.stream.Stage

/**
 * Streaming, error-tolerant parsing — okay-parse's Parse.scala
 * (okay2-lex-parse; specs/streaming-parse.md): a TOTAL function from
 * token streams to trees. Any input — truncated or damaged — yields a
 * tree; what did not parse is IN the tree as error nodes with the
 * offending tokens. The one instruction language below is the meeting
 * point of both surfaces (a hand-written driver and the combinators):
 * whoever emits Instr, the same total builder folds it into the same
 * lossless CST.
 */
sealed trait Instr[K]
object Instr {
  /** enter a node (the token, if any, is kept — lossless) */
  final case class Open[K](kind: String, tok: Option[Token[K]]) extends Instr[K]
  /** attach one token to the current node */
  final case class Emit[K](tok: Token[K]) extends Instr[K]
  /** leave the current node (with its closing token, if any) */
  final case class Close[K](tok: Option[Token[K]]) extends Instr[K]
  /** an error leaf: the offending token (if any) and why */
  final case class Bad[K](tok: Option[Token[K]], message: String) extends Instr[K]
}

/** the lossless concrete syntax tree */
sealed trait Cst[K]

object Cst {
  final case class Node[K](kind: String, children: Vector[Cst[K]]) extends Cst[K]
  final case class Leaf[K](tok: Token[K]) extends Cst[K]
  final case class Err[K](tok: Option[Token[K]], message: String) extends Cst[K]

  /**
   * Every node of the tree, pre-order, on an EXPLICIT stack: the tree is
   * as deep as the document, and the recursive walks the Scala 3 core
   * writes (`cs.map(lexemes)`, `cs.flatMap(errors)`) overflow the JVM
   * stack on a document `Parse.full` itself builds without trouble —
   * measured here at 20 000 levels (`errors` overflowed where the parse
   * did not; okay-parse backlog `cst-walk-stack-safe`).
   */
  private def preorder[K](c: Cst[K])(visit: Cst[K] => Unit): Unit = {
    var stack: List[Cst[K]] = c :: Nil
    while (stack.nonEmpty) {
      val here = stack.head
      stack = stack.tail
      visit(here)
      here match {
        case Node(_, cs) => stack = cs.foldRight(stack)(_ :: _)
        case _ => ()
      }
    }
  }

  /** the lossless law: every kept token's lexeme, in order */
  def lexemes[K](c: Cst[K]): String = {
    val out = new StringBuilder
    preorder(c) {
      case Leaf(t) => out ++= t.lexeme
      case Err(t, _) => t.foreach(x => out ++= x.lexeme)
      case _ => ()
    }
    out.result()
  }

  /** the diagnostics are IN the tree: collect them */
  def errors[K](c: Cst[K]): Vector[(Option[Token[K]], String)] = {
    val out = Vector.newBuilder[(Option[Token[K]], String)]
    preorder(c) {
      case Err(t, m) => out += ((t, m))
      case _ => ()
    }
    out.result()
  }

  /** shift every span in a subtree (the absolute-span tax on reuse after
   * a length-changing edit; a length-preserving one reuses by reference) */
  def rebase[K](c: Cst[K], offsetDelta: Int, lineDelta: Int): Cst[K] =
    if (offsetDelta == 0 && lineDelta == 0) c
    else {
      def tok(t: Token[K]): Token[K] =
        t.copy(span = t.span.copy(offset = t.span.offset + offsetDelta, line = t.span.line + lineDelta))
      c match {
        case Node(k, cs) => Node(k, cs.map(rebase(_, offsetDelta, lineDelta)))
        case Leaf(t) => Leaf(tok(t))
        case Err(t, m) => Err(t.map(tok), m)
      }
    }
}

object Parse {

  /** the parser side of the pipeline: tokens await in, instructions tell out */
  type Driver[K, A] = Stage[Token[K], Instr[K], A]

  final case class Building[K](stack: List[(String, Option[Token[K]], Vector[Cst[K]])], done: Vector[Cst[K]])

  /**
   * The TOTAL builder: any instruction stream folds into a tree — a Close
   * with nothing open becomes an error leaf, and open nodes left at the
   * end are closed by `present` with an unclosed-marker, so a truncated
   * stream is a tree with holes, never a fault.
   */
  def build[K]: Fold[Instr[K], Building[K]] =
    Fold[Instr[K], Building[K]](Building[K](Nil, Vector.empty)) { (b, i) =>
      i match {
        case Instr.Open(kind, tok) => Building((kind, tok, tok.map(t => Cst.Leaf(t): Cst[K]).toVector) :: b.stack, b.done)
        case Instr.Emit(tok) => attach(b, Cst.Leaf(tok))
        case Instr.Bad(tok, msg) => attach(b, Cst.Err(tok, msg))
        case Instr.Close(tok) => b.stack match {
          case (kind, _, kids) :: rest =>
            val node: Cst[K] = Cst.Node(kind, kids ++ tok.map(t => Cst.Leaf(t): Cst[K]))
            attach(Building(rest, b.done), node)
          case Nil => attach(b, Cst.Err(tok, "nothing to close"))
        }
      }
    }

  private def attach[K](b: Building[K], c: Cst[K]): Building[K] = b.stack match {
    case (kind, tok, kids) :: rest => Building((kind, tok, kids :+ c) :: rest, b.done)
    case Nil => Building(Nil, b.done :+ c)
  }

  /** finish: close what is still open (holes, marked), one root */
  def present[K](b: Building[K]): Cst[K] = {
    val closed = b.stack.foldLeft(b) { (acc, _) =>
      acc.stack match {
        case (kind, _, kids) :: rest => attach(Building(rest, acc.done), Cst.Node(kind, kids :+ (Cst.Err[K](None, "unclosed"): Cst[K])))
        case Nil => acc
      }
    }
    Cst.Node("root", closed.done)
  }

  /** fold a finished instruction sequence into the tree */
  def toCst[K](instrs: IterableOnce[Instr[K]]): Cst[K] =
    present(instrs.iterator.foldLeft(build[K].init)(build[K].add))

  // ------------------------------------------------------------------
  // incremental reparse: node-boundary snapshots over lex reconvergence

  /** a parsed session: the lexed layer, the tree, and builder snapshots
   * (tokenIndex, Building, driver state) at ROOT-LEVEL node boundaries;
   * Building is persistent, so a snapshot is a pointer */
  final case class Parsed[K, S, D](lexed: Scan.Lexed[K, S], tree: Cst[K], snaps: Vector[(Int, Building[K], D)])

  /** a driver as a pure STEP FUNCTION, exactly like a Scan one layer down */
  type Step[K, D] = (D, Token[K]) => (D, Vector[Instr[K]])

  /** the stateless driver of the simple dialects, as a Step */
  def stateless[K](f: Token[K] => Vector[Instr[K]]): Step[K, Unit] = (_, t) => ((), f(t))

  /** parse a whole input, snapshotting at ROOT-LEVEL node boundaries */
  def fullWith[K, S, D](sc: Scan[K, S], step: Step[K, D], initD: D,
                        finish: D => Vector[Instr[K]] = (_: D) => Vector.empty[Instr[K]])
                       (input: String, snapshotEvery: Int = 64): Parsed[K, S, D] = {
    val lexed = Scan.all(sc)(input, snapshotEvery)
    val snaps = Vector.newBuilder[(Int, Building[K], D)]
    var b = build[K].init
    var d = initD
    var i = 0
    while (i < lexed.tokens.length) {
      // `sizeIs`, not `length`: the stack's length is the nesting depth,
      // and a per-token `.length` made deep documents O(depth²)
      // (parse-quadratic-stack-length in the Scala 3 core)
      if (b.stack.sizeIs <= 1) snaps += ((i, b, d))
      val (d2, is) = step(d, lexed.tokens(i))
      d = d2
      b = is.foldLeft(b)(build[K].add)
      i += 1
    }
    b = finish(d).foldLeft(b)(build[K].add)
    Parsed(lexed, present(b), snaps.result())
  }

  /** the common case: a per-token driver with no state of its own */
  def full[K, S](sc: Scan[K, S], step: Token[K] => Vector[Instr[K]])(input: String, snapshotEvery: Int = 64): Parsed[K, S, Unit] =
    fullWith[K, S, Unit](sc, stateless(step), (), (_: Unit) => Vector.empty[Instr[K]])(input, snapshotEvery)

  /**
   * Reparse after an edit: relex (reconvergence), resume the builder from
   * the nearest node-boundary snapshot before the damage, drive forward,
   * and SPLICE once the token stream is the old stream again at a
   * matching boundary — by REFERENCE when the edit preserved offsets and
   * lines, else rebased. Driver and builder work is O(damage); no
   * convergence reparses to the end — never wrong, at worst not
   * incremental.
   */
  def reparseWith[K, S, D](sc: Scan[K, S], step: Step[K, D], initD: D,
                           finish: D => Vector[Instr[K]] = (_: D) => Vector.empty[Instr[K]])
                          (old: Parsed[K, S, D], oldInput: String, newInput: String,
                           editStart: Int, editEndOld: Int, editEndNew: Int,
                           snapshotEvery: Int = 64): Parsed[K, S, D] = {
    val lexed = Scan.relex(sc)(old.lexed, oldInput, newInput, editStart, editEndOld, editEndNew, snapshotEvery)
    val toks = lexed.tokens
    val oldToks = old.lexed.tokens
    val delta = newInput.length - oldInput.length
    val lineDelta =
      newInput.substring(editStart, editEndNew).count(_ == '\n') -
        oldInput.substring(editStart, editEndOld).count(_ == '\n')
    val tokenDelta = toks.length - oldToks.length

    def shifted(t: Token[K]): Token[K] =
      t.copy(span = t.span.copy(offset = t.span.offset + delta, line = t.span.line + lineDelta))

    var p = 0
    while (p < toks.length && p < oldToks.length && toks(p) == oldToks(p)) p += 1
    var l = 0
    while (l < toks.length - p && l < oldToks.length - p
      && toks(toks.length - 1 - l) == shifted(oldToks(oldToks.length - 1 - l))) l += 1

    val oldSnaps = old.snaps.map(s => (s._1, (s._2, s._3))).toMap
    val oldRootKids = old.tree match {
      case Cst.Node(_, kids) => kids
      case other => Vector(other)
    }

    val (start, b0, d0) = old.snaps.filter(_._1 <= p).lastOption.getOrElse((0, build[K].init, initD))
    val snaps = Vector.newBuilder[(Int, Building[K], D)]
    snaps ++= old.snaps.takeWhile(_._1 < start)

    var b = b0
    var d = d0
    var i = start
    var spliced: Parsed[K, S, D] = null
    while (spliced == null && i < toks.length) {
      if (b.stack.sizeIs <= 1) {
        snaps += ((i, b, d))
        // reconverged? the rest of the tokens is the old stream, the old
        // run stood at a matching boundary here, AND its driver was in the
        // same state
        if (i >= toks.length - l) {
          val o = i - tokenDelta
          oldSnaps.get(o) match {
            case Some((bo, dOld)) if bo.stack.sizeCompare(b.stack) == 0
              && bo.stack.map(_._1) == b.stack.map(_._1) && dOld == d =>
              val suffixTop = oldRootKids.drop(bo.done.length)
              val kids = (b.stack, bo.stack) match {
                case (Nil, Nil) => b.done ++ suffixTop.map(Cst.rebase(_, delta, lineDelta))
                case ((k, _, kidsN) :: Nil, (_, _, kidsO) :: Nil) =>
                  val frame: Cst[K] = suffixTop.headOption match {
                    case Some(Cst.Node(fk, fkids)) if fk == k =>
                      Cst.Node(k, kidsN ++ fkids.drop(kidsO.length).map(Cst.rebase(_, delta, lineDelta)))
                    case _ => Cst.Node(k, kidsN)   // shape drifted: keep the new side
                  }
                  b.done ++ (frame +: suffixTop.drop(1).map(Cst.rebase(_, delta, lineDelta)))
                case _ => b.done   // unreachable: lengths matched above
              }
              spliced = Parsed(lexed, Cst.Node("root", kids), snaps.result())
            case _ => ()
          }
        }
      }
      if (spliced == null) {
        val (d2, is) = step(d, toks(i))
        d = d2
        b = is.foldLeft(b)(build[K].add)
        i += 1
      }
    }
    if (spliced != null) spliced
    else {
      b = finish(d).foldLeft(b)(build[K].add)
      Parsed(lexed, present(b), snaps.result())
    }
  }

  /** the common case: a per-token driver with no state of its own */
  def reparse[K, S](sc: Scan[K, S], step: Token[K] => Vector[Instr[K]])
                   (old: Parsed[K, S, Unit], oldInput: String, newInput: String,
                    editStart: Int, editEndOld: Int, editEndNew: Int,
                    snapshotEvery: Int = 64): Parsed[K, S, Unit] =
    reparseWith[K, S, Unit](sc, stateless(step), (), (_: Unit) => Vector.empty[Instr[K]])(
      old, oldInput, newInput, editStart, editEndOld, editEndNew, snapshotEvery)
}
