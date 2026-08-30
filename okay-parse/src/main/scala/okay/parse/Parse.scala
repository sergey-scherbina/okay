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
}
