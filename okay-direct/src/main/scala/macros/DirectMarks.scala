package okay
package macros

import scala.quoted.*

/**
 * MARKS: what the reader wrote that the block must bind — the four
 * spellings of the mark and the two auto-colouring conversions — and
 * the tree walks over them: is there one under here, which value
 * does it carry, where is a symbol used and how, replace those uses.
 * Syntax only: nothing here knows the monad, nothing here emits.
 */
private[okay] trait DirectMarks[F[_]] extends DirectPhase[F]:
  import q.reflect.*

  /** the colouring conversions of `direct` blocks; the spellings and the
   * tree questions over them are MarkSyntax's, shared with the arrow road */
  lazy val colorSyms: Set[Symbol] = (directSym.methodMember("selfColor") ++
    directSym.methodMember("opColor") ++
    Symbol.requiredModule("okay.Free").methodMember("directColor")).toSet

  /** the GENERIC mark `reflect[F[_], A]` — by arity, since `reflect[W]`
   * on a generator value (specs/generators.md) shares the name */
  lazy val reflectMark: Symbol =
    directSym.methodMember("reflect").find(_.paramSymss.headOption.exists(_.sizeIs == 2)).get

  /** does this tree mention any of these symbols? */
  def mentionsAny(t: Tree, syms: Set[Symbol]): Boolean =
    if syms.isEmpty then false
    else
      var found = false
      val tr = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          if !found then tree match
            case id: Ident if syms.contains(id.symbol) => found = true
            case _ => super.traverseTree(tree)(owner)
      tr.traverseTree(t)(Symbol.spliceOwner)
      found

  /** replace references to `sym` with `ref` */
  def subst(t: Term, sym: Symbol, ref: Term): Term =
    val m = new TreeMap:
      override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
        case id: Ident if id.symbol == sym => ref
        case _ => super.transformTerm(tree)(owner)
    m.transformTerm(t)(Symbol.spliceOwner)

  /** replace every use of `sym` in the statements and the result with a
   * fresh `ref()` — INCLUDING a use wrapped in a colouring conversion,
   * which is how a colourless val of the block's program type is read
   * (direct-colourless-val): the conversion goes with the reference,
   * since `ref()` already stands at the element type */
  def substUses(stats: List[Statement], expr: Term, sym: Symbol, ref: () => Term): (List[Statement], Term) =
    substUsesBy(stats, expr, sym, ref, ref)

  /** the same, with the COLOURED use and the BARE use replaced by
   * different terms — a nested program def needs that: read as a value
   * it becomes a mark, read as a program it becomes the program */
  def substUsesBy(stats: List[Statement], expr: Term, sym: Symbol,
                  refColoured: () => Term, refBare: () => Term): (List[Statement], Term) =
    val m = new TreeMap:
      override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
        case Apply(Select(conv, "apply"), List(id: Ident))
          if colorSyms(calleeRoot(conv)) && id.symbol == sym => refColoured()
        case id: Ident if id.symbol == sym => refBare()
        case _ => super.transformTerm(tree)(owner)
    (stats.map(st => m.transformStatement(st)(Symbol.spliceOwner)), m.transformTerm(expr)(Symbol.spliceOwner))

  /**
   * How a local of the block's PROGRAM type is read in what follows:
   * COLOURED (the conversion applied to the bare reference — read as a
   * value, `x + 1`) or BARE (read as a program — marked, passed on,
   * `!.once(p)`).
   */
  def useKinds(stats: List[Statement], expr: Term, sym: Symbol): (Int, Int) =
    var coloured = 0
    var bare = 0
    val probe = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
        case Apply(Select(conv, "apply"), List(id: Ident))
          if colorSyms(calleeRoot(conv)) && id.symbol == sym => coloured += 1
        case id: Ident if id.symbol == sym => bare += 1
        case _ => super.traverseTree(tree)(owner)
    (stats :+ expr).foreach(t => probe.traverseTree(t)(Symbol.spliceOwner))
    (coloured, bare)
