package okay
package macros

import scala.quoted.*

/**
 * THE MARK SYNTAX, ONCE (direct-macros-shared-syntax): what a `direct`
 * block's reader wrote that the macro must bind — the four spellings of a
 * mark and the colouring conversions the typer inserts — and the two tree
 * questions every road asks of them: which value a mark carries, and is
 * there one under here. Syntax only: no monad, no arrow, no emission.
 *
 * Two roads read it. `Direct`'s compiler (monadic, applicative, selective:
 * one compiler with modes) mixes it in through `DirectPhase`; the arrow
 * road, `ProcMacro`, makes an instance at its own Quotes. They differ ONLY
 * in the colouring conversions they recognise, which is the one abstract
 * member here. Before this trait the arrow road had its own copy of these
 * lines, so a spelling added to one road was silently not a mark on the
 * other.
 *
 * What is NOT shared, and why (specs/proc-notation.md, stage 2): the IR. A
 * statement NESTS A CONTINUATION on the monadic road and APPENDS TO AN
 * ENVIRONMENT on the arrow one; one IR would be two IRs with one name.
 */
private[okay] trait MarkSyntax:
  val q: Quotes
  protected given q.type = q
  import q.reflect.*

  /** the colouring conversions THIS road recognises as marks */
  def colorSyms: Set[Symbol]

  lazy val directSym: Symbol = TypeRepr.of[Direct.type].typeSymbol
  lazy val markSyms: Set[Symbol] = (directSym.methodMember("reflect")
    ++ directSym.methodMember("?") ++ directSym.methodMember("unary_!")).toSet

  /** a term with its inlining and ascription wrappers taken off */
  def stripped(t: Term): Term = Direct.stripped(t)

  def calleeRoot(t: Term): Symbol = t match
    case Apply(f, _) => calleeRoot(f)
    case TypeApply(f, _) => calleeRoot(f)
    case Inlined(_, Nil, inner) => calleeRoot(inner)
    case _ => t.symbol

  /** the marked value: an explicit mark call OR an inserted colouring
   * conversion call — one dispatch serves both, because both name an
   * operation whose answer the block wants */
  def asMark(t: Term): Option[Term] = t match
    case Apply(TypeApply(fun, _), List(m)) if markSyms(fun.symbol) => Some(m)
    case Apply(Select(conv, "apply"), List(x)) if colorSyms(calleeRoot(conv)) => Some(x)
    case _ => None

  def hasMark(t: Tree): Boolean =
    var found = false
    val probe = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case term: Term if asMark(term).isDefined => found = true
          case _ => if !found then super.traverseTree(tree)(owner)
    probe.traverseTree(t)(Symbol.spliceOwner)
    found
