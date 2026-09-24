package okay
package macros

import scala.quoted.*

/**
 * What every phase of the `direct` compiler sees (direct-compiler-
 * phases, 2026-09-20): the Quotes it builds terms in, the block's
 * monad F, the hoisted `Monad[F]` expression, and the two modes the
 * imports set. The phases are traits over this one — DirectMarks,
 * DirectRow, DirectEmit, DirectDefer, DirectVals, DirectLoops,
 * DirectParallel — and `DirectCompiler` is the class that mixes them
 * and holds the recursion knot: `compile` and `compileBlock` are
 * abstract HERE so that a phase which calls back into the compiler
 * says so by using them, and nothing else in a phase can.
 *
 * Path dependence sets the shape. `q.reflect.Term` is a type OF this
 * instance's `q`, so a phase is a trait sharing the instance rather
 * than a function taking a Term; the given below is declared at
 * `q.type`, not `Quotes`, so a dependent method called from a phase
 * (`Direct.stripped`, `DirectCompiler.pipeline`) resolves to THIS q
 * and its Term is ours. Whatever crosses from another Quotes — the
 * entry, the try body's sub-pipeline, a test probe — crosses as an
 * `Expr`, which no path owns.
 */
private[okay] trait DirectPhase[F[_]] extends MarkSyntax:
  val fT: Type[F]
  protected given Type[F] = fT
  import q.reflect.*

  /** the hoisted `Monad[F]`: one val for the whole block (DirectCompiler.pipeline) */
  def M: Expr[Monad[F]]
  /** `import Direct.eagerCalls.given` — build calls where they stand */
  def eager: Boolean
  /** `import Direct.parallelBinds.given` — spawn independent binds together */
  def parallel: Boolean
  /** the block's `Stager` object, hoisted to a val, when the block is a
   * staged one (specs/direct-staged.md): every operation under a mark
   * is emitted as `stage.stage(op)` instead of `Free.Inject(op)` */
  def stage: Option[q.reflect.Term]

  /** the recursion knot — the core's, implemented in DirectCompiler */
  def compile(t: Term): Out
  def compileBlock(stats: List[Statement], expr: Term): Out

  /** the monad's words — DirectEmit's; declared here so a phase
   * below it (DirectRow's program walker, direct-staged v2) can
   * emit a bind or a pure without extending emission */
  def bind(fa: Term, vTpe: TypeRepr, resTpe: TypeRepr)(body: Term => Term): Term
  def pureF(t: Term): Term

  def refuse(t: Tree, where: String): Nothing =
    report.errorAndAbort(
      s"a Direct mark (.reflect) $where cannot be rewritten by direct's v1 — " +
        "bind the marked value to a val before it, or use a for-comprehension over Monadic",
      t.pos)

  /**
   * Compile a term to either a pure term (no marks) or an F[elem]
   * term. Eff CARRIES its element type: F is invariant in general,
   * so the value type travels with the term instead of being
   * parsed back out of it. Pure children that precede an effectful
   * child are NOT reordered: the ANF hoisting below binds children
   * left to right.
   */
  enum Out:
    case Pure(t: Term)
    case Eff(f: Term, elem: TypeRepr) // f : F[elem]

  /** run f with the TypeRepr as a Type given */
  def tpe2[R](tpe: TypeRepr)(f: [T] => Type[T] ?=> R): R =
    tpe.asType match
      case '[t] => f[t]
    end match
