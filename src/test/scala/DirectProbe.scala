package okay

import scala.quoted.*
import Direct.{DirectCtx, Deferral}

/**
 * Probes into the phases of the direct compiler (direct-compiler-phases,
 * specs/direct-macro.md "Structure"): each runs ONE phase over a block
 * and answers with plain data — a shown tree, a count, a list of names
 * — so a phase's decision is asserted on its own, before any bind is
 * emitted and without running the program. Test-side macros over the
 * `private[okay]` compiler; the block is never evaluated.
 */
object DirectProbe:

  /** the block after the defer pre-pass, shown — no bind emitted yet */
  inline def deferred[F[_], A](inline block: DirectCtx[F] ?=> A)(using inline d: Deferral): String =
    ${ deferredImpl[F, A]('block, 'd) }

  /** how many marks the mark analysis finds in the block */
  inline def marks[F[_], A](inline block: DirectCtx[F] ?=> A): Int =
    ${ marksImpl[F, A]('block) }

  /** the element type a value of V could RUN at as a bare statement of a
   * block over F, shown; None when it could not */
  inline def runnable[F[_], V]: Option[String] =
    ${ runnableImpl[F, V] }

  /** would a statement of type V be a silent drop in a block over F? */
  inline def dropped[F[_], V]: Boolean =
    ${ droppedImpl[F, V] }

  /** the names of the leading vals the parallel analysis would spawn together */
  inline def independentRun[F[_], A](inline block: DirectCtx[F] ?=> A): List[String] =
    ${ independentRunImpl[F, A]('block) }

  /** the value slots the block's expression hoists, shown, left to right */
  inline def slots[F[_], A](inline block: DirectCtx[F] ?=> A): List[String] =
    ${ slotsImpl[F, A]('block) }

  /** one compiler over the call site's Monad[F], in the probe's own Quotes */
  @scala.annotation.publicInBinary
  private[okay] def compiler[F[_] : Type](eager: Boolean)(using q: Quotes): DirectCompiler[F] =
    val M = Expr.summon[Monad[F]].getOrElse(
      q.reflect.report.errorAndAbort("DirectProbe: no Monad[F] at the call site"))
    new DirectCompiler[F](q, Type.of[F], M, eager, false)

  @scala.annotation.publicInBinary
  private[okay] def deferredImpl[F[_] : Type, A: Type](block: Expr[DirectCtx[F] ?=> A], d: Expr[Deferral])
                                                (using q: Quotes): Expr[String] =
    val eager = { import q.reflect.*; d.asTerm.tpe <:< TypeRepr.of[Deferral.Eager.type] }
    val body = Direct.blockBody[F, A](block)
    val c = compiler[F](eager)
    import c.q.reflect.*
    Expr(c.deferSelfCalls(body.asTerm.changeOwner(Symbol.spliceOwner)).show)

  @scala.annotation.publicInBinary
  private[okay] def marksImpl[F[_] : Type, A: Type](block: Expr[DirectCtx[F] ?=> A])(using Quotes): Expr[Int] =
    val body = Direct.blockBody[F, A](block)
    val c = compiler[F](false)
    import c.q.reflect.*
    var n = 0
    val probe = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case t: Term if c.asMark(t).isDefined => n += 1
          case _ => ()
        super.traverseTree(tree)(owner)
    probe.traverseTree(body.asTerm)(Symbol.spliceOwner)
    Expr(n)

  @scala.annotation.publicInBinary
  private[okay] def runnableImpl[F[_] : Type, V: Type](using Quotes): Expr[Option[String]] =
    val c = compiler[F](false)
    import c.q.reflect.*
    Expr(c.runnableElemT(TypeRepr.of[V]).map(_.show))

  @scala.annotation.publicInBinary
  private[okay] def droppedImpl[F[_] : Type, V: Type](using Quotes): Expr[Boolean] =
    val c = compiler[F](false)
    import c.q.reflect.*
    Expr(c.discardedMonadic(TypeRepr.of[V]))

  @scala.annotation.publicInBinary
  private[okay] def independentRunImpl[F[_] : Type, A: Type](block: Expr[DirectCtx[F] ?=> A])
                                                      (using Quotes): Expr[List[String]] =
    val body = Direct.blockBody[F, A](block)
    val c = compiler[F](false)
    import c.q.reflect.*
    val stats = c.stripped(body.asTerm) match
      case Block(stats, _) => stats
      case _ => Nil
    Expr(c.independentRun(stats).map(_._1.name))

  @scala.annotation.publicInBinary
  private[okay] def slotsImpl[F[_] : Type, A: Type](block: Expr[DirectCtx[F] ?=> A])(using Quotes): Expr[List[String]] =
    val body = Direct.blockBody[F, A](block)
    val c = compiler[F](false)
    import c.q.reflect.*
    // the block's one expression, out of the empty Block the typer may wrap it in
    def expr(t: Term): Term = c.stripped(t) match
      case Block(Nil, e) => expr(e)
      case e => e
    Expr(c.spineSlots(expr(body.asTerm)).map(_._1.map(_.show)).getOrElse(Nil))
