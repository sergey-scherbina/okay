package okay
package macros

import scala.quoted.*
import scala.annotation.tailrec

/**
 * INDEPENDENT BINDS, RUN TOGETHER (specs/applicative-static.md,
 * stage 3; off unless `import Direct.parallelBinds.given`): which
 * leading run of vals may be spawned at once — bracket abstraction's
 * own question — and the flat spawn-all-then-join-all shape that
 * runs it. The analysis reads the mark's own argument first and
 * falls back to the compiled leaf through the knot.
 */
private[okay] trait DirectParallel[F[_]] extends DirectMarks[F] with DirectRow[F] with DirectEmit[F]:
  import q.reflect.*

  /**
   * A leaf this block may SPAWN — decided on the COMPILED leaf, not
   * on the syntax.
   *
   * The first cut matched the mark itself and found nothing: by the
   * time the macro sees `async(1).?` the inline expansion has
   * wrapped it in `Inlined` nodes carrying `$proxy` bindings, which
   * `stripped` does not go through, so `asMark` answered None on
   * every leaf and the whole feature was silently off (caught by a
   * fork COUNT of 0, which is why that assertion exists).
   *
   * `compile` already knows how to get through all of it, and what
   * it hands back is the program this block will bind. If that
   * program's type is `X ! Async` then it can be spawned, and
   * asking the type is both simpler and more honest than asking the
   * syntax.
   *
   * THE LIMIT THAT PUT ON v1 IS GONE (direct-parallel-wider-rows):
   * for a block over a WIDER row the compiled leaf has already been
   * lifted by `Row.into`, so its type is `X ! (Async + …)` and
   * it is not spawnable — the import used to do nothing there,
   * quietly. `markedProgram` below reads the mark's own argument
   * first, which is the program the author wrote, and only falls
   * back to the compiled leaf. The compiled leaf remains the road
   * for a mark shape the walk cannot take apart.
   */
  def spawnableLeaf(rhs: Term): Option[(Term, TypeRepr)] =
    if !hasMark(rhs) then None
    else
      // the mark's OWN argument first (a wider row still has Async
      // leaves), and the compiled leaf as the fallback
      markedProgram(rhs).orElse(
        compile(rhs) match
          case Out.Eff(c, e) => Some((c, e.widen))
          case Out.Pure(_) => None
      ).filter((c, e) => isAsyncProgram(c, e))

  /** is this term a program of EXACTLY the Async row? */
  def isAsyncProgram(c: Term, e: TypeRepr): Boolean =
    tpe2(e.widen) { [X] => (tX: Type[X]) ?=> c.tpe.widen <:< TypeRepr.of[X ! Async] }

  /**
   * THE MARK'S OWN ARGUMENT, before `markTerm` narrows it into this
   * block's row (direct-parallel-wider-rows).
   *
   * `compile` hands back a leaf already lifted by `Row.into`,
   * so in a block over `Async + Throws` its type is
   * `X ! Async + Throws` and `Async.spawn` will not take it — the
   * import did nothing there, quietly, and v1 said so. The program
   * the author WROTE is still `X ! Async`, and it is reachable: the
   * obstacle was never the narrowing, it was that inline expansion
   * wraps a leaf in `Inlined` nodes carrying `$proxy` bindings,
   * which `stripped` does not remove.
   *
   * `compile` already goes through them, by turning such an
   * `Inlined` into a `Block`. The same walk here KEEPS the bindings
   * around the mark's argument — `Block(bindings, program)` is a
   * term of the program's own type — so the extracted leaf is
   * self-contained and can be spawned where it stands.
   */
  def markedProgram(rhs: Term): Option[(Term, TypeRepr)] =
    def go(t: Term): Option[Term] = stripped(t) match
      case Inlined(_, bindings, inner) if bindings.nonEmpty =>
        go(Block(bindings, inner))
      case Block(stats, expr) =>
        go(expr).map(m => if stats.isEmpty then m else Block(stats, m))
      case other => asMark(other).map(stripped)
    go(rhs).flatMap { m =>
      m.tpe.widen.dealias.baseType(freeClass) match
        case AppliedType(_, List(_, e)) => Some((m, e.widen))
        case _ => None
    }

  /** the maximal leading run of vals that may be spawned together */
  def independentRun(stats: List[Statement]): List[(ValDef, Term, TypeRepr)] =
    @tailrec def go(rest: List[Statement], bound: Set[Symbol],
           acc: List[(ValDef, Term, TypeRepr)]): List[(ValDef, Term, TypeRepr)] =
      rest match
        case (vd @ ValDef(_, _, Some(rhs))) :: tail
          if !vd.symbol.flags.is(Flags.Lazy) && !vd.symbol.flags.is(Flags.Mutable) =>
          spawnableLeaf(rhs) match
            case Some((m, e)) if !mentionsAny(rhs, bound) =>
              go(tail, bound + vd.symbol, (vd, m, e) :: acc)
            case _ => acc.reverse
        case _ => acc.reverse
    go(stats, Set.empty, Nil)

  /** `async(Async.spawn(m))`, with its Fiber element type */
  def spawnOf(m: Term, e: TypeRepr, sched: Expr[Scheduler]): (Term, TypeRepr) =
    tpe2(e) { [X] => (tX: Type[X]) ?=>
      ('{ okay.async(okay.Async.spawn[X](${ m.asExprOf[X ! Async] })(using $sched)) }.asTerm,
        TypeRepr.of[Fiber[X]])
    }

  /** `fiber.joinAsync` */
  def joinOf(f: Term, e: TypeRepr): Term =
    tpe2(e) { [X] => (tX: Type[X]) ?=>
      '{ ${ f.asExprOf[Fiber[X]] }.joinAsync }.asTerm
    }

  /**
   * N spawns, then N joins in the order written — the FLAT shape,
   * which is `parAll`'s and not `Par`'s. The applicative spine was
   * measured at ~5x this at eight leaves because `app` is pairwise;
   * a macro holds the whole group, so it never has to be.
   *
   * Each fiber keeps its own element type, so nothing here casts.
   * The val keeps its symbol, re-bound to the join's value, exactly
   * as the sequential road does — a later def or assignment still
   * refers to it.
   */
  def parallelGroup(run: List[(ValDef, Term, TypeRepr)],
                    rest: List[Statement], expr: Term): Out =
    val sched = Expr.summon[Scheduler].getOrElse(report.errorAndAbort(
      "direct: `import Direct.parallelBinds.given` needs a Scheduler in scope — " +
        "it starts a fiber per independent bind (an `Async.spawn`), and there is no " +
        "given Scheduler here", run.head._1.pos))
    val resTpe = expr.tpe

    def joins(pairs: List[((ValDef, Term, TypeRepr), Term)]): Term =
      pairs match
        case Nil => asFAt(compileBlock(rest, expr), resTpe)
        case ((vd, _, e), fib) :: tail =>
          bind(markTerm(joinOf(fib, e), e, vd.pos), e, resTpe) { v =>
            Block(List(ValDef.copy(vd)(vd.name, vd.tpt, Some(v))), joins(tail))
          }

    def spawns(todo: List[(ValDef, Term, TypeRepr)],
               done: List[((ValDef, Term, TypeRepr), Term)]): Term =
      todo match
        case Nil => joins(done.reverse)
        case (leaf @ (vd, m, e)) :: tail =>
          val (sp, fibTpe) = spawnOf(m, e, sched)
          bind(markTerm(sp, fibTpe, vd.pos), fibTpe, resTpe) { f =>
            spawns(tail, (leaf, f) :: done)
          }

    Out.Eff(spawns(run, Nil), resTpe.widen)
