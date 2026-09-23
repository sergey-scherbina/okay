package okay
package macros

import scala.quoted.*

/**
 * EMISSION: the only place that writes the monad's own words —
 * `M.pure`, `M.flatMap`, `M.fmap` — so the target of the whole
 * rewrite (direct-flatmap-emission: plain `F[T]` terms, a bind is a
 * flatMap call, the pure tail is `pure`, no Cont layer between the
 * block and its monad) is these few functions and nowhere else.
 *
 * TWO ROADS to the same call (direct-staged, 2026-09-22). For a
 * STAGED block the call is built by `Select` on the hoisted
 * instance's PRECISE type — the given's own class, which
 * `DirectCompiler.pipeline` keeps on the val — so an `override
 * inline` member is what it names, and the inliner reduces the bind
 * at compile time. A quote types `$M` at `Monad[F]` and names the
 * TRAIT's `flatMap`, a virtual call that keeps its closures: measured
 * on the staged carrier as 152 568 B/op against the hand-written
 * 84 568 on the same block. Every other block keeps the quote road,
 * unchanged — the precise type broke two carriers when tried on all
 * (pipeline says which); the Free road was tried and refuted
 * (direct-inline-bind-free: bytes 0.97, time 1.00).
 */
private[okay] trait DirectEmit[F[_]] extends DirectPhase[F]:
  import q.reflect.*

  /** the compiler's own evidence that V is a T, summoned at macro
   * time and spliced: the generated code upcasts through it, so no
   * `asInstanceOf` is ever emitted — the macro checked V <:< T on
   * the TypeReprs before asking, and a refusal here would be its bug */
  def upcast[V: Type, T: Type]: Expr[V <:< T] =
    Expr.summon[V <:< T].getOrElse(
      report.errorAndAbort(s"direct: ${Type.show[V]} is not a ${Type.show[T]} (macro bug)"))

  /** the quote road for every block but a staged one — see
   * DirectCompiler.pipeline for the two carriers the precise type broke */
  private lazy val viaQuote: Boolean = stage.isEmpty

  private def m(name: String): Term = Select.unique(M.asTerm, name)

  /** a lambda QUOTED, as the old road built it: the quote machinery
   * owns the body under the lambda, where a reflected `Lambda` with
   * `changeOwner` left a `$anonfun` LambdaLift could not find
   * (`Dependencies.narrowLogicOwner`, the whole okay-direct suite) —
   * only the CALL around it is reflected, for the inline member */
  private def lambda(argTpe: TypeRepr, resTpe: TypeRepr)(body: Term => Term): Term =
    tpe2(argTpe) { [T] => (tT: Type[T]) ?=>
      tpe2(resTpe) { [R] => (tR: Type[R]) ?=>
        '{ (v: T) => ${ body('v.asTerm).changeOwner(Symbol.spliceOwner).asExprOf[R] } }.asTerm
      }
    }

  /** an argument ASCRIBED to its widened type, as the quote road's
   * `asExprOf[T]` does: the callee is inline now, and the inliner
   * types a by-value argument's proxy at the argument's own type — a
   * selection on an unstable prefix (`wire[Env].uid`, type `Env#uid`)
   * is not a type a val can have ("cannot establish a reference",
   * TestDirectDoors) */
  private def at(t: Term, tpe: TypeRepr): Term = Typed(t, Inferred(tpe))

  /** M.pure(t) at t's type */
  def pureF(t: Term): Term = pureAt(t, t.tpe.widen)

  private def pureAt(t: Term, tpe: TypeRepr): Term =
    if viaQuote then
      tpe2(tpe) { [T] => (tT: Type[T]) ?=> '{ $M.pure[T](${ t.asExprOf[T] }) }.asTerm }
    else Apply(TypeApply(m("pure"), List(Inferred(tpe))), List(at(t, tpe)))

  /** fa.flatMap(v => body(v)) — body built from a reference to v,
   * returning an F[resTpe] term. `flatMap` is an extension on the
   * instance — `flatMap[A](fa)[B](f)` as a method */
  def bind(fa: Term, vTpe: TypeRepr, resTpe: TypeRepr)(body: Term => Term): Term =
    val t = vTpe.widen
    val b = resTpe.widen
    if viaQuote then
      tpe2(t) { [T] => (tT: Type[T]) ?=>
        tpe2(b) { [B] => (tB: Type[B]) ?=>
          val fa2 = fa.asExprOf[F[T]]
          '{
            $M.flatMap[T]($fa2)[B]((v: T) =>
              ${ body('v.asTerm).changeOwner(Symbol.spliceOwner).asExprOf[F[B]] })
          }.asTerm
        }
      }
    else
      val lam = lambda(t, TypeRepr.of[F].appliedTo(b))(body)
      Apply(TypeApply(Apply(TypeApply(m("flatMap"), List(Inferred(t))),
        List(at(fa, TypeRepr.of[F].appliedTo(t)))), List(Inferred(b))), List(lam))

  /** the compiled term at exactly F[tpe] — a no-op when the types
   * already agree; a real fmap when a branch narrows (F need not
   * be covariant, so ascription cannot widen it) */
  def asFAt(o: Out, tpe: TypeRepr): Term =
    val t = tpe.widen
    o match
      case Out.Pure(p) => pureAt(p, t)
      case Out.Eff(f, e) =>
        val v = e.widen
        if v =:= t then f
        else if viaQuote then
          tpe2(t) { [T] => (tT: Type[T]) ?=>
            tpe2(v) { [V] => (tV: Type[V]) ?=>
              if TypeRepr.of[T] =:= TypeRepr.of[Unit] then
                '{ $M.fmap[V, Unit](${ f.asExprOf[F[V]] }, (_: V) => ()) }.asTerm
              else
                val ev = upcast[V, T]
                '{ $M.fmap[V, T](${ f.asExprOf[F[V]] }, (x: V) => $ev(x)) }.asTerm
            }
          }
        else
          // a STATEMENT's value is discarded (Scala's own rule for a
          // Unit position) — said so, not cast; anything else is an
          // upcast the compiler vouches for
          val fn =
            if t =:= TypeRepr.of[Unit] then lambda(v, t)(_ => Literal(UnitConstant()))
            else tpe2(v) { [V] => (tV: Type[V]) ?=>
              tpe2(t) { [T] => (tT: Type[T]) ?=>
                val ev = upcast[V, T]
                lambda(v, t)(x => Apply(Select.unique(ev.asTerm, "apply"), List(x)))
              }
            }
          Apply(TypeApply(m("fmap"), List(Inferred(v), Inferred(t))),
            List(at(f, TypeRepr.of[F].appliedTo(v)), fn))

  def asF(o: Out): Term = o match
    case Out.Eff(f, _) => f
    case Out.Pure(p) => pureF(p)
