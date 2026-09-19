package okay

import scala.quoted.*

/**
 * EMISSION: the only place that writes the monad's own words —
 * `M.pure`, `M.flatMap`, `M.fmap` — so the target of the whole
 * rewrite (direct-flatmap-emission: plain `F[T]` terms, a bind is a
 * flatMap call, the pure tail is `pure`, no Cont layer between the
 * block and its monad) is these few functions and nowhere else.
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

  /** M.pure(t) at t's type */
  def pureF(t: Term): Term =
    tpe2(t.tpe.widen) { [T] => (tT: Type[T]) ?=>
      '{ $M.pure[T](${ t.asExprOf[T] }) }.asTerm
    }

  /** fa.flatMap(v => body(v)) — body built from a reference to v,
   * returning an F[resTpe] term */
  def bind(fa: Term, vTpe: TypeRepr, resTpe: TypeRepr)(body: Term => Term): Term =
    tpe2(vTpe.widen) { [T] => (tT: Type[T]) ?=>
      tpe2(resTpe.widen) { [B] => (tB: Type[B]) ?=>
        val fa2 = fa.asExprOf[F[T]]
        '{
          $M.flatMap[T]($fa2)[B]((v: T) =>
            ${ body('v.asTerm).changeOwner(Symbol.spliceOwner)
                 .asExprOf[F[B]] })
        }.asTerm
      }
    }

  /** the compiled term at exactly F[tpe] — a no-op when the types
   * already agree; a real fmap when a branch narrows (F need not
   * be covariant, so ascription cannot widen it) */
  def asFAt(o: Out, tpe: TypeRepr): Term =
    tpe2(tpe.widen) { [T] => (tT: Type[T]) ?=>
      o match
        case Out.Pure(p) => '{ $M.pure[T](${ p.asExprOf[T] }) }.asTerm
        case Out.Eff(f, e) =>
          if e.widen =:= tpe.widen then f
          else tpe2(e.widen) { [V] => (tV: Type[V]) ?=>
            // a STATEMENT's value is discarded (Scala's own rule for a
            // Unit position) — said so, not cast; anything else is an
            // upcast the compiler vouches for
            if TypeRepr.of[T] =:= TypeRepr.of[Unit] then
              '{ $M.fmap[V, Unit](${ f.asExprOf[F[V]] }, (_: V) => ()) }.asTerm
            else
              val ev = upcast[V, T]
              '{ $M.fmap[V, T](${ f.asExprOf[F[V]] }, (x: V) => $ev(x)) }.asTerm
          }
    }

  def asF(o: Out): Term = o match
    case Out.Eff(f, _) => f
    case Out.Pure(p) => pureF(p)
