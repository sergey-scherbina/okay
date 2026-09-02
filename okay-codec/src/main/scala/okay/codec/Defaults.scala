package okay.codec

import scala.quoted.*

/**
 * The first of the two macros this library allows itself (Staged is
 * the other, the fold mode specs/codecs.md promised), and the reason
 * here is structural: Mirrors carry labels and types but NOT defaults — those
 * live as `<init>$default$N` methods on the companion, reachable only
 * by reflection. This macro reads what the compiler already wrote and
 * hands back ordinary values; nothing downstream knows a macro ran.
 *
 * A default that cannot be CALLED at decode time — one that takes
 * value parameters (`b: Int = a`) or type parameters — is honestly
 * None, not a guess: decode refuses the absent field as before.
 */
object Defaults {

  inline def of[A]: Vector[Option[() => Any]] = ${ ofImpl[A] }

  def ofImpl[A: Type](using Quotes): Expr[Vector[Option[() => Any]]] =
    import quotes.reflect.*
    val sym = TypeRepr.of[A].typeSymbol
    val comp = sym.companionModule
    val params =
      if !sym.isClassDef || sym.primaryConstructor.isNoSymbol then Nil
      else sym.primaryConstructor.paramSymss.flatten.filterNot(_.isTypeParam)
    val exprs: List[Expr[Option[() => Any]]] = params.zipWithIndex.map { (_, i) =>
      comp.methodMember("$lessinit$greater$default$" + (i + 1)) match
        case m :: Nil if m.paramSymss.isEmpty =>
          Ref(comp).select(m).asExprOf[Any] match
            case call => '{ Some(() => $call) }
        case _ => '{ None }
    }
    '{ Vector(${ Varargs(exprs) }*) }
}
