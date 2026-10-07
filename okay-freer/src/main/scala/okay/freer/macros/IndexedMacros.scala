package okay.freer
package macros


import scala.quoted.*

/** the macro behind `TypeableI.derived` (okay-macros-package) */
@scala.annotation.publicInBinary private[okay] object IndexedMacros:

  def derivedImpl[F[_, _, +_] : Type](using Quotes): Expr[TypeableI[F]] =
    import quotes.reflect.*
    val body = TypeRepr.of[F].dealias match
      case tl: TypeLambda => tl.resType.dealias
      case other => other.appliedTo(List(TypeRepr.of[Any], TypeRepr.of[Any], TypeRepr.of[Any])).dealias
    body match
      case OrType(_, _) =>
        report.errorAndAbort(
          "TypeableI.derived is for ONE signature, and this is a row (F +~ G).\n" +
          "The erasure of a union is its LUB, a class every operation matches; let each\n" +
          "signature derive its own instance, and splitI will find it.")
      case _ =>
        val erased = body match
          case AppliedType(tycon, args) => AppliedType(tycon, args.map(_ => TypeBounds.empty))
          case other => other
        if !erased.typeSymbol.isClassDef then
          report.errorAndAbort(s"TypeableI.derived needs a class to test for, and ${erased.show} is not one (a match-typed member such as Unary[F] has none: write `new TypeableI[...] { def test(x: Any) = x.isInstanceOf[F[?]] }`)")
        erased.asType match
          case '[t] => '{ new TypeableI[F] { def test(x: Any): Boolean = x.isInstanceOf[t] } }
