package okay
package macros

import scala.quoted.*

/** the macros behind `derives Effect` / `TypeableK.derived` and `Row.flat` (okay-macros-package) */
@scala.annotation.publicInBinary private[okay] object AnswersMacros:

  /**
   * The check is the reason this is a macro and not one line.
   *
   * A `ClassTag` of a UNION is its LUB, and a LUB is useless as a
   * test: measured, `ClassTag[(Choose + Writer % String)[Any]]` is
   * `interface java.io.Serializable` and `ClassTag[(Db + Writer %
   * String)[Any]]` is `interface scala.reflect.Enum` — classes every
   * operation in the program matches. A row derived this way would
   * send every operation left and say nothing, which is the failure
   * mode this library refuses on principle.
   *
   * A blacklist of such classes is whack-a-mole (the two above are
   * already different). The type says it exactly: refuse a union,
   * accept a signature. And a row does not need this anyway — the
   * generic instance below handles a composite row correctly, by
   * testing the parts.
   */
  def derivedImpl[F[_] : Type](using Quotes): Expr[Effect[F]] =
    import quotes.reflect.*
    val body = TypeRepr.of[F].dealias match
      case tl: TypeLambda => tl.resType.dealias
      case other => other.appliedTo(TypeRepr.of[Any]).dealias
    body match
      case OrType(_, _) =>
        report.errorAndAbort(
          "TypeableK.derived is for ONE signature, and this is a row.\n" +
          "The erasure of a union is its LUB, a class every operation matches, so the\n" +
          "split would send all of them left and say nothing.\n" +
          "A row needs no instance of its own: let each signature derive one, and the\n" +
          "row split will find them.")
      case _ =>
        // the signature's class with every argument a wildcard — what
        // `x.isInstanceOf[Users[?]]` tests. Emitted as a class of its
        // own per `derives` site (one per signature) so that the test
        // is a CONSTANT-class `instanceof` in the bytecode, where
        // `ByClass` reads its class from a field and calls
        // `Class.isInstance` (typeablek-instanceof: the residual of
        // handler-fusion-flat, 5.6% on a lane that is nothing but
        // dispatch). `ByClass` stays for `typeableK(cls)`, whose class
        // is a run-time value.
        val erased = body match
          case AppliedType(tycon, args) => AppliedType(tycon, args.map(_ => TypeBounds.empty))
          case other => other
        if !erased.typeSymbol.isClassDef then
          report.errorAndAbort(s"TypeableK.derived needs a class to test for, and ${erased.show} is not one")
        erased.asType match
          case '[t] => '{ new Effect[F] { def test(x: Any): Boolean = x.isInstanceOf[t] } }
