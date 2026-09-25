package okay2.http

import scala.reflect.macros.blackbox

/**
 * `route.of[C]`: the capture read and written as a case class. Scala 3
 * demands a `Mirror.ProductOf[C]` whose element types ARE the route's
 * tuple; here the macro reads C's constructor and refuses, at compile
 * time, unless its field types are the capture — `Unit` for no fields,
 * the value for one, the tuple for several — then writes the two
 * positional conversions. The names are the value-level half, checked
 * by `Route.Of.checked` at construction, as in Scala 3.
 */
object RouteMacro {

  def of[C: c.WeakTypeTag, A: c.WeakTypeTag](c: blackbox.Context): c.Expr[Route.Of[C, A]] = {
    import c.universe._
    val C = weakTypeOf[C].dealias
    val A = weakTypeOf[A].dealias
    val cls = C.typeSymbol
    if (!cls.isClass || !cls.asClass.isCaseClass)
      c.abort(c.enclosingPosition, s"of[$C]: a route maps onto a case class")
    val params = cls.asClass.primaryConstructor.asMethod.paramLists.headOption.getOrElse(Nil)
    val types = params.map(_.typeSignature.substituteTypes(cls.asClass.typeParams, C.typeArgs))
    val names = params.map(_.name.decodedName.toString)
    val expected: Type = types match {
      case Nil => typeOf[Unit]
      case one :: Nil => one
      case many => appliedType(definitions.TupleClass.seq(many.length - 1).toType.typeConstructor, many)
    }
    if (!(A =:= expected))
      c.abort(c.enclosingPosition,
        s"of[$C]: the route captures $A, and ${C.typeSymbol.name}'s fields ${names.mkString("(", ", ", ")")} are $expected")
    val a = TermName(c.freshName("a"))
    val x = TermName(c.freshName("c"))
    val accessors = names.map(n => q"$x.${TermName(n)}")
    val make = params.length match {
      case 0 => q"($a: $A) => new $C()"
      case 1 => q"($a: $A) => new $C($a)"
      case n => q"($a: $A) => new $C(..${(1 to n).map(i => q"$a.${TermName("_" + i)}")})"
    }
    val parts = accessors match {
      case Nil => q"($x: $C) => ()"
      case one :: Nil => q"($x: $C) => $one"
      case many => q"($x: $C) => (..$many)"
    }
    c.Expr[Route.Of[C, A]](
      q"_root_.okay2.http.Route.Of.checked[$C, $A](${c.prefix}, $make, $parts, _root_.scala.collection.immutable.Vector(..$names))")
  }
}
