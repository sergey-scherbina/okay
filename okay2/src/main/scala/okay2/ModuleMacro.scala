package okay2

import scala.reflect.macros.blackbox

/**
 * What a module installed, for a container that wants values BY CLASS
 * (the Scala 3 core's `Installed`, specs/di.md stage 2): the name is
 * the plan's, the class is the erased one — an alias role (`type
 * Primary = RDb`) exports under its underlying class and keeps its
 * role in the name — and the value is what the scope built.
 */
final case class Installed(name: String, cls: Class[_], value: Any)

/**
 * THE PLAN IS THE TYPE, read by a Scala 2 def macro. A module's `F` is
 * the curried chain `A => B => … => X` (Scala 2 has no context
 * function, so the chain is plain `Function1`s — the same chain
 * `Providing.and` composes), outer to inner in acquisition order. The
 * Scala 3 core walks `F[Marker]` with quotes; here the prefix's type
 * gives `F` as a type lambda, `appliedType` applies it to the marker,
 * and each level is dealiased on its own — a composed `F[G[X]]` is a
 * projection inside a projection, and one `dealias` opens one of them.
 *
 * Names are read WITHOUT dealiasing the parameter itself
 * (the TypeRef's own symbol): an alias role shows as itself, which is what
 * the Scala 3 core's opaque qualifier gets from its symbol, and an
 * applied capability keeps its arguments (`New[Conn]`).
 */
object ModuleMacro {

  private def chain(c: blackbox.Context)(end: c.Type): List[c.Type] = {
    import c.universe._
    val module = c.prefix.actualType.baseType(symbolOf[Module[Any]])
    val f = module.typeArgs match {
      case List(f) => f
      case _ => c.abort(c.enclosingPosition, s"Module: cannot read F off ${c.prefix.actualType}")
    }
    val fn1 = symbolOf[Function1[Any, Any]]
    def walk(t: Type, acc: List[Type]): List[Type] = t.dealias match {
      case d if d =:= end => acc.reverse
      case TypeRef(_, sym, List(a, rest)) if sym == fn1 => walk(rest, a :: acc)
      case d if !(d eq t) => walk(d, acc)
      case other => c.abort(c.enclosingPosition,
        s"Module: expected a chain of functions ending in $end, found $other")
    }
    walk(appliedType(f.typeConstructor, List(end)), Nil)
  }

  private def name(c: blackbox.Context)(t: c.Type): String = {
    import c.universe._
    // the symbol of the TypeRef as written: an alias's own, not its target's
    def direct(t: Type): String = t match {
      case TypeRef(_, sym, _) => sym.name.decodedName.toString
      case other => other.typeSymbol.name.decodedName.toString
    }
    t.typeArgs match {
      case Nil => direct(t)
      case args => args.map(direct).mkString(s"${direct(t)}[", ", ", "]")
    }
  }

  private def names(c: blackbox.Context): List[String] = {
    import c.universe._
    chain(c)(typeOf[Module.Marker]).map(t => name(c)(t))
  }

  def plan(c: blackbox.Context): c.Tree = {
    import c.universe._
    q"_root_.scala.Vector[_root_.java.lang.String](..${names(c)})"
  }

  def shadowed(c: blackbox.Context): c.Tree = {
    import c.universe._
    val p = names(c)
    q"_root_.scala.Vector[_root_.java.lang.String](..${p.diff(p.distinct).distinct})"
  }

  /**
   * Generates `m.build.map(p => p.run[Vector[Installed]]((x1: A) => (x2: B) => … Vector(Installed(…, x1), …)))`.
   * Every parameter is typed from the chain, so the typer — not a cast
   * — checks the generated body against `F[Vector[Installed]]`.
   */
  def exports(c: blackbox.Context): c.Tree = {
    import c.universe._
    val out = typeOf[Vector[Installed]]
    val levels = chain(c)(out).map(t => (t, TermName(c.freshName("x"))))
    val installed = levels.map { case (t, x) =>
      q"_root_.okay2.Installed(${name(c)(t)}, _root_.scala.Predef.classOf[$t], $x)"
    }
    val body = levels.foldRight[Tree](q"_root_.scala.Vector[_root_.okay2.Installed](..$installed)") {
      case ((t, x), inner) => q"($x: $t) => $inner"
    }
    q"${c.prefix}.build.map(_.run[$out]($body))"
  }
}
