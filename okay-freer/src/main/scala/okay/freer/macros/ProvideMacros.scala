package okay.freer
package macros

import okay.*

import scala.annotation.tailrec
import scala.quoted.*

/** the macros behind `Module.plan` and `Module.exports` (okay-macros-package) */
@scala.annotation.publicInBinary private[okay] object ProvideMacros:

  /** `F[Marker]` dealiased is `ContextFunction1[A, ContextFunction1[B, … Marker]]`;
   * walk it to the marker, naming each parameter */
  /** the chain `A ?=> B ?=> … ?=> End` as its parameters, outer first,
   * each with the type that remains after it */
  private def chain(using q: Quotes)(t: q.reflect.TypeRepr, end: q.reflect.TypeRepr)
      : List[(q.reflect.TypeRepr, q.reflect.TypeRepr)] =
    import q.reflect.*
    @tailrec def walk(t: TypeRepr, acc: List[(TypeRepr, TypeRepr)]): List[(TypeRepr, TypeRepr)] = t.dealias match
      case AppliedType(fn, List(a, rest)) if fn.typeSymbol.name.startsWith("ContextFunction") =>
        walk(rest, (a, rest) :: acc)
      case t if t =:= end => acc.reverse
      case other => report.errorAndAbort(
        s"Module: expected a chain of context functions ending in ${end.show}, found ${other.show}")
    walk(t, Nil)

  def planImpl[F[_] : Type](using Quotes): Expr[Vector[String]] =
    import quotes.reflect.*
    // an APPLIED capability keeps its argument: a prototype reads as
    // `New[Conn]`, not `New`, which is the difference between a plan
    // and a list of type constructors (di-prototype)
    def name(using q: Quotes)(t: q.reflect.TypeRepr): String =
      import q.reflect.*
      t.dealias match
        case AppliedType(tc, args) =>
          s"${tc.typeSymbol.name}[${args.map(a => a.typeSymbol.name).mkString(", ")}]"
        case other => other.typeSymbol.name
    val names = chain(TypeRepr.of[F[Module.Marker]], TypeRepr.of[Module.Marker]).map(t => name(t._1))
    val list = Expr(names)
    '{ $list.toVector }

  /**
   * Generates `m.build.map(p => p((a: A) ?=> (b: B) ?=> … List(Installed(…, a), Installed(…, b)).toVector))`.
   * Each level is quoted with its own parameter type and ascribed to
   * the type the chain says remains — `asExprOf` is a CHECK at
   * expansion time, not a runtime cast, and it fails the expansion
   * if the generated body's type ever disagrees with the chain's.
   */
  def exportsImpl[F[_] : Type](m: Expr[Module[F]])(using Quotes): Expr[Vector[Installed] ! Resource] =
    import quotes.reflect.*
    val end = TypeRepr.of[Vector[Installed]]
    val levels = chain(TypeRepr.of[F[Vector[Installed]]], end)
    def body(ls: List[(TypeRepr, TypeRepr)], acc: List[Expr[Installed]]): Expr[Any] = ls match
      case Nil => '{ ${ Expr.ofList(acc.reverse) }.toVector }
      case (a, rest) :: more =>
        val name = Expr(a.typeSymbol.name)
        // the erased class: an opaque type's is its underlying's
        val cls = Literal(ClassOfConstant(a.dealias)).asExprOf[Class[?]]
        a.asType match
          case '[at] => rest.asType match
            case '[rt] =>
              '{ (x: at) ?=> ${ body(more, '{ Installed($name, $cls, x) } :: acc).asExprOf[rt] } }
    val collect = body(levels, Nil).asExprOf[F[Vector[Installed]]]
    '{ $m.build.map(p => p[Vector[Installed]]($collect)) }
