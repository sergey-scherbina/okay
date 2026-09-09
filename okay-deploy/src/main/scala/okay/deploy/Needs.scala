package okay.deploy

import okay.Module
import scala.quoted.*

/**
 * The root module's unresolved inputs ARE the deployment's needs
 * (specs/di.md, stage 3). An application's root is a module still
 * waiting for what only the PLACE can give — `Pg ?=> Files ?=> Module[…]`
 * — and each such input type says, once, what it is in deployment
 * terms: `given Needs[Pg] = Needs(Need.Database(Engine.Postgres, "16", "shop"))`.
 * `Needs.of[Root]` walks the root's type at compile time, summons one
 * `Needs` per input, and hands back the `Vector[Need]` a `Service`
 * carries — so the database is said in the type, and the deployment
 * reads it there. An input with no `Needs` is a compile error naming
 * the type: a need the module has and the deployment does not know.
 */
final case class Needs[A](need: Need)

object Needs:
  /** the needs of a root module type, read off its unresolved inputs */
  inline def of[Root]: Vector[Need] = ${ ofImpl[Root] }

  def ofImpl[Root: Type](using Quotes): Expr[Vector[Need]] =
    import quotes.reflect.*
    val module = TypeRepr.of[Module[?]]
    def inputs(t: TypeRepr, acc: List[TypeRepr]): List[TypeRepr] = t.dealias match
      case AppliedType(fn, args) if fn.typeSymbol.name.startsWith("ContextFunction") =>
        inputs(args.last, acc ++ args.init)
      case t if t <:< module => acc
      case other => report.errorAndAbort(
        s"Needs.of: a root is a chain of context functions ending in a Module; found ${other.show}")
    val needs = inputs(TypeRepr.of[Root], Nil).map { t =>
      t.asType match
        case '[a] => Expr.summon[Needs[a]] match
          case Some(n) => '{ $n.need }
          case None => report.errorAndAbort(
            s"the root module needs ${t.show} and the deployment does not know what that is: " +
            s"declare `given Needs[${t.show}] = Needs(Need.…)` where the capability is defined")
    }
    '{ ${ Expr.ofList(needs) }.toVector }
