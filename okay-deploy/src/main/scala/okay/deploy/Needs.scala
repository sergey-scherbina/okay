package okay.deploy

import okay.Module
import scala.quoted.*

/**
 * The root module's unresolved inputs are what a deployment declares
 * — the ones a PLACE provides
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
enum Needs[A]:
  /** what the PLACE must provide: a database, a volume, a certificate */
  case Place[A](need: Need) extends Needs[A]
  /**
   * What the PROCESS brings and no target can give it — a timer, a
   * scheduler, a clock (di-dogfood found this, needs-runtime: a real
   * root's inputs are MIXED). Without it the only answers were a lie
   * (declaring a `Need` for something no place provisions) or losing
   * the guarantee that an undeclared input stops the build.
   */
  case Runtime[A]() extends Needs[A]

object Needs:
  /** the usual declaration: this capability is the place's business */
  def apply[A](need: Need): Needs[A] = Place(need)
  /** this one is the runtime's, and the deployment says nothing about it */
  def runtime[A]: Needs[A] = Runtime()

  /** a timer and a scheduler are the process's own, in every
   * application, so the declaration lives here rather than in each */
  given Needs[okay.Timer] = Runtime()
  given Needs[okay.Scheduler] = Runtime()

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
          case Some(n) => n
          case None => report.errorAndAbort(
            s"the root module needs ${t.show} and the deployment does not know what that is: " +
            s"declare `given Needs[${t.show}] = Needs(Need.…)` where the capability is defined, " +
            s"or `= Needs.runtime` if the process brings it and no place can")
    }
    // the runtime ones are declared so the error above stays honest,
    // and dropped here: a deployment says nothing about a timer
    '{ ${ Expr.ofList(needs) }.collect { case Needs.Place(n) => n }.toVector }
