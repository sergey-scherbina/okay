package okay

import okay.one.row.{In, Pure as OnePure}
import scala.quoted.*

/**
 * THE PLACE OF AN EFFECT, BY KEY (specs/keyed-root.md): an effect's operations are performed at a place by key — the
 * kernel's `In.At` — and its handler says the key is its own (`Handler.at`), so a program is the kernel's tree with no
 * row to fit and no paths to carry: a bind is one node. The key is the effect's constructor CLASS (`Writer`, `State`,
 * `Async`), found at compile time, one `In.At` a class shared by every program (`ClassValue`); an effect with no class
 * of its own (`Produce = Id`) is keyed by its alias's name. Two effects of one constructor in one program — `Writer %
 * Int` beside `Writer % String` — share the key: the nearer handler answers both (okay-freer's union had the same
 * reading, `Distinct` refusing it).
 */
final class KeyOf[E[_]] private[okay] (val in: In.At[E, OnePure]):
  inline def ref: AnyRef = in.ref
  /** the place of this key in any row: a place by key is in every row */
  inline def at[R <: okay.one.row.Row]: In.At[E, R] = in.asInstanceOf[In.At[E, R]]
  override def toString: String = s"KeyOf(${in.ref})"

object KeyOf:
  private object byClass extends ClassValue[KeyOf[?]]:
    protected def computeValue(c: Class[?]): KeyOf[?] = KeyOf[[x] =>> Any](In.At(c))
  private val byName = java.util.concurrent.ConcurrentHashMap[String, KeyOf[?]]()
  /** the key of the effect whose constructor is the class `c` */
  def of[E[_]](c: Class[?]): KeyOf[E] = byClass.get(c).asInstanceOf[KeyOf[E]]
  /** the key of an effect with no class of its own, by its name */
  def named[E[_]](n: String): KeyOf[E] =
    byName.computeIfAbsent(n, n => KeyOf[[x] =>> Any](In.At(n))).asInstanceOf[KeyOf[E]]
  /** a fresh key: an effect named by its installation (okay.Lexical) */
  def fresh[E[_]](): KeyOf[E] = KeyOf[E](In.At(new AnyRef))
  /** found at compile time from the effect's type */
  inline given derived[E[_]]: KeyOf[E] = ${ derivedImpl[E] }

  def derivedImpl[E[_]: Type](using q: Quotes): Expr[KeyOf[E]] =
    import q.reflect.*
    val e = TypeRepr.of[E]
    val applied = e.appliedTo(TypeRepr.of[Any]).dealias.simplified
    val head = applied match
      case AppliedType(t, _) => t.dealias
      case t => t
    val sym = head.typeSymbol
    val boring = Set(defn.AnyClass, defn.AnyRefClass, defn.ObjectClass, defn.NothingClass, defn.NullClass)
    if sym.isClassDef && !boring(sym) && !sym.flags.is(Flags.Param) then
      val cls = Literal(ClassOfConstant(head.typeSymbol.typeRef.appliedTo(head.typeSymbol.typeMembers.filter(_.isTypeParam).map(_ => TypeBounds.empty))))
      '{ KeyOf.of[E](${ cls.asExprOf[Class[?]] }) }
    else
      // an effect with no class of its own (`Produce = Id`): the name of the first alias its type goes through
      def aliasOf(t: TypeRepr): Option[String] = t match
        case l: TypeLambda => aliasOf(l.resType)
        case AppliedType(tc: TypeRef, _) if tc.typeSymbol.isAliasType => Some(tc.typeSymbol.fullName)
        case tr: TypeRef if tr.typeSymbol.isAliasType && !tr.typeSymbol.flags.is(Flags.Param) => Some(tr.typeSymbol.fullName)
        case _ => None
      val own = aliasOf(e)
      own match
        case Some(n) => '{ KeyOf.named[E](${ Expr(n) }) }
        case None =>
          report.errorAndAbort(s"no key for the effect ${e.show}: its constructor is not known here — take it as `using okay.KeyOf[...]` from where it is")
