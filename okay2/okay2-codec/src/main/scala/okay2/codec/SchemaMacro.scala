package okay2.codec

import scala.reflect.macros.blackbox

/**
 * `Schema.derived` for Scala 2: what okay-codec's `derived` reads from
 * a `Mirror`, and `Defaults` from the companion, read here from the
 * class itself.
 *
 *  - a CASE CLASS is an `SProduct`: its first parameter list by name, a
 *    schema thunk per field (`Schema.once(implicitly[...])`, so a
 *    recursive field is looked up lazily), `make` by the constructor,
 *    `parts` by `productElement`, and each parameter's declared default
 *    as the companion's `<init>$default$N` — applied to the type's own
 *    arguments, so a GENERIC product's defaults are callable too (Scala
 *    3's macro holds None there; see specs/okay2.md).
 *  - a CASE OBJECT is an `SProduct` with no fields.
 *  - a SEALED trait or abstract class is an `SSum`: its known direct
 *    subclasses in declaration order, `caseOf` by a type test.
 *
 * Types under `scala.` and `java.` are not derived: `Option` is the
 * option instance, never the sum `Some | None`.
 */
object SchemaMacro {

  def derive[A: c.WeakTypeTag](c: blackbox.Context): c.Expr[Schema[A]] = {
    import c.universe._
    val A = weakTypeOf[A].dealias
    val sym = A.typeSymbol
    def refuse(why: String): Nothing =
      c.abort(c.enclosingPosition, s"Schema.derived[$A]: $why")
    if (!sym.isClass) refuse("not a class")
    val cls = sym.asClass
    val full = cls.fullName
    if (full.startsWith("scala.") || full.startsWith("java."))
      refuse("a standard type has its own instance or none")

    val S = q"_root_.okay2.codec.Schema"
    val ST = tq"_root_.okay2.codec.Schema"
    val prefix = A match {
      case TypeRef(pre, _, _) => pre
      case _ => NoPrefix
    }
    def ref(s: Symbol): Tree = c.internal.gen.mkAttributedRef(prefix, s)
    def thunk(t: Type): Tree = q"$S.once(_root_.scala.Predef.implicitly[$ST[$t]])"
    val name = cls.name.decodedName.toString

    val body: Tree =
      if (cls.isModuleClass && cls.isCaseClass) {
        val obj = ref(cls.module)
        q"""$S.SProduct[$A]($name, _root_.scala.collection.immutable.Vector.empty,
              (_: _root_.scala.collection.Seq[_root_.scala.Any]) => $obj,
              (_: $A) => _root_.scala.collection.immutable.Nil)"""
      } else if (cls.isCaseClass) {
        val ctor = cls.primaryConstructor.asMethod
        val params = ctor.paramLists match {
          case ps :: rest if rest.forall(_.forall(_.isImplicit)) => ps
          case _ => refuse("a case class with a second, explicit parameter list")
        }
        val types = params.map(_.typeSignature.substituteTypes(cls.typeParams, A.typeArgs))
        params.zip(types).foreach { case (p, t) =>
          if (t.typeSymbol == definitions.RepeatedParamClass) refuse(s"field ${p.name.decodedName} is a repeated parameter")
        }
        val names = params.map(_.name.decodedName.toString)
        val fields = names.zip(types).map { case (n, t) => q"($n, ${thunk(t)})" }
        val xs = TermName(c.freshName("xs"))
        val a = TermName(c.freshName("a"))
        val args = types.zipWithIndex.map { case (t, i) => q"$xs($i).asInstanceOf[$t]" }
        val parts = params.indices.map(i => q"$a.productElement($i)")
        // a LOCAL case class has no `companion` symbol in Scala 2's
        // reflection (NoSymbol); its companion is in scope by name at
        // the derivation, which is where a local class can be derived
        val companion =
          if (cls.companion != NoSymbol) ref(cls.companion) else Ident(cls.name.toTermName)
        val defaults = params.zipWithIndex.map { case (p, i) =>
          if (p.asTerm.isParamWithDefault) {
            val m = TermName("$lessinit$greater$default$" + (i + 1))
            val call = if (A.typeArgs.isEmpty) q"$companion.$m" else q"$companion.$m[..${A.typeArgs}]"
            q"_root_.scala.Some(() => ($call: _root_.scala.Any))"
          } else q"_root_.scala.None"
        }
        val defaultsTree =
          if (params.exists(_.asTerm.isParamWithDefault))
            q"_root_.scala.collection.immutable.Vector[_root_.scala.Option[() => _root_.scala.Any]](..$defaults)"
          else q"_root_.scala.collection.immutable.Vector.empty"
        q"""$S.SProduct[$A]($name,
              _root_.scala.collection.immutable.Vector[(_root_.java.lang.String, () => $ST[_])](..$fields),
              ($xs: _root_.scala.collection.Seq[_root_.scala.Any]) => new $A(..$args),
              ($a: $A) => _root_.scala.collection.immutable.Vector[_root_.scala.Any](..$parts),
              $defaultsTree)"""
      } else if (cls.isSealed && (cls.isTrait || cls.isAbstract)) {
        val _ = cls.typeSignature   // completes the class, so its children are known
        val subs = cls.knownDirectSubclasses.toList.sortBy(s => (s.pos.line, s.pos.column))
        if (subs.isEmpty)
          refuse("no known subclasses. A sealed type's children are known only once they are " +
            "typed: declare them before the derivation, or in another file (SI-7046)")
        val cases = subs.map { s =>
          val sc = s.asClass
          val t =
            if (sc.typeParams.isEmpty) sc.toType
            else if (sc.typeParams.length == A.typeArgs.length) appliedType(sc.toType.typeConstructor, A.typeArgs)
            else refuse(s"case ${sc.name.decodedName} does not take the sum's type arguments")
          if (!(t <:< A)) refuse(s"case ${sc.name.decodedName} is not a $A under the sum's type arguments")
          val pattern =
            if (sc.typeParams.isEmpty) tq"$t"
            else tq"${c.internal.existentialAbstraction(sc.typeParams, sc.toType)}"
          (sc.name.decodedName.toString, t, pattern)
        }
        val entries = cases.map { case (n, t, _) => q"($n, ${thunk(t)})" }
        val a = TermName(c.freshName("a"))
        val arms = cases.zipWithIndex.map { case ((_, _, p), i) => cq"_: $p => $i" }
        q"""$S.SSum[$A]($name,
              _root_.scala.collection.immutable.Vector[(_root_.java.lang.String, () => $ST[_ <: $A])](..$entries),
              ($a: $A) => $a match { case ..$arms })"""
      } else refuse("neither a case class, a case object nor a sealed trait or abstract class")

    c.Expr[Schema[A]](body)
  }

  /** `Json.literals`' string door: a string LITERAL converts, a value
   * does not (Scala 3's `constValueOpt` on a singleton type) */
  def literal(c: blackbox.Context)(s: c.Expr[String]): c.Expr[Json] = {
    import c.universe._
    s.tree match {
      case Literal(Constant(_: String)) => c.Expr[Json](q"_root_.okay2.codec.Json.JStr($s)")
      case _ => c.abort(s.tree.pos,
        "only a string LITERAL converts to Json here. For a String value write JStr(x); " +
        "if it holds a serialized document you want Json.parse(x). See Json.literals.")
    }
  }
}
