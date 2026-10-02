package okay
package macros

import scala.quoted.*

/** the macros behind `At.here`, `Shift.Key.of` and `Shift.Machine.of` (okay-macros-package) */
@scala.annotation.publicInBinary private[okay] object ShiftMacros:

  // the macro behind `here`
  def hereImpl(using scala.quoted.Quotes): scala.quoted.Expr[At] =
    import scala.quoted.*
    import quotes.reflect.*
    val pos = Position.ofMacroExpansion
    val name =
      try pos.sourceFile.name
      catch case _: Throwable => "<unknown>"
    val line = pos.startLine + 1
    val where = Expr(s"$name:$line")
    '{ At($where) }

  def keyImpl[R: Type](using q: Quotes): Expr[Shift.Key[R]] =
    import q.reflect.*
    def parts(t: TypeRepr, or: Boolean): List[TypeRepr] = t.dealias match
      case OrType(a, b) if or => parts(a, or) ++ parts(b, or)
      case AndType(a, b) if !or => parts(a, or) ++ parts(b, or)
      case other => List(other)
    // bounded by the type's own nesting, which the compiler has already walked
    def norm(t: TypeRepr): String = t.dealias.simplified match
      case o: OrType => parts(o, or = true).map(norm).distinct.sorted.mkString("(", " | ", ")")
      case a: AndType => parts(a, or = false).map(norm).distinct.sorted.mkString("(", " & ", ")")
      case AppliedType(c, args) => norm(c) + args.map(norm).mkString("[", ", ", "]")
      case c: ConstantType => c.show
      case other =>
        val s = other.typeSymbol
        if s.isClassDef || s.flags.is(Flags.Opaque) then s.fullName
        else report.errorAndAbort(
          s"the answer type ${Type.show[R]} is abstract here (${other.show}), so a reset or shift of it has no key; " +
            s"take a `Shift.Key[${other.show}]` as a parameter where the type is known")
    '{ Shift.Key.intern[R](${ Expr(norm(TypeRepr.of[R])) }) }

  def machineImpl[F[+_]: Type](using q: Quotes): Expr[Shift.Machine[F]] =
    import q.reflect.*
    val shift = TypeRepr.of[Shift[Any, Any]].typeSymbol
    val delim = TypeRepr.of[Cont0[?, ?, ?, Any]].typeSymbol
    // bounded by the row's own nesting, which the compiler has already walked
    def members(t: TypeRepr): List[TypeRepr] = t.dealias.simplified match
      case OrType(a, b) => members(a) ++ members(b)
      case other => List(other)
    // an alias of a type lambda (`State % Int`, `Instances.Of[G]`) applied: one beta step per alias, at most
    // as many as the source wrote
    @scala.annotation.tailrec
    def reduce(t: TypeRepr, fuel: Int): TypeRepr = t.dealias.simplified match
      case r @ AppliedType(tc, args) if fuel > 0 =>
        val d = tc.dealias
        if d == tc then r else reduce(d.appliedTo(args), fuel - 1)
      case other => other
    val ms = members(TypeRepr.of[F].appliedTo(TypeRepr.of[Any])).map(reduce(_, 64)).flatMap(members)
    val inner = ms.exists(m => m.typeSymbol == shift || m.typeSymbol == delim)
    val unread = ms.filterNot(m => m.typeSymbol.isClassDef)
    if !inner && unread.nonEmpty then
      report.errorAndAbort(
        s"whether a machine already runs in the row ${Type.show[F]} cannot be read here: " +
          s"${unread.map(_.show).mkString(", ")} is abstract, and it may hold a Shift.\n" +
          "Pass the obligation on to the caller, who knows the row: take `(using Shift.Machine[F])`\n" +
          "(docs/continuations/12-one-machine.md)")
    '{ new Shift.Machine[F](${ Expr(inner) }) }
