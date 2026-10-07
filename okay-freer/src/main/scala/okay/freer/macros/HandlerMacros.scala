package okay.freer
package macros

import okay.{guard}

import scala.quoted.*

/**
 * the macros behind `Handler`'s case forms (okay-macros-package, stage 2): `Handler.Seen.of` and the checks
 * `checkAnswers`, `checkStates`, `checkInto` splice
 */
@scala.annotation.publicInBinary private[okay] object HandlerMacros:

  def seenImpl[F[+_]: Type](using q: Quotes): Expr[Handler.Seen[F]] =
    import q.reflect.*
    val effect = TypeRepr.of[F].appliedTo(TypeRepr.of[Any]).dealias.typeSymbol
    // does `t` name the type parameter `p`; bounded by the type's own nesting
    def names(t: TypeRepr, p: Symbol): Boolean = t.dealias match
      case AppliedType(c, as) => names(c, p) || as.exists(names(_, p))
      case r: TypeRef => r.typeSymbol == p
      case _ => false
    // per operation: (its answer is its own field's type, its caller chooses its answer)
    val kinds = effect.children.map { c =>
      val tps = if c.isClassDef then c.declaredTypes.filter(_.isTypeParam) else Nil
      if tps.isEmpty then (false, false)
      else
        val self = c.typeRef.appliedTo(tps.map(_.typeRef))
        self.baseType(effect) match
          case AppliedType(_, dargs) if dargs.nonEmpty =>
            dargs.last match
              case r: TypeRef if tps.contains(r.typeSymbol) =>
                val fixed = dargs.init.exists(names(_, r.typeSymbol))
                (fixed && c.caseFields.exists(f => names(self.memberType(f), r.typeSymbol)), !fixed)
              case _ => (false, false)
          case _ => (false, false)
    }
    val seenAt = if kinds.exists(_._1) && !kinds.exists(_._2) then TypeRepr.of[Any] else TypeRepr.of[Handler.Answer]
    seenAt.asType match
      case '[t] => '{ new Handler.Seen[F] { type T = t } }

  /** `kind`: 0 the body is the answer, 1 the pair's second is, 2 the program's value is */
  def checkImpl[F[+_]: Type, S: Type, T: Type, C: Type](cases: Expr[C], kind: Int)(using q: Quotes): Expr[C] =
    import q.reflect.*
    checkCore(cases.asTerm, TypeRepr.of[F], TypeRepr.of[S], kind, TypeRepr.of[T] =:= TypeRepr.of[Any])
    cases

  /** the cases of a `{ case … }` lambda */
  def caseDefsOf(using q: Quotes)(cases: q.reflect.Term): List[q.reflect.CaseDef] =
    import q.reflect.*
    def strip(t: Term): Term = t match
      case Inlined(_, Nil, e) => strip(e)
      case Block(Nil, e) => strip(e)
      case Typed(e, _) => strip(e)
      // the compiler's own adaptation of the lambda to `F[Answer] => …`
      case TypeApply(Select(e, "$asInstanceOf$"), _) => strip(e)
      case other => other
    strip(cases) match
      case Block(List(DefDef(_, _, _, Some(body))), _: Closure) => strip(body) match
        case Match(_, cds) => cds
        case other => report.errorAndAbort("write the handler as cases: `{ case Op(…) => … }`", other.pos)
      case other => report.errorAndAbort("write the handler as cases: `{ case Op(…) => … }`", other.pos)

  /** the check itself, for an effect given as a type constructor */
  def checkCore(using q: Quotes)(cases: q.reflect.Term, effectType: q.reflect.TypeRepr, stateType: q.reflect.TypeRepr, kind: Int,
                seenAtAny: Boolean): Unit =
    import q.reflect.*
    val effect = effectType.appliedTo(TypeRepr.of[Any]).dealias.typeSymbol
    val pair = TypeRepr.of[(Any, Any)].typeSymbol
    val program = TypeRepr.of[Free[scala.Nothing, Any]].typeSymbol
    def last(t: TypeRepr, of: Symbol): Option[TypeRepr] = t.widen.dealias.baseType(of) match
      case AppliedType(_, args) if args.nonEmpty => Some(args.last)
      case _ => None
    // the effect's own parameters, at the answer the cases see (`Env % Int` -> Env[Int, Answer])
    val effectArgs: List[TypeRepr] = effectType.appliedTo(TypeRepr.of[Handler.Answer]).dealias match
      case AppliedType(_, as) => as
      case _ => Nil
    val answer = TypeRepr.of[Handler.Answer]
    // what an operation answers, from its constructor's DECLARATION: `Get[R, A] extends Env[R, R]` read at the
    // effect's parameters is Int; an answer still naming the constructor's own parameter (`Asks[R, A] extends
    // Reader[R, A]`'s A) is the caller's to choose, and is seen at `Answer`
    def declared(sym: Symbol): Option[TypeRepr] =
      val tps = if sym.isClassDef then sym.declaredTypes.filter(_.isTypeParam) else Nil
      val self =
        if !sym.isClassDef then sym.termRef.widen
        else if tps.isEmpty then sym.typeRef
        else sym.typeRef.appliedTo(tps.map(_.typeRef))
      self.baseType(effect) match
        case AppliedType(_, dargs) if dargs.nonEmpty =>
          val solved = dargs.init.zip(effectArgs).collect {
            case (t: TypeRef, f) if tps.contains(t.typeSymbol) => (t.typeSymbol, f)
          }
          val a = dargs.last.substituteTypes(solved.map(_._1), solved.map(_._2))
          val open = tps.filterNot(p => solved.exists(_._1 == p))
          Some(if open.exists(p => a.typeSymbol == p) then answer else a)
        case _ => None
    // the operation's constructor a pattern names, for the exhaustiveness check; bounded by the pattern's nesting
    def ctor(p: Tree): Option[Symbol] = p match
      case q.reflect.Bind(_, inner) => ctor(inner)
      case TypedOrTest(inner, tpt) => ctor(inner).orElse(Some(tpt.tpe.typeSymbol).filter(_ != effect))
      case Unapply(fun, _, _) => Some(fun.symbol.owner.companionClass).filter(_.exists)
      case t: Term if t.tpe.termSymbol.exists => Some(t.tpe.termSymbol)
      case _ => None
    def answers(p: Tree): Option[TypeRepr] = ctor(p).flatMap(declared)
    // does `t` name the type parameter `p`; bounded by the type's own nesting
    def mentions(t: TypeRepr, p: Symbol): Boolean = t.dealias match
      case AppliedType(c, as) => mentions(c, p) || as.exists(mentions(_, p))
      case AndType(a, b) => mentions(a, p) || mentions(b, p)
      case OrType(a, b) => mentions(a, p) || mentions(b, p)
      case r: TypeRef => r.typeSymbol == p
      case _ => false
    // WHERE THE CASE FORM STOPS: an operation whose answer is a parameter of the effect that one of its fields
    // also has (`Set(s: S) extends State[S, S]`). Matched at F[Answer] the typer needs S = Int and S <: Answer
    // at once, binds S fresh, and the field is no longer an Int. The field's name, when it is so.
    def tied(sym: Symbol): Option[String] =
      val tps = if sym.isClassDef then sym.declaredTypes.filter(_.isTypeParam) else Nil
      if tps.isEmpty then None
      else
        val self = sym.typeRef.appliedTo(tps.map(_.typeRef))
        self.baseType(effect) match
          case AppliedType(_, dargs) if dargs.nonEmpty =>
            dargs.last match
              case r: TypeRef if tps.contains(r.typeSymbol) && dargs.init.exists(mentions(_, r.typeSymbol)) =>
                sym.caseFields.find(f => mentions(self.memberType(f), r.typeSymbol)).map(_.name)
              case _ => None
          case _ => None
    def opPattern(p: Tree): Tree = if kind != 1 then p else p match
      case Unapply(_, _, List(_, op)) => op
      case q.reflect.Bind(_, inner) => opPattern(inner)
      case other => other
    def value(rhs: Term): TypeRepr = kind match
      case 0 => rhs.tpe.widen
      case 1 => rhs.tpe.widen.dealias.baseType(pair) match
        case AppliedType(_, List(s, a)) =>
          if !(s <:< stateType) then
            report.error(s"a case answers (state, answer): its state is ${s.show(using Printer.TypeReprShortCode)}, not ${stateType.show(using Printer.TypeReprShortCode)}", rhs.pos)
          a
        case _ =>
          report.error("a case answers (state, answer)", rhs.pos)
          TypeRepr.of[scala.Nothing]
      case _ => last(rhs.tpe, program).getOrElse(rhs.tpe.widen)
    val caseDefs: List[CaseDef] = caseDefsOf(cases)
    // EXHAUSTIVE, as an error and not the compiler's warning: every operation of the effect has a case with no
    // guard, or a `case _` stands last
    val all = effect.children
    val covered = caseDefs.filter(_.guard.isEmpty).map(cd => opPattern(cd.pattern))
    val wildcard = covered.exists { case Wildcard() => true; case _ => false }
    val named = covered.flatMap(ctor).toSet
    val missing = all.filterNot(c => named.contains(c) || named.contains(c.companionModule))
    if !wildcard && all.nonEmpty && missing.nonEmpty then
      report.error(s"not every operation of ${effect.name} is handled: ${missing.map(_.name).mkString(", ")}", cases.pos)
    def check(cd: CaseDef): Unit =
      val op = ctor(opPattern(cd.pattern))
      val name = op.map(_.name).getOrElse(opPattern(cd.pattern).show)
      val short = Printer.TypeReprShortCode
      // seen at Any (Seen), a field's operation keeps its field's type: nothing to refuse
      op.flatMap(o => if seenAtAny then None else tied(o)) match
        case Some(field) =>
          report.error(s"$name answers the type its field `$field` has, which a case written against the whole " +
            s"${effect.name} cannot keep: write this handler with `.poly { [X] => (e: …[X]) => … }`, where the " +
            "compiler checks it", cd.pattern.pos)
        case None =>
          val got = value(cd.rhs)
          answers(opPattern(cd.pattern)) match
            case Some(want) if want =:= answer =>
              if !(got <:< answer) then
                report.error(s"$name answers what its caller chose, which only the operation's own data can give; " +
                  s"this case gives ${got.show(using short)}", cd.rhs.pos)
            case Some(want) =>
              if !(got <:< want) then
                report.error(s"$name answers ${want.show(using short)}, but this case gives ${got.show(using short)}", cd.rhs.pos)
            case None =>
              if !(got <:< TypeRepr.of[scala.Nothing]) then
                report.error("a case that names no operation cannot know what to answer: only a `throw` may stand here", cd.rhs.pos)
    caseDefs.foreach(check)
