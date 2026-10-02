package okay

import okay.Free.{Return, Inject, Bind}
import okay.Row.up
import scala.annotation.tailrec
import scala.quoted.*

/**
 * A handler (Plotkin & Pretnar's sense, level 1, specs/api-levels.md): a VALUE that takes the effect `E` off
 * any program's row and answers `O[A]` — `p.handle(State(5))`, `p.handle(State(5)).handle(Throws.either).run`.
 * `Handler[E, O]` is the usual one: any answer, nothing needed of the rest of the row. `Handler.Full` bounds the
 * answer by `I` and needs `Needs[F]` of the rest `F` (`Reset[R]`: the answer is `R`, the rest's `Nesting`).
 * An answer per operation and no more, the old `Handler[F]`, is `Answers[F]`.
 */
type Handler[E[+_], O[_]] = Handler.Full[E, Any, O, Handler.Nothing]

object Handler:
  /** the handler in full: the answers it takes (`I`) and what it needs of the rest of the row (`Needs`) */
  trait Full[E[+_], I, O[_], Needs[_[+_]]]:
    def run[A, F[+_]](p: A ! E + F)(using A <:< I, Distinct[E + F], Needs[F]): O[A] ! F

  /** the evidence of nothing: always there */
  final class Nothing[F[+_]] private[Handler] ()
  object Nothing:
    given any[F[+_]]: Nothing[F] = new Nothing[F]()

  // THE AUTHOR'S DOOR (level 2, specs/handler-forms.md): four forms by power, each a level-1 value on the
  // machinery that is already fastest for its case.

  /**
   * 1 · answer each operation with a value, and the program goes on (`!.relay`):
   * `Handler.answer[Users] { case Find(id) => … }`, or `.poly { [X] => (e: Users[X]) => … }`
   */
  def answer[F[+_]]: Answering[F] = new Answering[F]

  final class Answering[F[+_]] private[Handler] ():
    /** cases, each checked at compile time against its operation's answer type */
    inline def apply(inline cases: F[Any] => Any)(using TypeableK[F]): Handler[F, [A] =>> A] =
      // THE CAST the check licenses: each case answers its operation's type (checkAnswers)
      answerOf[F]([X] => (e: F[X]) => checkAnswers[F](cases)(e).asInstanceOf[X])
    /** a polymorphic function, typed by the compiler with no macro */
    def poly(f: [X] => F[X] => X)(using TypeableK[F]): Handler[F, [A] =>> A] = answerOf[F](f)

  def answerOf[F[+_]](f: [X] => F[X] => X)(using TypeableK[F]): Handler[F, [A] =>> A] = new Handler[F, [A] =>> A]:
    def run[A, G[+_]](p: A ! F + G)(using A <:< Any, Distinct[F + G], Nothing[G]): A ! G =
      Effects.relay[A, A, F, G](p)(pure(_))([X, Y] => (e: F[X]) => Cont.Pure[X, Y](f(e)))

  /** 1 · the same from an `Answers[F]` (its own name: an overload would cost the lambda form its expected type) */
  def from[F[+_]](a: Answers[F])(using TypeableK[F]): Handler[F, [A] =>> A] =
    answerOf[F]([X] => (e: F[X]) => a.handle(e))

  /**
   * 2 · a state threaded through the operations: `(s, op) => (s', answer)`, the result carrying the last state:
   * `Handler.state[Users, Int](0) { case (n, Find(id)) => … }`, or `.poly { [X] => (n: Int, e: Users[X]) => … }`
   */
  def state[F[+_], S](init: S): Stating[F, S] = new Stating[F, S](init)

  final class Stating[F[+_], S] private[Handler] (val init: S):
    /** cases, each checked at compile time: the second of the pair answers the operation's type */
    inline def apply(inline cases: (S, F[Any]) => (S, Any))(using TypeableK[F]): Handler[F, [A] =>> (S, A)] =
      // THE CAST the check licenses (checkStates)
      stateOf[F, S](init)([X] => (s: S, e: F[X]) => checkStates[F, S](cases)(s, e).asInstanceOf[(S, X)])
    /** a polymorphic function, typed by the compiler with no macro */
    inline def poly(inline f: [X] => (S, F[X]) => (S, X))(using TypeableK[F]): Handler[F, [A] =>> (S, A)] =
      stateOf[F, S](init)(f)

  @scala.annotation.nowarn("msg=New anonymous class definition will be duplicated")
  inline def stateOf[F[+_], S](init: S)(inline f: [X] => (S, F[X]) => (S, X))(using TypeableK[F]): Handler[F, [A] =>> (S, A)] =
    // a class per call site is the point: the clause expands into that site's own loop, where the JIT can
    // drop the pair it answers (handler-forms: 1.60x with the clause a function value)
    new Handler[F, [A] =>> (S, A)]:
      def run[A, G[+_]](p: A ! F + G)(using A <:< Any, Distinct[F + G], Nothing[G]): (S, A) ! G =
        // a call from inside flatMap cannot be a jump; `again` takes it, so the walk stays a checked loop
        def again(s: S)(x: A ! F + G): (S, A) ! G = loop(s)(x)
        @tailrec def loop(s: S)(x: A ! F + G): (S, A) ! G = (x.resume: @unchecked) match
          case Return(a) => Return((s, a))
          case i @ Inject(e) => split[F, G](e)(op => { val (s2, v) = f(s, op); Return((s2, v)): (S, A) ! G })
                                               (_ => forwarded[F, G](i).map((s, _)))
          case Bind(i @ Inject(e), k) => split[F, G](e)(op => { val (s2, v) = f(s, op); loop(s2)(k(v)) })
                                                       (_ => forwarded[F, G](i).flatMap(x => again(s)(k(x))))
        loop(init)(p)

  /** what `into` needs of the rest of the row: that it holds `G` */
  type Holds[G[+_]] = [R[+_]] =>> Row.Sub[G, R]

  /**
   * 3 · each operation a program in the effects `G`, which the rest of the row must hold (`!.translate`):
   * `Handler.into[Users, State % M] { case Find(id) => … }`, or `.poly { [X] => (e: Users[X]) => … }`
   */
  def into[F[+_], G[+_]]: Into[F, G] = new Into[F, G]

  final class Into[F[+_], G[+_]] private[Handler] ():
    /** cases, each checked at compile time: the program's value answers the operation's type */
    inline def apply(inline cases: F[Any] => Any ! G)(using TypeableK[F]): Full[F, Any, [A] =>> A, Holds[G]] =
      // THE CAST the check licenses (checkInto)
      intoOf[F, G]([X] => (e: F[X]) => checkInto[F, G](cases)(e).asInstanceOf[X ! G])
    /** a polymorphic function, typed by the compiler with no macro */
    def poly(f: [X] => F[X] => X ! G)(using TypeableK[F]): Full[F, Any, [A] =>> A, Holds[G]] = intoOf[F, G](f)

  def intoOf[F[+_], G[+_]](f: [X] => F[X] => X ! G)(using TypeableK[F]): Full[F, Any, [A] =>> A, Holds[G]] =
    new Full[F, Any, [A] =>> A, Holds[G]]:
      def run[A, R[+_]](p: A ! F + R)(using A <:< Any, Distinct[F + R], Row.Sub[G, R]): A ! R =
        Effects.translate[A, F, R](p)([X] => (e: F[X]) => f(e).up[R])

  /**
   * 4 · the continuation in hand: `resume` once, twice, or not at all (`Effects.handle`). `ret` shapes a
   * finished program's answer; the clause is polymorphic in that answer and in the rest of the row.
   */
  def control[F[+_], O[_]](ret: [A] => A => O[A])(f: [X, A, G[+_]] => (F[X], X => O[A] ! G) => O[A] ! G)
                          (using TypeableK[F]): Handler[F, O] = new Handler[F, O]:
    def run[A, G[+_]](p: A ! F + G)(using A <:< Any, Distinct[F + G], Nothing[G]): O[A] ! G =
      Effects[Free].handle[F, G](p)(a => pure[G, O[A]](ret(a)))(
        [X] => (e: F[X]) =>
          val resume = new Resume[X, O[A], G]
          val out = f[X, A, G](e, resume)
          // `resume(x)` once, as the clause's answer: the program goes on, nothing to capture
          if resume.calls == 1 && (out eq resume.last) then Cont.Pure[X, O[A] ! G](resume.arg)
          else Cont.shift[X, O[A] ! G, O[A] ! G](k => { resume.k = k; out }))

  /**
   * The `resume` a `control` clause gets. Called once and returned as the clause's answer, it is a tail
   * resume, answered with no capture; otherwise each call is a program that enters the captured `k`, which
   * the capture fills in before any of them runs.
   */
  private final class Resume[X, B, G[+_]] extends (X => B ! G):
    var k: X => B ! G = scala.compiletime.uninitialized
    var arg: X = scala.compiletime.uninitialized
    var last: B ! G = scala.compiletime.uninitialized
    var calls: Int = 0
    def apply(x: X): B ! G =
      calls += 1
      arg = x
      last = Free.delay[G, B](() => k(x))
      last

  // THE CHECKS behind the `{ case … }` forms: each returns its cases unchanged, after proving that every case
  // answers what its operation answers (the pattern's constructor, read as an `F[T]`, answers `T`). A case
  // with no constructor to read (`case _`) must not answer at all (a `throw`).

  inline def checkAnswers[F[+_]](inline cases: F[Any] => Any): F[Any] => Any = ${ checkImpl[F, F[Any] => Any]('cases, 0) }
  inline def checkStates[F[+_], S](inline cases: (S, F[Any]) => (S, Any)): (S, F[Any]) => (S, Any) =
    ${ checkImpl[F, (S, F[Any]) => (S, Any)]('cases, 1) }
  inline def checkInto[F[+_], G[+_]](inline cases: F[Any] => Any ! G): F[Any] => Any ! G = ${ checkImpl[F, F[Any] => Any ! G]('cases, 2) }

  /** `kind`: 0 the body is the answer, 1 the pair's second is, 2 the program's value is */
  def checkImpl[F[+_]: Type, C: Type](cases: Expr[C], kind: Int)(using q: Quotes): Expr[C] =
    import q.reflect.*
    val effect = TypeRepr.of[F].appliedTo(TypeRepr.of[Any]).dealias.typeSymbol
    val pair = TypeRepr.of[(Any, Any)].typeSymbol
    val program = TypeRepr.of[Free[scala.Nothing, Any]].typeSymbol
    def last(t: TypeRepr, of: Symbol): Option[TypeRepr] = t.widen.dealias.baseType(of) match
      case AppliedType(_, args) if args.nonEmpty => Some(args.last)
      case _ => None
    // what the pattern's operation answers; None when the pattern names no constructor. Bounded by the
    // pattern's own nesting.
    def answers(p: Tree): Option[TypeRepr] = p match
      case q.reflect.Bind(_, inner) => answers(inner)
      // the type the typer checked the pattern at (`Ask[Int]`), not the case's declaration (`Ask[R]`)
      case TypedOrTest(inner, tpt) => last(tpt.tpe, effect).orElse(answers(inner))
      case Wildcard() => None
      case Unapply(fun, _, _) => fun.tpe.widen match
        case MethodType(_, List(scrutinee), _) => last(scrutinee, effect)
        case _ => None
      case t: Term => last(t.tpe, effect)
      case _ => None
    // the operation's constructor a pattern names, for the exhaustiveness check; bounded by the pattern's nesting
    def ctor(p: Tree): Option[Symbol] = p match
      case q.reflect.Bind(_, inner) => ctor(inner)
      case TypedOrTest(inner, tpt) => ctor(inner).orElse(Some(tpt.tpe.typeSymbol).filter(_ != effect))
      case Unapply(fun, _, _) => Some(fun.symbol.owner.companionClass).filter(_.exists)
      case t: Term if t.tpe.termSymbol.exists => Some(t.tpe.termSymbol)
      case _ => None
    def opPattern(p: Tree): Tree = if kind != 1 then p else p match
      case Unapply(_, _, List(_, op)) => op
      case q.reflect.Bind(_, inner) => opPattern(inner)
      case other => other
    def value(rhs: Term): TypeRepr = kind match
      case 0 => rhs.tpe.widen
      case 1 => last(rhs.tpe, pair).getOrElse(rhs.tpe.widen)
      case _ => last(rhs.tpe, program).getOrElse(rhs.tpe.widen)
    def strip(t: Term): Term = t match
      case Inlined(_, Nil, e) => strip(e)
      case Block(Nil, e) => strip(e)
      case Typed(e, _) => strip(e)
      case other => other
    val caseDefs: List[CaseDef] = strip(cases.asTerm) match
      case Block(List(DefDef(_, _, _, Some(body))), _: Closure) => strip(body) match
        case Match(_, cds) => cds
        case other => report.errorAndAbort("write the handler as cases: `{ case Op(…) => … }`", other.pos)
      case other => report.errorAndAbort("write the handler as cases: `{ case Op(…) => … }`", other.pos)
    // EXHAUSTIVE, as an error and not the compiler's warning: every operation of the effect has a case with no
    // guard, or a `case _` stands last
    val all = effect.children
    val covered = caseDefs.filter(_.guard.isEmpty).map(cd => opPattern(cd.pattern))
    val wildcard = covered.exists { case Wildcard() => true; case _ => false }
    val named = covered.flatMap(ctor).toSet
    val missing = all.filterNot(c => named.contains(c) || named.contains(c.companionModule))
    if !wildcard && all.nonEmpty && missing.nonEmpty then
      report.error(s"not every operation of ${effect.name} is handled: ${missing.map(_.name).mkString(", ")}", cases.asTerm.pos)
    for cd <- caseDefs do
      val got = value(cd.rhs)
      answers(opPattern(cd.pattern)) match
        case Some(want) if (want.dealias match { case t: TypeRef => !t.typeSymbol.isClassDef; case _ => false }) =>
          // the answer is a type the pattern itself binds (`Asks[R, A]`'s A): the case was typed against F[Any],
          // so its body is typed without it and cannot be checked here
          val op = ctor(opPattern(cd.pattern)).map(_.name).getOrElse(opPattern(cd.pattern).show)
          report.error(s"$op answers a type its own pattern binds (${want.show(using Printer.TypeReprShortCode)}), which a case written " +
            "against the whole effect cannot see: write this handler with `.poly { [X] => (e: …[X]) => … }`, where the compiler checks it", cd.rhs.pos)
        case Some(want) =>
          if !(got <:< want) then
            val op = ctor(opPattern(cd.pattern)).map(_.name).getOrElse(opPattern(cd.pattern).show)
            val short = Printer.TypeReprShortCode
            report.error(s"$op answers ${want.show(using short)}, but this case gives ${got.show(using short)}", cd.rhs.pos)
        case None =>
          if !(got <:< TypeRepr.of[scala.Nothing]) then
            report.error("a case that names no operation cannot know what to answer: only a `throw` may stand here", cd.rhs.pos)
    cases
