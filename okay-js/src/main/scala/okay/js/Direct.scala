package okay.js

import scala.annotation.compileTimeOnly
import scala.language.dynamics
import scala.quoted.*

/**
 * `js { … }` — the tree, written as Scala (specs/js.md stage 4).
 *
 * **THE SUBSET IS CLOSED AND SMALL, AND LEAVING IT IS A COMPILE
 * ERROR.** That is the whole design rather than a limitation of it.
 * Approximating Scala's semantics in a browser fails by being WRONG —
 * a `Long` that is not 64-bit, an `==` that coerces, a `match` that
 * desugars to something else — and wrong output in a browser is the
 * failure mode nobody notices. So nothing is approximated: a
 * construct that is not on the list below is refused by name, and the
 * refusal points at Scala.js, which is in this build already.
 *
 * What is on the list:
 *
 *   - `val x = e` / `var x = e`        →  `var x = e;`
 *   - `x = e`                          →  `x = e;`
 *   - `if (c) a else b`                →  statement, or `c ? a : b`
 *   - `while (c) { … }`                →  `while (c) { … }`
 *   - arithmetic, comparison, `&& || !`
 *   - `==` / `!=` on a primitive       →  `===` / `!==`
 *   - `d.f` and `d.f(a)` on a `Dyn`    →  `d.f`, `d.f(a)`
 *   - `(a, b) => …`                    →  `function (a, b) { … }`
 *   - any expression of type `Js`      →  spliced in place
 *
 * And that is all. No collections, no pattern matching, no
 * exceptions, no implicits, no `for`.
 */
object Direct:

  /** the statements of a block, as a value */
  inline def js(inline body: Any): Vector[Stmt] = ${ jsImpl('body) }

  /** the same block, printed at compile time into a constant */
  transparent inline def source(inline body: Any): String = ${ sourceImpl('body) }

  /**
   * The block with its TYPES (typescript-types T8): each `val`/`var` a
   * `Stmt.TypedVar` and each lambda a `Js.TypedFun`, typed with what the
   * Scala compiler inferred — `number`, `string`, `boolean`, `any` for a
   * `Dyn` or a spliced `Js` (untyped JavaScript, said so), `(a0: A) => B`
   * for a function, `void` for `Unit`. Any other type is refused by name.
   * `Js.printTs` prints it as TypeScript, `Js.print` as the same JavaScript.
   */
  inline def ts(inline body: Any): Vector[Stmt] = ${ tsImpl('body) }

  /** the typed block, printed as TypeScript at compile time */
  transparent inline def tsSource(inline body: Any): String = ${ tsSourceImpl('body) }

  def jsImpl(body: Expr[Any])(using Quotes): Expr[Vector[Stmt]] =
    '{ ${ Expr.ofSeq(read(body, typed = false).map(lift)) }.toVector }

  def sourceImpl(body: Expr[Any])(using q: Quotes): Expr[String] =
    import q.reflect.*
    Literal(StringConstant(Js.print(read(body, typed = false)))).asExprOf[String]

  def tsImpl(body: Expr[Any])(using Quotes): Expr[Vector[Stmt]] =
    '{ ${ Expr.ofSeq(read(body, typed = true).map(lift)) }.toVector }

  def tsSourceImpl(body: Expr[Any])(using q: Quotes): Expr[String] =
    import q.reflect.*
    Literal(StringConstant(Js.printTs(read(body, typed = true)))).asExprOf[String]

  private def read(body: Expr[Any], typed: Boolean)(using q: Quotes): Vector[Stmt] =
    import q.reflect.*

    /** the operators Scala and JavaScript spell and mean the same */
    // PLAIN NAMES. The reflection API hands operator names over
    // unencoded — `&&`, not `$amp$amp` — which a first draft got
    // wrong and the suite caught by printing `a.&&(b.unary_!)`
    val same = Set("+", "-", "*", "/", "%", "<", ">", "<=", ">=", "&&", "||")

    /** where `==` may become `===`. Scala's `==` is equality and
     * JavaScript's `==` coerces, so `===` is the honest mapping — but
     * ONLY where the operands are things whose equality both
     * languages agree on. Anything else is refused rather than
     * guessed at. */
    def primitive(t: TypeRepr): Boolean =
      val s = t.widen.dealias.show
      s.endsWith("Int") || s.endsWith("Long") || s.endsWith("Double") ||
        s.endsWith("Float") || s.endsWith("Short") || s.endsWith("Byte") ||
        s.endsWith("String") || s.endsWith("Boolean") || s.endsWith("Dyn")

    def no(what: String, where: Position, instead: String): Nothing =
      report.errorAndAbort(
        s"js { } does not translate $what, and will not guess at it.\n" +
          s"  $instead\n" +
          "  Everything js { } does translate is in okay-js's docs; anything else\n" +
          "  belongs in Scala.js, which this build already cross-compiles with, or\n" +
          "  in a Js.Raw whose one untyped line is visible and countable.",
        where)

    /** WHAT THE BLOCK ITSELF DECLARES.
     *
     * An identifier is a JavaScript variable when the block declared
     * it, and a SPLICE when it is a `Js` value from outside. Without
     * this the two are indistinguishable: `val c = null` has type
     * `Null`, which is a subtype of everything including `Js`, so the
     * splice case swallowed a variable the block had just made. */
    val declared = scala.collection.mutable.Set.empty[String]

    /** a Scala type as TypeScript, or refused by name (T8) */
    def tsType(t: TypeRepr, where: Position): String =
      val w = t.widen.dealias
      if w =:= TypeRepr.of[Int] || w =:= TypeRepr.of[Long] || w =:= TypeRepr.of[Double] ||
        w =:= TypeRepr.of[Float] || w =:= TypeRepr.of[Short] || w =:= TypeRepr.of[Byte] then "number"
      else if w =:= TypeRepr.of[String] then "string"
      else if w =:= TypeRepr.of[Boolean] then "boolean"
      else if w =:= TypeRepr.of[Unit] then "void"
      else if w =:= TypeRepr.of[Null] then "null"
      // untyped JavaScript, and the annotation says so rather than inventing a type
      else if w <:< TypeRepr.of[Dyn] || w <:< TypeRepr.of[Js] then "any"
      else if w.isFunctionType then
        val as = w.typeArgs
        val ps = as.init.zipWithIndex.map((p, i) => s"a$i: ${tsType(p, where)}")
        s"(${ps.mkString(", ")}) => ${tsType(as.last, where)}"
      else no(s"a value of type ${w.show} as TypeScript", where,
        "js { } values are numbers, strings, booleans, Dyn, Js and functions of them")

    def block(t: Term): Vector[Stmt] = t match
      case Inlined(_, _, inner) => block(inner)
      case Block(stats, last) =>
        stats.toVector.flatMap(stat) ++ tail(last)
      case other => tail(other)

    /** the last expression of a block is a statement, not a value:
     * `js { }` builds a program */
    def tail(t: Term): Vector[Stmt] = t match
      case Literal(UnitConstant()) => Vector.empty
      case Inlined(_, _, inner) => tail(inner)
      // through `statement`, not `expr`: an `if` or a `while` at the
      // END of a block is still a statement, and its braces are not
      // an expression
      case other => statement(other)

    def stat(s: Statement): Vector[Stmt] = s match
      case ValDef(name, tpt, Some(rhs)) =>
        val v = if typed then Stmt.TypedVar(name, tsType(tpt.tpe, s.pos), expr(rhs)) else Stmt.Var(name, expr(rhs))
        declared += name
        Vector(v)
      case ValDef(name, _, None) =>
        no(s"a `val $name` with no value", s.pos, "give it one")
      case t: Term => statement(t)
      case other => no(s"`${other.show}`", other.pos, "it is not an expression")

    /** a term in statement position: some shapes are statements in
     * JavaScript and expressions in Scala, and `if` is the one that
     * matters */
    def statement(t: Term): Vector[Stmt] = t match
      case Inlined(_, _, inner) => statement(inner)
      // `e: Unit` is how a caller says "for the effect", and Scala
      // spells it `{ e; () }` — a BLOCK, which is a statement and not
      // an expression
      case Typed(inner, _) => statement(inner)
      case Block(stats, last) => stats.toVector.flatMap(stat) ++ tail(last)
      case If(c, y, n) =>
        Vector(Stmt.If(expr(c), statement(y), statement(n) match
          case Vector(Stmt.Do(Js.Undefined)) => Vector.empty
          case other => other))
      case While(c, b) => Vector(Stmt.While(expr(c), statement(b)))
      case Assign(lhs, rhs) => Vector(Stmt.Set(expr(lhs), expr(rhs)))
      // `d.f = v` on a Dyn is Scala's `d.updateDynamic("f")(v)`, which is
      // an ASSIGNMENT — printed as the call it is spelled as, it was
      // `document.updateDynamic("title", label)`, a method no JavaScript
      // object has. Found by tsc on T8's first typed program
      case Apply(Apply(Select(on, "updateDynamic"), List(Literal(StringConstant(n)))), List(v)) =>
        Vector(Stmt.Set(callee(on, n), expr(v)))
      case Literal(UnitConstant()) => Vector.empty
      case other => Vector(Stmt.Do(expr(other)))

    /** VARARGS ARRIVE WRAPPED. `applyDynamic(name)(args: Any*)` hands
     * the arguments over as one `Typed(Repeated(...))` term, so a
     * call with two arguments looks like a call with one until this
     * unwraps it — found by `global.console.log(x)` being refused
     * with the argument's own name in the message. */
    /** the one name that is scaffolding rather than JavaScript: it
     * exists so `global.console.log(x)` typechecks, and it is not in
     * the output */
    def isGlobal(t: Term): Boolean = t match
      case Inlined(_, _, inner) => isGlobal(inner)
      case Select(_, "global") => true
      case i: Ident => i.name == "global"
      case _ => false

    /** what a dynamic call is ON: a member of something, or a bare
     * name when the something is the global root */
    def callee(on: Term, n: String): Js =
      if isGlobal(on) then Js.Name(n) else Js.Field(expr(on), n)

    def args(as: List[Term]): Vector[Js] = as.toVector.flatMap {
      case Typed(Repeated(xs, _), _) => xs.toVector.map(expr)
      case Repeated(xs, _) => xs.toVector.map(expr)
      case one => Vector(expr(one))
    }

    def expr(t: Term): Js = t match
      case Inlined(_, _, inner) => expr(inner)
      case Typed(inner, _) => expr(inner)

      case Literal(IntConstant(v)) => Js.Num(v.toDouble)
      case Literal(LongConstant(v)) => Js.Num(v.toDouble)
      case Literal(DoubleConstant(v)) => Js.Num(v)
      case Literal(FloatConstant(v)) => Js.Num(v.toDouble)
      case Literal(StringConstant(v)) => Js.Str(v)
      case Literal(BooleanConstant(v)) => Js.Bool(v)
      case Literal(NullConstant()) => Js.Null
      case Literal(UnitConstant()) => Js.Undefined

      // A VALUE THAT IS ALREADY A Js TREE is spliced where it stands,
      // which is what lets a hand-written fragment and a generated
      // one share one program. It comes AFTER the literals because
      // `null`'s type is `Null`, which is a subtype of everything —
      // including `Js` — so a splice case above them swallows it.
      // a name the block made is a VARIABLE, checked before the
      // splice below, which would otherwise take any identifier whose
      // type happens to conform to `Js`
      case i: Ident if declared(i.name) => Js.Name(i.name)

      case term if term.tpe <:< TypeRepr.of[Js] =>
        term.asExprOf[Js].value(using Emit.jsOf).getOrElse(
          no("a Js value the compiler cannot read here", term.pos,
            "build it from Js's own constructors, or use Js.print at runtime"))

      case If(c, y, n) => Js.Ternary(expr(c), expr(y), expr(n))

      // `global` and anything reached through it
      case i: Ident => Js.Name(i.name)

      case Select(on, "unary_!") => Js.Unary("!", expr(on))
      case Select(on, "unary_-") => Js.Unary("-", expr(on))

      // `global` IS THE ROOT AND IT DISAPPEARS: `global.console.log`
      // is `console.log`, because the name a reader writes to make
      // Scala typecheck is not part of the JavaScript
      case Select(root, n) if isGlobal(root) => Js.Name(n)

      // `d.f` on a Dyn is a selectDynamic, which arrives as a call
      case Apply(Select(on, "selectDynamic"), List(Literal(StringConstant(n)))) =>
        // a Dyn reads through selectDynamic, so THIS is where the
        // global root disappears rather than in the plain Select case
        if isGlobal(on) then Js.Name(n) else Js.Field(expr(on), n)
      case Apply(Apply(TypeApply(Select(on, "applyDynamic"), _),
        List(Literal(StringConstant(n)))), as) =>
        Js.Call(callee(on, n), args(as))
      case Apply(Select(on, "applyDynamic"), List(Literal(StringConstant(n)))) =>
        Js.Call(callee(on, n), Vector.empty)
      case Apply(Apply(Select(on, "applyDynamic"), List(Literal(StringConstant(n)))), as) =>
        Js.Call(callee(on, n), args(as))

      case Apply(Select(l, op), List(r)) if same(op) =>
        Js.Bin(op, expr(l), expr(r))
      case Apply(Select(l, op), List(r)) if op == "==" || op == "!=" =>
        if !primitive(l.tpe) then
          no(s"`$op` on ${l.tpe.widen.show}", t.pos,
            "Scala's == is equality and JavaScript's == coerces; === is only the " +
              "honest mapping for a number, a string, a boolean or a Dyn")
        else Js.Bin(if op == "==" then "===" else "!==", expr(l), expr(r))

      // a lambda is a function, which is the one place a Scala shape
      // and a JavaScript shape line up exactly
      case Lambda(params, body) =>
        if typed then Js.TypedFun(params.toVector.map(p => (p.name, tsType(p.tpt.tpe, p.pos))), statement(body))
        else Js.Fun(params.toVector.map(_.name), statement(body))

      case Select(on, name) => Js.Field(expr(on), name)
      // CALLING A FUNCTION VALUE IS A CALL. Scala spells `f(1)` on a
      // function-typed val as `f.apply(1)`, and printed as written that is
      // JavaScript's Function.prototype.apply — `this` = 1, no arguments —
      // a different program with no error to show for it. Found by T8's
      // first typed test (typescript-types), which printed `f.apply(n, s)`
      case Apply(Select(on, "apply"), as) if on.tpe.widen.isFunctionType =>
        Js.Call(expr(on), args(as))
      case Apply(Select(root, n), as) if isGlobal(root) =>
        Js.Call(Js.Name(n), args(as))
      case Apply(Select(on, name), as) =>
        Js.Call(Js.Field(expr(on), name), args(as))
      case Apply(f, as) => Js.Call(expr(f), args(as))

      case Match(_, _) =>
        no("a `match`", t.pos, "its desugaring is Scala's, not JavaScript's")
      case Try(_, _, _) =>
        no("a `try`", t.pos, "use a Js.Raw for the one line that needs it")
      case other =>
        no(s"`${other.show}`", other.pos, "it is not in the subset")


      block(body.asTerm)

  // ---- lifting the computed tree back into an expression ------------

  private def lift(s: Stmt)(using Quotes): Expr[Stmt] = s match
    case Stmt.Var(n, v) => '{ Stmt.Var(${ Expr(n) }, ${ liftJs(v) }) }
    case Stmt.TypedVar(n, t, v) => '{ Stmt.TypedVar(${ Expr(n) }, ${ Expr(t) }, ${ liftJs(v) }) }
    case Stmt.Set(t, v) => '{ Stmt.Set(${ liftJs(t) }, ${ liftJs(v) }) }
    case Stmt.Do(o) => '{ Stmt.Do(${ liftJs(o) }) }
    case Stmt.Return(o) =>
      o.fold('{ Stmt.Return(None) })(x => '{ Stmt.Return(Some(${ liftJs(x) })) })
    case Stmt.If(c, y, n) =>
      '{ Stmt.If(${ liftJs(c) }, ${ liftAll(y) }, ${ liftAll(n) }) }
    case Stmt.While(c, b) => '{ Stmt.While(${ liftJs(c) }, ${ liftAll(b) }) }
    case Stmt.For(i, c, st, b) =>
      val init = i.fold('{ None })(x => '{ Some(${ lift(x) }) })
      val cond = c.fold('{ None })(x => '{ Some(${ liftJs(x) }) })
      val step = st.fold('{ None })(x => '{ Some(${ liftJs(x) }) })
      '{ Stmt.For($init, $cond, $step, ${ liftAll(b) }) }
    case Stmt.Switch(on, cs, f) =>
      val cases = Expr.ofSeq(cs.map((k, b) => '{ (${ liftJs(k) }, ${ liftAll(b) }) }))
      '{ Stmt.Switch(${ liftJs(on) }, $cases.toVector, ${ liftAll(f) }) }
    case Stmt.Break => '{ Stmt.Break }
    case Stmt.Block(b) => '{ Stmt.Block(${ liftAll(b) }) }
    case Stmt.Comment(s) => '{ Stmt.Comment(${ Expr(s) }) }
    case Stmt.Raw(s) => '{ Stmt.Raw(${ Expr(s) }) }

  private def liftAll(ss: Vector[Stmt])(using Quotes): Expr[Vector[Stmt]] =
    '{ ${ Expr.ofSeq(ss.map(lift)) }.toVector }

  private def liftJs(j: Js)(using Quotes): Expr[Js] = j match
    case Js.Num(v) => '{ Js.Num(${ Expr(v) }) }
    case Js.Str(s) => '{ Js.Str(${ Expr(s) }) }
    case Js.Bool(b) => '{ Js.Bool(${ Expr(b) }) }
    case Js.Null => '{ Js.Null }
    case Js.Undefined => '{ Js.Undefined }
    case Js.Name(id) => '{ Js.Name(${ Expr(id) }) }
    case Js.Raw(s) => '{ Js.Raw(${ Expr(s) }) }
    case Js.Arr(items) => '{ Js.Arr(${ liftJsAll(items) }) }
    case Js.Obj(fs) =>
      val fields = Expr.ofSeq(fs.map((k, v) => '{ (${ Expr(k) }, ${ liftJs(v) }) }))
      '{ Js.Obj($fields.toVector) }
    case Js.Field(of, n) => '{ Js.Field(${ liftJs(of) }, ${ Expr(n) }) }
    case Js.Index(of, a) => '{ Js.Index(${ liftJs(of) }, ${ liftJs(a) }) }
    case Js.Call(f, as) => '{ Js.Call(${ liftJs(f) }, ${ liftJsAll(as) }) }
    case Js.New(f, as) => '{ Js.New(${ liftJs(f) }, ${ liftJsAll(as) }) }
    case Js.Unary(op, of) => '{ Js.Unary(${ Expr(op) }, ${ liftJs(of) }) }
    case Js.Bin(op, l, r) => '{ Js.Bin(${ Expr(op) }, ${ liftJs(l) }, ${ liftJs(r) }) }
    case Js.Ternary(c, y, n) =>
      '{ Js.Ternary(${ liftJs(c) }, ${ liftJs(y) }, ${ liftJs(n) }) }
    case Js.Fun(ps, b) =>
      val params = Expr.ofSeq(ps.map(Expr(_)))
      '{ Js.Fun($params.toVector, ${ liftAll(b) }) }
    case Js.TypedFun(ps, b) =>
      val params = Expr.ofSeq(ps.map((n, t) => '{ (${ Expr(n) }, ${ Expr(t) }) }))
      '{ Js.TypedFun($params.toVector, ${ liftAll(b) }) }

  private def liftJsAll(js: Vector[Js])(using Quotes): Expr[Vector[Js]] =
    '{ ${ Expr.ofSeq(js.map(liftJs)) }.toVector }


/**
 * A NAME IN THE BROWSER, so `global.console.log(x)` typechecks and
 * the macro can read it.
 *
 * It has no runtime: every member is dynamic and every use is inside
 * a `js { }` block, which the macro consumes. Calling one outside a
 * block is a compile error rather than a puzzle at runtime.
 */
@compileTimeOnly("Dyn only means anything inside js { }")
class Dyn extends Dynamic:
  /** a member, which IS a value: `global.document.body` */
  def selectDynamic(name: String): Dyn = ???

  /**
   * A CALL IS A STATEMENT, and that is why this answers `Unit`.
   *
   * In a `js { }` block a dynamic call is almost always made for its
   * effect, and typing it as a value made every one of them a
   * DISCARDED value — a warning at each call site, in a codebase whose
   * rule is no warnings ever. A call whose value is wanted is outside
   * the subset on purpose: build it with `Js.Call` and splice the
   * value in, where the shape is explicit and nothing is guessed.
   */
  def applyDynamic(name: String)(args: Any*): Unit = ???

  def updateDynamic(name: String)(value: Any): Unit = ???

/** the browser's globals, as one name. `global.console.log(x)` is the
 * shape a reader expects and the shape the macro reads */
object Dyn:
  @compileTimeOnly("global only means anything inside js { }")
  val global: Dyn = new Dyn
