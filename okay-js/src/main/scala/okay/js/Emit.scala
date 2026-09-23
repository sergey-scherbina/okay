package okay.js

import scala.quoted.*

/**
 * THE COMPILE-TIME ROAD (specs/js.md).
 *
 * `Js.print` builds the text when it is called. `Emit.emit` builds it
 * while the compiler is running and puts a STRING CONSTANT in the
 * jar, so a program that never changes costs nothing at startup and
 * needs no build step and no artifact beside the jar — which is the
 * property okay-ui deliberately bought by hand-writing `LiveJs`, and
 * the one a generated script must not give away.
 *
 * It works by UNLIFTING: a `FromExpr[Js]` reads the tree back out of
 * the typed expression the author wrote, and the printer runs inside
 * the compiler. Nothing about Scala's semantics is interpreted — only
 * the literal shape of a value built from this module's own
 * constructors. That is the whole reason there is no `Long` question
 * and no equality question here: this is not a compiler.
 *
 * A tree it cannot read is a COMPILE ERROR naming what it could not
 * read, never a silent fall back to runtime: a constant somebody
 * expected in the jar, quietly computed at every startup instead, is
 * exactly the kind of thing nobody notices until a cold start is
 * measured.
 */
object Emit:

  // ---- unlifting, declared before it is used ------------------------
  //
  // One `FromExpr` per case, which is the price of not interpreting
  // Scala: the compiler reads a VALUE built from constructors, and a
  // value built any other way is refused rather than guessed at. They
  // are NAMED because they are mutually recursive — a `Call` holds
  // arguments, an argument may be a `Fun`, a `Fun` holds statements.

  given vectorOf[T](using FromExpr[T], Type[T]): FromExpr[Vector[T]] with
    def unapply(x: Expr[Vector[T]])(using Quotes): Option[Vector[T]] = x match
      case '{ Vector[T](${ Varargs(Exprs(items)) }*) } => Some(items.toVector)
      case '{ Vector.empty[T] } => Some(Vector.empty)
      case '{ scala.collection.immutable.Vector[T](${ Varargs(Exprs(items)) }*) } =>
        Some(items.toVector)
      case _ => None

  given pairOf[A, B](using FromExpr[A], Type[A], FromExpr[B], Type[B]): FromExpr[(A, B)] with
    def unapply(x: Expr[(A, B)])(using Quotes): Option[(A, B)] = x match
      case '{ ($a: A) -> ($b: B) } => for l <- a.value; r <- b.value yield (l, r)
      case '{ Tuple2(${ Expr(a) }: A, ${ Expr(b) }: B) } => Some((a, b))
      case _ => None

  given jsOf: FromExpr[Js] with
    def unapply(x: Expr[Js])(using Quotes): Option[Js] = x match
      case '{ Js.Num(${ Expr(v) }) } => Some(Js.Num(v))
      case '{ Js.Str(${ Expr(s) }) } => Some(Js.Str(s))
      case '{ Js.Bool(${ Expr(b) }) } => Some(Js.Bool(b))
      case '{ Js.Null } => Some(Js.Null)
      case '{ Js.Undefined } => Some(Js.Undefined)
      case '{ Js.Name(${ Expr(id) }) } => Some(Js.Name(id))
      case '{ Js.Raw(${ Expr(s) }) } => Some(Js.Raw(s))
      case '{ Js.Arr(${ Expr(items) }) } => Some(Js.Arr(items))
      case '{ Js.Obj(${ Expr(fields) }) } => Some(Js.Obj(fields))
      case '{ Js.Field(${ Expr(of) }, ${ Expr(n) }) } => Some(Js.Field(of, n))
      case '{ Js.Index(${ Expr(of) }, ${ Expr(a) }) } => Some(Js.Index(of, a))
      case '{ Js.Call(${ Expr(f) }, ${ Expr(as) }) } => Some(Js.Call(f, as))
      case '{ Js.New(${ Expr(f) }, ${ Expr(as) }) } => Some(Js.New(f, as))
      case '{ Js.Unary(${ Expr(op) }, ${ Expr(of) }) } => Some(Js.Unary(op, of))
      case '{ Js.Bin(${ Expr(op) }, ${ Expr(l) }, ${ Expr(r) }) } => Some(Js.Bin(op, l, r))
      case '{ Js.Ternary(${ Expr(c) }, ${ Expr(y) }, ${ Expr(n) }) } => Some(Js.Ternary(c, y, n))
      case '{ Js.Fun(${ Expr(ps) }, ${ Expr(b) }) } => Some(Js.Fun(ps, b))
      case '{ Js.TypedFun(${ Expr(ps) }, ${ Expr(b) }) } => Some(Js.TypedFun(ps, b))
      case _ => None

  given stmtOf: FromExpr[Stmt] with
    def unapply(x: Expr[Stmt])(using Quotes): Option[Stmt] = x match
      case '{ Stmt.Var(${ Expr(n) }, ${ Expr(v) }) } => Some(Stmt.Var(n, v))
      case '{ Stmt.TypedVar(${ Expr(n) }, ${ Expr(t) }, ${ Expr(v) }) } => Some(Stmt.TypedVar(n, t, v))
      case '{ Stmt.Set(${ Expr(t) }, ${ Expr(v) }) } => Some(Stmt.Set(t, v))
      case '{ Stmt.Do(${ Expr(o) }) } => Some(Stmt.Do(o))
      case '{ Stmt.Return(${ Expr(o) }) } => Some(Stmt.Return(o))
      case '{ Stmt.If(${ Expr(c) }, ${ Expr(y) }, ${ Expr(n) }) } => Some(Stmt.If(c, y, n))
      case '{ Stmt.If(${ Expr(c) }, ${ Expr(y) }) } => Some(Stmt.If(c, y, Vector.empty))
      case '{ Stmt.While(${ Expr(c) }, ${ Expr(b) }) } => Some(Stmt.While(c, b))
      case '{ Stmt.For(${ Expr(i) }, ${ Expr(c) }, ${ Expr(s) }, ${ Expr(b) }) } =>
        Some(Stmt.For(i, c, s, b))
      case '{ Stmt.Switch(${ Expr(on) }, ${ Expr(cs) }, ${ Expr(f) }) } =>
        Some(Stmt.Switch(on, cs, f))
      case '{ Stmt.Switch(${ Expr(on) }, ${ Expr(cs) }) } =>
        Some(Stmt.Switch(on, cs, Vector.empty))
      case '{ Stmt.Break } => Some(Stmt.Break)
      case '{ Stmt.Block(${ Expr(b) }) } => Some(Stmt.Block(b))
      case '{ Stmt.Comment(${ Expr(s) }) } => Some(Stmt.Comment(s))
      case '{ Stmt.Raw(${ Expr(s) }) } => Some(Stmt.Raw(s))
      case _ => None

  // ---- the two entry points -----------------------------------------

  /**
   * The printed source of a tree the compiler can read.
   *
   * TRANSPARENT, and that is not decoration: a transparent inline def
   * takes the type of what it produced, so the result is the SINGLETON
   * type of the text. That is what lets a caller write `inline val`
   * and have the compiler refuse it if the work did not happen at
   * compile time — the property is checkable rather than asserted.
   */
  transparent inline def emit(inline js: Js): String = ${ emitExpr('js) }

  /** the printed source of a program the compiler can read */
  transparent inline def program(inline stmts: Vector[Stmt]): String =
    ${ programExpr('stmts) }

  def emitExpr(js: Expr[Js])(using Quotes): Expr[String] =
    js.value match
      case Some(tree) => constant(Js.print(tree))
      case None => unreadable(js)

  def programExpr(stmts: Expr[Vector[Stmt]])(using Quotes): Expr[String] =
    stmts.value match
      case Some(tree) => constant(Js.print(tree))
      case None => unreadable(stmts)

  /** a literal, so the expression's TYPE is the text and not merely
   * `String` */
  private def constant(text: String)(using q: Quotes): Expr[String] =
    import q.reflect.*
    Literal(StringConstant(text)).asExprOf[String]

  private def unreadable(e: Expr[?])(using q: Quotes): Nothing =
    import q.reflect.*
    report.errorAndAbort(
      "this tree cannot be read at compile time, so there is no constant to emit.\n" +
        "  Build it from Js's own constructors and literals, or call Js.print for the\n" +
        "  runtime road — which is the right answer whenever the tree depends on\n" +
        "  anything the compiler does not know.\n" +
        s"  what could not be read: ${e.show}",
      e)
