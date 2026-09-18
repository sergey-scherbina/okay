package okay.js

/**
 * JavaScript AS A VALUE (specs/js.md).
 *
 * NOT a compiler. Nothing here translates Scala: the author writes
 * the JavaScript's STRUCTURE, typed, and the printer writes the text.
 * That is deliberate and it is what makes this small — emitting
 * JavaScript from Scala EXPRESSIONS would mean deciding the semantics
 * of `Long`, of equality, of exceptions, of closures and of `match`
 * desugaring, which is the work Scala.js already is (and which this
 * build already cross-compiles with).
 *
 * It is the move the rest of this codebase makes everywhere: `Ui` is
 * a tree that `Html`, `React` and `Frame` interpret; `Schema` is a
 * tree the codecs interpret; `Json` is a tree. So JavaScript is a
 * tree, and printing is its interpreter.
 *
 * What that buys over a string literal is what `okay.ui.LiveJs`
 * already proved with the one line it generates rather than types:
 * a program you can build from other data cannot drift from that
 * data.
 */
enum Js:
  case Num(v: Double)
  case Str(s: String)
  case Bool(b: Boolean)
  case Null
  case Undefined
  /** an identifier, or a dotted path already known to be good */
  case Name(id: String)
  case Arr(items: Vector[Js])
  case Obj(fields: Vector[(String, Js)])
  case Field(of: Js, name: String)
  case Index(of: Js, at: Js)
  case Call(fn: Js, args: Vector[Js])
  case New(fn: Js, args: Vector[Js])
  case Unary(op: String, of: Js)
  case Bin(op: String, l: Js, r: Js)
  case Ternary(cond: Js, yes: Js, no: Js)
  case Fun(params: Vector[String], body: Vector[Stmt])
  /** THE ESCAPE HATCH, AND IT IS NAMED.
   *
   * A tree that could not express a regex literal or a `try/catch`
   * would push its author back to concatenating strings for the whole
   * file. This keeps the rest typed and marks the one place that is
   * not — which is the difference between a hole a review can see and
   * one it cannot. `Js.raws` counts them. */
  case Raw(source: String)

/** a statement, which is a different thing from an expression.
 *
 * JavaScript separates them, and a printer that pretends otherwise
 * emits `var x = if (...)`, which does not parse. */
enum Stmt:
  case Var(name: String, value: Js)
  case Set(target: Js, value: Js)
  /** an expression evaluated for its effect */
  case Do(of: Js)
  case Return(of: Option[Js])
  case If(cond: Js, yes: Vector[Stmt], no: Vector[Stmt] = Vector.empty)
  case While(cond: Js, body: Vector[Stmt])
  case For(init: Option[Stmt], cond: Option[Js], step: Option[Js],
           body: Vector[Stmt])
  case Switch(on: Js, cases: Vector[(Js, Vector[Stmt])],
              fallback: Vector[Stmt] = Vector.empty)
  case Break
  case Block(body: Vector[Stmt])
  case Raw(source: String)

object Js:

  // ---- building, so a caller writes JavaScript and not constructors

  def name(id: String): Js = Name(id)
  def str(s: String): Js = Str(s)
  def num(v: Double): Js = Num(v)
  def bool(b: Boolean): Js = Bool(b)

  extension (self: Js)
    /** `self.name` */
    def dot(name: String): Js = Field(self, name)
    /** `self[at]` */
    def at(at: Js): Js = Index(self, at)
    /** `self(args...)` */
    def of(args: Js*): Js = Call(self, args.toVector)
    def ===(other: Js): Js = Bin("===", self, other)
    def !==(other: Js): Js = Bin("!==", self, other)
    def +(other: Js): Js = Bin("+", self, other)
    def &&(other: Js): Js = Bin("&&", self, other)
    def ||(other: Js): Js = Bin("||", self, other)
    def not: Js = Unary("!", self)

  def obj(fields: (String, Js)*): Js = Obj(fields.toVector)
  def arr(items: Js*): Js = Arr(items.toVector)
  def fun(params: String*)(body: Stmt*): Js = Fun(params.toVector, body.toVector)

  /** how many escape hatches a program carries. A number a test can
   * hold to zero, or to a stated few */
  def raws(stmts: Vector[Stmt]): Int =
    def inJs(j: Js): Int = j match
      case Raw(_) => 1
      case Arr(xs) => xs.map(inJs).sum
      case Obj(fs) => fs.map((_, v) => inJs(v)).sum
      case Field(o, _) => inJs(o)
      case Index(o, a) => inJs(o) + inJs(a)
      case Call(f, as) => inJs(f) + as.map(inJs).sum
      case New(f, as) => inJs(f) + as.map(inJs).sum
      case Unary(_, o) => inJs(o)
      case Bin(_, l, r) => inJs(l) + inJs(r)
      case Ternary(c, y, n) => inJs(c) + inJs(y) + inJs(n)
      case Fun(_, b) => b.map(inStmt).sum
      case _ => 0
    def inStmt(s: Stmt): Int = s match
      case Stmt.Raw(_) => 1
      case Stmt.Var(_, v) => inJs(v)
      case Stmt.Set(t, v) => inJs(t) + inJs(v)
      case Stmt.Do(o) => inJs(o)
      case Stmt.Return(o) => o.map(inJs).getOrElse(0)
      case Stmt.If(c, y, n) => inJs(c) + y.map(inStmt).sum + n.map(inStmt).sum
      case Stmt.While(c, b) => inJs(c) + b.map(inStmt).sum
      case Stmt.For(i, c, st, b) =>
        i.map(inStmt).getOrElse(0) + c.map(inJs).getOrElse(0) +
          st.map(inJs).getOrElse(0) + b.map(inStmt).sum
      case Stmt.Switch(on, cs, f) =>
        inJs(on) + cs.map((k, b) => inJs(k) + b.map(inStmt).sum).sum + f.map(inStmt).sum
      case Stmt.Block(b) => b.map(inStmt).sum
      case Stmt.Break => 0
    stmts.map(inStmt).sum

  // ---- printing ----------------------------------------------------

  /** one expression, as source */
  def print(j: Js): String =
    val sb = StringBuilder()
    expr(j, 0, sb)
    sb.result()

  /** a program, as source */
  def print(stmts: Vector[Stmt]): String =
    val sb = StringBuilder()
    stmts.foreach(stmt(_, 0, sb))
    sb.result()

  /**
   * A STRING LITERAL, AND THE ESCAPING IS A SECURITY PROPERTY.
   *
   * Three of these are not about readability:
   *
   * - `</script` ends a script element WHEREVER it appears, including
   *   inside a string literal, so a generator that emits data into a
   *   page can be ended by its own data. The `<` is escaped, which is
   *   enough and leaves the text readable.
   * - U+2028 and U+2029 are line terminators in JavaScript and are
   *   NOT in JSON, so a value that round-tripped through JSON can
   *   still break a script.
   * - the control range below 0x20 is escaped because a raw newline
   *   in a literal does not parse.
   */
  def quote(s: String): String =
    val sb = StringBuilder("\"")
    s.foreach {
      case '"' => sb ++= "\\\""
      case '\\' => sb ++= "\\\\"
      case '\n' => sb ++= "\\n"
      case '\r' => sb ++= "\\r"
      case '\t' => sb ++= "\\t"
      case '\b' => sb ++= "\\b"
      case '\f' => sb ++= "\\f"
      // the one that ends the document it is written into
      case '<' => sb ++= "\\u003c"
      // U+2028 and U+2029 BY CODE, never as a Scala escape: `\\uXXXX`
      // in Scala source is processed before the parser sees it, so
      // writing the escape here would put a real line separator in
      // this file and break it
      case c if c.toInt == 0x2028 || c.toInt == 0x2029 =>
        sb ++= f"\\u${c.toInt}%04x"
      case c if c < ' ' => sb ++= f"\\u${c.toInt}%04x"
      case c => sb += c
    }
    sb += '"'
    sb.result()

  /** an object key needs no quotes when it is a plain identifier, and
   * must have them otherwise. `{class: 1}` is a reserved word in old
   * engines, so a key that is not `[A-Za-z_$][A-Za-z0-9_$]*` or that
   * is reserved is quoted */
  def key(k: String): String =
    val plain = k.nonEmpty && !k.head.isDigit &&
      k.forall(c => c.isLetterOrDigit || c == '_' || c == '$') &&
      k.forall(_ < 128) && !reserved(k)
    if plain then k else quote(k)

  private val reserved: Set[String] = Set(
    "break", "case", "catch", "class", "const", "continue", "debugger",
    "default", "delete", "do", "else", "export", "extends", "finally",
    "for", "function", "if", "import", "in", "instanceof", "new", "return",
    "super", "switch", "this", "throw", "try", "typeof", "var", "void",
    "while", "with", "yield", "let", "static", "enum", "await", "implements",
    "package", "protected", "interface", "private", "public", "null",
    "true", "false")

  /**
   * PRECEDENCE, so the printer parenthesises what must be and nothing
   * else.
   *
   * Always-parenthesising would be correct and is what a first draft
   * does; it is rejected because this output is served on every page
   * load and because a person reads it when something is wrong. The
   * numbers are JavaScript's own, low binds loosest.
   */
  private def prec(j: Js): Int = j match
    case _: (Num | Str | Bool | Name | Arr | Obj | Raw) => 20
    case Null | Undefined => 20
    case _: (Field | Index | Call) => 18
    case _: New => 18
    case _: Unary => 14
    case Bin(op, _, _) => op match
      case "*" | "/" | "%" => 13
      case "+" | "-" => 12
      case "<<" | ">>" | ">>>" => 11
      case "<" | ">" | "<=" | ">=" | "instanceof" | "in" => 10
      case "==" | "!=" | "===" | "!==" => 9
      case "&" => 8
      case "^" => 7
      case "|" => 6
      case "&&" => 5
      case "||" => 4
      case _ => 4
    case _: Ternary => 3
    case _: Fun => 20

  private def expr(j: Js, outer: Int, sb: StringBuilder): Unit =
    val mine = prec(j)
    val wrap = mine < outer
    if wrap then sb += '('
    j match
      case Num(v) =>
        sb ++= (if v == v.toLong.toDouble && v.abs < 1e15 then v.toLong.toString else v.toString)
      case Str(s) => sb ++= quote(s)
      case Bool(b) => sb ++= b.toString
      case Null => sb ++= "null"
      case Undefined => sb ++= "undefined"
      case Name(id) => sb ++= id
      case Raw(source) => sb ++= source
      case Arr(items) =>
        sb += '['
        items.zipWithIndex.foreach { (x, i) =>
          if i > 0 then sb ++= ", "
          expr(x, 0, sb)
        }
        sb += ']'
      case Obj(fields) =>
        sb += '{'
        fields.zipWithIndex.foreach { case ((k, v), i) =>
          if i > 0 then sb ++= ", "
          sb ++= key(k) ++= ": "
          expr(v, 0, sb)
        }
        sb += '}'
      case Field(of, n) =>
        expr(of, mine, sb)
        sb += '.' ++= n
      case Index(of, a) =>
        expr(of, mine, sb)
        sb += '['
        expr(a, 0, sb)
        sb += ']'
      case Call(fn, args) =>
        expr(fn, mine, sb)
        sb += '('
        args.zipWithIndex.foreach { (x, i) =>
          if i > 0 then sb ++= ", "
          expr(x, 0, sb)
        }
        sb += ')'
      case New(fn, args) =>
        sb ++= "new "
        expr(fn, mine, sb)
        sb += '('
        args.zipWithIndex.foreach { (x, i) =>
          if i > 0 then sb ++= ", "
          expr(x, 0, sb)
        }
        sb += ')'
      case Unary(op, of) =>
        sb ++= op
        // `typeof x` needs the space; `!x` does not
        if op.head.isLetter then sb += ' '
        // AND A UNARY ON A UNARY IS PARENTHESISED, because `-(-x)`
        // printed as `--x` is a DECREMENT — a different program, not
        // a different spelling. Found by the test that expected the
        // parentheses, which is the only reason it is not still there
        of match
          case _: Unary => sb += '('; expr(of, 0, sb); sb += ')'
          case _ => expr(of, mine, sb)
      case Bin(op, l, r) =>
        expr(l, mine, sb)
        sb += ' ' ++= op += ' '
        // the right side of a left-associative operator binds tighter
        expr(r, mine + 1, sb)
      case Ternary(c, y, n) =>
        expr(c, mine + 1, sb)
        sb ++= " ? "
        expr(y, 0, sb)
        sb ++= " : "
        expr(n, 0, sb)
      case Fun(params, body) =>
        sb ++= "function (" ++= params.mkString(", ") ++= ") {"
        body.foreach(stmt(_, 1, sb))
        sb ++= "}"
    if wrap then sb += ')'

  private def pad(depth: Int, sb: StringBuilder): Unit =
    sb += '\n'
    var i = 0
    while i < depth do { sb ++= "  "; i += 1 }

  /** SEMICOLONS ARE WRITTEN, NEVER INFERRED. Automatic semicolon
   * insertion is a language feature nobody should depend on twice */
  private def stmt(s: Stmt, depth: Int, sb: StringBuilder): Unit =
    pad(depth, sb)
    s match
      case Stmt.Var(n, v) =>
        sb ++= "var " ++= n ++= " = "
        expr(v, 0, sb)
        sb += ';'
      case Stmt.Set(t, v) =>
        expr(t, 0, sb)
        sb ++= " = "
        expr(v, 0, sb)
        sb += ';'
      case Stmt.Do(o) =>
        expr(o, 0, sb)
        sb += ';'
      case Stmt.Return(None) => sb ++= "return;"
      case Stmt.Return(Some(o)) =>
        sb ++= "return "
        expr(o, 0, sb)
        sb += ';'
      case Stmt.If(c, yes, no) =>
        sb ++= "if ("
        expr(c, 0, sb)
        sb ++= ") {"
        yes.foreach(stmt(_, depth + 1, sb))
        pad(depth, sb)
        sb += '}'
        if no.nonEmpty then
          sb ++= " else {"
          no.foreach(stmt(_, depth + 1, sb))
          pad(depth, sb)
          sb += '}'
      case Stmt.While(c, body) =>
        sb ++= "while ("
        expr(c, 0, sb)
        sb ++= ") {"
        body.foreach(stmt(_, depth + 1, sb))
        pad(depth, sb)
        sb += '}'
      case Stmt.For(init, cond, step, body) =>
        sb ++= "for ("
        init.foreach {
          case Stmt.Var(n, v) => sb ++= "var " ++= n ++= " = "; expr(v, 0, sb)
          case Stmt.Set(t, v) => expr(t, 0, sb); sb ++= " = "; expr(v, 0, sb)
          case Stmt.Do(o) => expr(o, 0, sb)
          case other => sb ++= Js.print(Vector(other)).trim.stripSuffix(";")
        }
        sb ++= "; "
        cond.foreach(expr(_, 0, sb))
        sb ++= "; "
        step.foreach(expr(_, 0, sb))
        sb ++= ") {"
        body.foreach(stmt(_, depth + 1, sb))
        pad(depth, sb)
        sb += '}'
      case Stmt.Switch(on, cases, fallback) =>
        sb ++= "switch ("
        expr(on, 0, sb)
        sb ++= ") {"
        cases.foreach { (k, body) =>
          pad(depth + 1, sb)
          sb ++= "case "
          expr(k, 0, sb)
          sb += ':'
          body.foreach(stmt(_, depth + 2, sb))
        }
        if fallback.nonEmpty then
          pad(depth + 1, sb)
          sb ++= "default:"
          fallback.foreach(stmt(_, depth + 2, sb))
        pad(depth, sb)
        sb += '}'
      case Stmt.Break => sb ++= "break;"
      case Stmt.Block(body) =>
        sb += '{'
        body.foreach(stmt(_, depth + 1, sb))
        pad(depth, sb)
        sb += '}'
      case Stmt.Raw(source) => sb ++= source
