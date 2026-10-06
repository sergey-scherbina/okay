package okay.semantic.ossie

import okay.semantic.Value

/** A vendor adapter returns portable expression text; it never executes source SQL. */
trait Language:
  def normalize(dialect: String, expression: String): Either[String,String]
object Language:
  val portable: Language = new Language:
    def normalize(dialect: String, expression: String): Either[String,String] =
      if Set("ANSI_SQL","OSSIE_SQL_2026")(dialect) then Right(expression)
      else Left(s"dialect $dialect requires an explicit Language adapter")
  /** Explicit opt-in for the common expression subset, not whole dialect equivalence. */
  val sqlFamily: Language = new Language:
    def normalize(dialect: String, expression: String): Either[String,String] =
      if Set("ANSI_SQL","OSSIE_SQL_2026","SNOWFLAKE","BIGQUERY","DATABRICKS")(dialect) then Right(expression)
      else Left(s"dialect $dialect requires a Language adapter")

private[ossie] object Program:
  final case class Name(parts: Vector[Expressions.Part]):
    def text: String = parts.map(_.value).mkString(".")
  final case class Sort(node: Int, descending: Boolean, nullsFirst: Boolean)
  final case class Window(partition: Vector[Int], order: Vector[Sort], frame: Option[(Int,Int)])
  enum Node:
    case Literal(value: Value)
    case Reference(name: Name)
    case Unary(op: String, child: Int)
    case Binary(op: String, left: Int, right: Int)
    case Call(name: String, args: Vector[Int], distinct: Boolean = false, filter: Option[Int] = None, window: Option[Window] = None)
    case Conditional(branches: Vector[(Int,Int)], otherwise: Int)
    def children: Vector[Int] = this match
      case Unary(_,c) => Vector(c)
      case Binary(_,l,r) => Vector(l,r)
      case Call(_,args,_,filter,window) => args ++ filter.toVector ++ window.toVector.flatMap(w => w.partition ++ w.order.map(_.node))
      case Conditional(bs,o) => bs.flatMap((c,v) => Vector(c,v)) :+ o
      case _ => Vector.empty
  final case class Tree(nodes: Vector[Node], root: Int)
  val aggregates: Set[String] = Set("SUM","COUNT","AVG","MIN","MAX","MEDIAN","STDDEV","STDDEV_POP","STDDEV_SAMP","VARIANCE","VAR_POP","VAR_SAMP","PERCENTILE_CONT","PERCENTILE_DISC")
  val windows: Set[String] = Set("ROW_NUMBER","RANK","DENSE_RANK","LAG","LEAD","FIRST_VALUE","LAST_VALUE","NTILE")
  val MaxDepth = 128
  private enum Lex:
    case Word(value: String, quoted: Boolean = false)
    case Str(value: String)
    case Num(value: BigDecimal)
    case Symbol(value: String)
  private def upper(s: String): String = s.map(c => if c >= 'a' && c <= 'z' then (c - 32).toChar else c)
  private def lex(input: String): Either[String,Vector[Lex]] =
    val out = Vector.newBuilder[Lex]
    var at = 0
    var error: Option[String] = None
    while at < input.length && error.isEmpty do
      val c = input(at)
      if c.isWhitespace then at += 1
      else if c == '\'' || c == '"' || c == '`' then
        val quote = c; at += 1
        val value = new StringBuilder
        var closed = false
        while at < input.length && !closed do
          if input(at) == quote then
            if at + 1 < input.length && input(at + 1) == quote then { value.append(quote); at += 2 }
            else { closed = true; at += 1 }
          else { value.append(input(at)); at += 1 }
        if !closed then error = Some("unclosed quoted value")
        else out += (if quote == '\'' then Lex.Str(value.toString) else Lex.Word(value.toString,true))
      else if c.isDigit || (c == '.' && at + 1 < input.length && input(at + 1).isDigit) then
        val start = at
        while at < input.length && (input(at).isDigit || input(at) == '.') do at += 1
        if at < input.length && Set('e','E')(input(at)) then
          at += 1
          if at < input.length && Set('+','-')(input(at)) then at += 1
          while at < input.length && input(at).isDigit do at += 1
        scala.util.Try(BigDecimal(input.substring(start,at))).toOption match
          case Some(n) => out += Lex.Num(n)
          case None => error = Some("invalid decimal")
      else if c.isLetter || c == '_' then
        val start = at; at += 1
        while at < input.length && (input(at).isLetterOrDigit || input(at) == '_') do at += 1
        out += Lex.Word(input.substring(start,at))
      else
        val two = input.substring(at, (at + 2).min(input.length))
        if Set("<=",">=","<>","!=","||")(two) then { out += Lex.Symbol(two); at += 2 }
        else if "()+-*/%,.=<>".contains(c) then { out += Lex.Symbol(c.toString); at += 1 }
        else error = Some(s"unexpected character at $at: $c")
    error.toLeft(out.result())

  def parse(input: String): Either[String,Tree] = lex(input).flatMap(ts => new Parser(ts).parse())
  private final class Parser(tokens: Vector[Lex]):
    private val nodes = scala.collection.mutable.ArrayBuffer.empty[Node]
    private var at = 0
    private var error: Option[String] = None
    private def fail(message: String): Unit = if error.isEmpty then error = Some(s"expression token $at: $message")
    private def add(n: Node): Int = { nodes += n; nodes.size - 1 }
    private def is(s: String): Boolean = tokens.lift(at).exists {
      case Lex.Word(w,false) => upper(w) == s
      case Lex.Symbol(w) => w == s
      case _ => false }
    private def take(s: String): Boolean = if is(s) then { at += 1; true } else false
    private def need(s: String): Unit = if !take(s) then fail(s"expected $s")
    private def nullNode: Int = add(Node.Literal(Value.Null))
    private def priority: Int =
      if is("OR") then 1 else if is("AND") then 2
      else if Vector("=","<>","!=","<",">","<=",">=","IS","IN","LIKE","ILIKE","BETWEEN").exists(is) then 3
      else if is("+") || is("-") || is("||") then 4
      else if is("*") || is("/") || is("%") then 5 else 0
    // BOUNDED: every recursive parse edge increments depth, and stops at MaxDepth (128).
    private def expression(minimum: Int, depth: Int): Int =
      if depth > MaxDepth then { fail(s"nesting exceeds $MaxDepth"); nullNode }
      else
        var left = atom(depth + 1)
        var p = priority
        while p >= minimum && p > 0 && error.isEmpty do
          val op = tokens(at) match
            case Lex.Word(w,_) => upper(w)
            case Lex.Symbol(s) => s
            case _ => ""
          at += 1
          if op == "IS" then
            val negative = take("NOT"); need("NULL")
            left = add(Node.Unary(if negative then "IS NOT NULL" else "IS NULL",left))
          else if op == "IN" then
            need("(")
            val values = arguments(depth + 1)
            left = add(Node.Call("IN",left +: values))
          else if op == "BETWEEN" then
            val low = expression(4,depth + 1); need("AND"); val high = expression(4,depth + 1)
            left = add(Node.Call("BETWEEN",Vector(left,low,high)))
          else left = add(Node.Binary(op,left,expression(p + 1,depth + 1)))
          p = priority
        left
    private def arguments(depth: Int): Vector[Int] =
      val args = Vector.newBuilder[Int]
      if !take(")") then
        args += expression(1,depth + 1)
        while take(",") && error.isEmpty do args += expression(1,depth + 1)
        need(")")
      args.result()
    private def atom(depth: Int): Int =
      if depth > MaxDepth then { fail(s"nesting exceeds $MaxDepth"); nullNode }
      else if take("(") then
        val n = expression(1,depth + 1); need(")"); n
      else if take("-") then add(Node.Unary("-",expression(6,depth + 1)))
      else if take("+") then expression(6,depth + 1)
      else if take("NOT") then add(Node.Unary("NOT",expression(3,depth + 1)))
      else if take("NULL") then nullNode
      else if take("TRUE") then add(Node.Literal(Value.Bool(true)))
      else if take("FALSE") then add(Node.Literal(Value.Bool(false)))
      else if take("CASE") then
        val base = if is("WHEN") then None else Some(expression(1,depth + 1))
        val branches = Vector.newBuilder[(Int,Int)]
        while take("WHEN") && error.isEmpty do
          val cond = expression(1,depth + 1)
          val checked = base.fold(cond)(b => add(Node.Binary("=",b,cond)))
          need("THEN"); branches += checked -> expression(1,depth + 1)
        val bs = branches.result()
        if bs.isEmpty then fail("CASE needs WHEN")
        val otherwise = if take("ELSE") then expression(1,depth + 1) else nullNode
        need("END"); add(Node.Conditional(bs,otherwise))
      else tokens.lift(at) match
        case Some(Lex.Num(n)) => at += 1; add(Node.Literal(Value.Number(n)))
        case Some(Lex.Str(s)) => at += 1; add(Node.Literal(Value.Text(s)))
        case Some(Lex.Word(w,quoted)) =>
          at += 1
          if take("(") then
            val fn = upper(w)
            val distinct = take("DISTINCT")
            val args = if take("*") then { need(")"); Vector.empty } else arguments(depth + 1)
            var actual = args
            if take("WITHIN") then
              need("GROUP"); need("("); need("ORDER"); need("BY")
              actual = args :+ expression(1,depth + 1); need(")")
            val filter = if take("FILTER") then
              need("("); need("WHERE"); val f = expression(1,depth + 1); need(")"); Some(f)
            else None
            val window = if take("OVER") then Some(windowSpec(depth + 1)) else None
            add(Node.Call(fn,actual,distinct,filter,window))
          else
            val parts = Vector.newBuilder[Expressions.Part]
            parts += Expressions.Part(w,quoted)
            while take(".") && error.isEmpty do tokens.lift(at) match
              case Some(Lex.Word(v,q)) => parts += Expressions.Part(v,q); at += 1
              case _ => fail("expected qualified field")
            add(Node.Reference(Name(parts.result())))
        case _ => fail("expected operand"); nullNode
    private def windowSpec(depth: Int): Window =
      need("(")
      val partition = Vector.newBuilder[Int]
      if take("PARTITION") then
        need("BY"); partition += expression(1,depth + 1)
        while take(",") && error.isEmpty do partition += expression(1,depth + 1)
      val order = Vector.newBuilder[Sort]
      if take("ORDER") then
        need("BY")
        var more = true
        while more && error.isEmpty do
          val n = expression(1,depth + 1)
          val desc = take("DESC"); if !desc then { val _ = take("ASC") }
          var first = false
          if take("NULLS") then
            if take("FIRST") then first = true else need("LAST")
          order += Sort(n,desc,first); more = take(",")
      val frame = if take("ROWS") then
        if take("BETWEEN") then
          val start = boundary(); need("AND"); Some(start -> boundary())
        else Some(boundary() -> 0)
      else None
      need(")"); Window(partition.result(),order.result(),frame)
    private def boundary(): Int =
      if take("UNBOUNDED") then
        if take("PRECEDING") then Int.MinValue else { need("FOLLOWING"); Int.MaxValue }
      else if take("CURRENT") then { need("ROW"); 0 }
      else tokens.lift(at) match
        case Some(Lex.Num(n)) if n.isWhole && n.isValidInt && n >= 0 =>
          at += 1; val count = n.toInt
          if take("PRECEDING") then -count else { need("FOLLOWING"); count }
        case _ => fail("invalid ROWS boundary"); 0
    def parse(): Either[String,Tree] =
      val root = expression(1,0)
      if at != tokens.size then fail("unexpected trailing input")
      error.toLeft(Tree(nodes.toVector,root))
