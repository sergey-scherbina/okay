package okay.semantic.ossie

private[ossie] object Expressions:
  case class Part(value: String, quoted: Boolean):
    def matches(name: String): Boolean = if quoted then value == name else value.equalsIgnoreCase(name)
  case class Ref(parts: Vector[Part])
  enum Token:
    case MetricRef(ref: Ref)
    case Aggregate(function: String, field: Option[Ref], distinct: Boolean)
    case Number(value: BigDecimal)
    case Operator(value: Char)

  /** Shunting yard, with aggregates as atoms. No recursive expression/tree descent. */
  def parse(input: String): Either[String,Vector[Token]] =
    val out = Vector.newBuilder[Token]
    var operators = List.empty[Char]
    var at = 0
    var expecting = true
    var error: Option[String] = None
    def fail(why: String): Unit = if error.isEmpty then error = Some(s"expression at $at: $why")
    def space(): Unit = while at < input.length && input(at).isWhitespace do at += 1
    def identifier(): Option[Ref] =
      val parts = Vector.newBuilder[Part]
      var more = true
      while more && error.isEmpty do
        space()
        if at >= input.length then fail("expected identifier")
        else if input(at) == '"' then
          at += 1
          val name = new StringBuilder
          var closed = false
          while at < input.length && !closed do
            if input(at) == '"' then
              if at + 1 < input.length && input(at + 1) == '"' then { name.append('"'); at += 2 }
              else { closed = true; at += 1 }
            else { name.append(input(at)); at += 1 }
          if !closed || name.isEmpty then fail("invalid quoted identifier")
          else parts += Part(name.toString,true)
        else if input(at).isLetter || input(at) == '_' then
          val start = at; at += 1
          while at < input.length && (input(at).isLetterOrDigit || input(at) == '_') do at += 1
          parts += Part(input.substring(start,at),false)
        else fail("expected identifier")
        space()
        if at < input.length && input(at) == '.' then at += 1 else more = false
      if error.nonEmpty then None else Some(Ref(parts.result()))
    def emit(token: Token): Unit =
      if !expecting then fail("missing operator") else { out += token; expecting = false }
    def precedence(c: Char): Int = if c == '~' then 3 else if c == '*' || c == '/' then 2 else 1
    while at < input.length && error.isEmpty do
      space()
      if at < input.length then input(at) match
        case '(' =>
          if !expecting then fail("missing operator before parenthesis")
          else { operators = '(' :: operators; at += 1 }
        case ')' =>
          if expecting then fail("empty or incomplete parenthesis")
          else
            while operators.nonEmpty && operators.head != '(' do
              out += Token.Operator(operators.head); operators = operators.tail
            if operators.isEmpty then fail("unmatched closing parenthesis") else { operators = operators.tail; at += 1 }
        case '+' | '-' | '*' | '/' =>
          val op = input(at); at += 1
          if expecting then
            if op == '-' then operators = '~' :: operators
            else if op != '+' then fail("expected operand")
          else
            while operators.nonEmpty && operators.head != '(' && precedence(operators.head) >= precedence(op) do
              out += Token.Operator(operators.head); operators = operators.tail
            operators = op :: operators; expecting = true
        case c if c.isDigit || c == '.' =>
          val start = at
          while at < input.length && (input(at).isDigit || input(at) == '.') do at += 1
          if at < input.length && (input(at) == 'e' || input(at) == 'E') then
            at += 1
            if at < input.length && (input(at) == '+' || input(at) == '-') then at += 1
            while at < input.length && input(at).isDigit do at += 1
          scala.util.Try(BigDecimal(input.substring(start,at))).toOption match
            case Some(number) => emit(Token.Number(number))
            case None => fail("invalid exact decimal")
        case c if c.isLetter || c == '_' || c == '"' =>
          identifier().foreach { ref =>
            space()
            if at < input.length && input(at) == '(' then
              if ref.parts.size != 1 || ref.parts.head.quoted then fail("unsupported function name")
              else
                val function = ref.parts.head.value.map(c => if c >= 'a' && c <= 'z' then (c - 32).toChar else c)
                if !Set("SUM","COUNT","AVG","MIN","MAX")(function) then fail(s"unsupported function $function")
                else
                  at += 1; space()
                  var distinct = false
                  val maybeDistinct = input.substring(at).takeWhile(c => c.isLetter)
                  if maybeDistinct.equalsIgnoreCase("DISTINCT") && at + maybeDistinct.length < input.length && input(at + maybeDistinct.length).isWhitespace then { distinct = true; at += maybeDistinct.length; space() }
                  val field = if at < input.length && input(at) == '*' then { at += 1; None } else identifier()
                  space()
                  if at >= input.length || input(at) != ')' then fail("aggregate requires one field or COUNT(*)")
                  else if (field.isEmpty && (function != "COUNT" || distinct)) || (distinct && function != "COUNT") then fail("unsupported aggregate operand")
                  else { at += 1; emit(Token.Aggregate(function,field,distinct)) }
            else emit(Token.MetricRef(ref))
          }
        case other => fail(s"unsupported token $other")
    if error.isEmpty && expecting then fail("missing operand")
    while operators.nonEmpty && error.isEmpty do
      if operators.head == '(' then fail("unclosed parenthesis") else out += Token.Operator(operators.head)
      operators = operators.tail
    error.toLeft(out.result())
