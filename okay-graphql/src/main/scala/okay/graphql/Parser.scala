package okay.graphql

import scala.collection.mutable.ArrayBuffer

/** THE BOUNDS of a request: how deep a document may nest — selections, values, types — and how deep the executor
 * follows fragments. A deeper one is an error, never a stack overflow */
final case class Limits(depth: Int = 64, fields: Int = 10000)

/** a document that does not parse: where, and why */
final case class ParseError(message: String, line: Int, column: Int):
  override def toString: String = s"$message at $line:$column"

/** THE PARSER, A FACADE (OURS OR THE STANDARD ONE): ours, `OkayGraphQLParser`, the default; another — an adapter
 * over graphql-java — is an import away (backlog) */
trait GraphQLParser:
  def parse(source: String): Either[ParseError, Document]

object GraphQLParser:
  given default: GraphQLParser = OkayGraphQLParser(Limits())

/** OURS: a lexer and a recursive descent over the executable part of the grammar (GraphQL, October 2021, §2),
 * every nesting counted against `limits.depth` */
final class OkayGraphQLParser(limits: Limits) extends GraphQLParser:
  def parse(source: String): Either[ParseError, Document] =
    try Right(OkayGraphQLParser.Descent(OkayGraphQLParser.Lexer(source).tokens, limits.depth).document())
    catch case e: OkayGraphQLParser.Failed => Left(e.error)

object OkayGraphQLParser:
  private[graphql] final class Failed(val error: ParseError) extends Exception(error.toString, null, false, false)

  private enum Kind:
    case Punct, Name, IntT, FloatT, StringT, End

  private final case class Token(kind: Kind, text: String, line: Int, column: Int)

  private def fail(message: String, line: Int, column: Int): Nothing = throw Failed(ParseError(message, line, column))

  private final class Lexer(s: String):
    private var i = 0
    private var line = 1
    private var lineStart = 0
    private val out = ArrayBuffer.empty[Token]

    def tokens: IndexedSeq[Token] =
      while next() do ()
      out.toIndexedSeq

    private def col = i - lineStart + 1
    private def newline(): Unit = { line += 1; lineStart = i }

    /** one token read, or the end: false */
    private def next(): Boolean =
      skipIgnored()
      if i >= s.length then { out += Token(Kind.End, "<end>", line, col); false }
      else
        val c = s.charAt(i)
        val l = line
        val k = col
        if "!$&()[]{}:=@|".indexOf(c) >= 0 then { out += Token(Kind.Punct, c.toString, l, k); i += 1 }
        else if c == '.' then
          if s.startsWith("...", i) then { out += Token(Kind.Punct, "...", l, k); i += 3 }
          else fail("unexpected '.'", l, k)
        else if c == '_' || c.isLetter then
          val st = i
          while i < s.length && (s.charAt(i) == '_' || s.charAt(i).isLetterOrDigit) do i += 1
          out += Token(Kind.Name, s.substring(st, i), l, k)
        else if c == '-' || c.isDigit then number(l, k)
        else if c == '"' then
          if s.startsWith("\"\"\"", i) then out += Token(Kind.StringT, block(l, k), l, k)
          else out += Token(Kind.StringT, string(l, k), l, k)
        else fail(s"unexpected character '$c'", l, k)
        true

    private def skipIgnored(): Unit =
      var go = true
      while go && i < s.length do
        s.charAt(i) match
          case ' ' | '\t' | ',' | '﻿' => i += 1
          case '\r' =>
            i += 1
            if i < s.length && s.charAt(i) == '\n' then i += 1
            newline()
          case '\n' => i += 1; newline()
          case '#' => while i < s.length && s.charAt(i) != '\n' && s.charAt(i) != '\r' do i += 1
          case _ => go = false

    private def number(l: Int, k: Int): Unit =
      val st = i
      if s.charAt(i) == '-' then i += 1
      def digits(): Int = { val d = i; while i < s.length && s.charAt(i).isDigit do i += 1; i - d }
      if digits() == 0 then fail("a number without digits", l, k)
      var float = false
      if i < s.length && s.charAt(i) == '.' then
        i += 1; float = true
        if digits() == 0 then fail("a fraction without digits", l, k)
      if i < s.length && (s.charAt(i) == 'e' || s.charAt(i) == 'E') then
        i += 1; float = true
        if i < s.length && (s.charAt(i) == '+' || s.charAt(i) == '-') then i += 1
        if digits() == 0 then fail("an exponent without digits", l, k)
      if i < s.length && (s.charAt(i) == '_' || s.charAt(i).isLetter || s.charAt(i) == '.') then
        fail("a number followed by a name", l, k)
      out += Token(if float then Kind.FloatT else Kind.IntT, s.substring(st, i), l, k)

    private def string(l: Int, k: Int): String =
      val b = StringBuilder()
      i += 1
      while i < s.length && s.charAt(i) != '"' do
        s.charAt(i) match
          case '\n' | '\r' => fail("a line break in a string", l, k)
          case '\\' if i + 1 < s.length =>
            s.charAt(i + 1) match
              case '"' => b += '"'; i += 2
              case '\\' => b += '\\'; i += 2
              case '/' => b += '/'; i += 2
              case 'b' => b += '\b'; i += 2
              case 'f' => b += '\f'; i += 2
              case 'n' => b += '\n'; i += 2
              case 'r' => b += '\r'; i += 2
              case 't' => b += '\t'; i += 2
              case 'u' if i + 6 <= s.length && s.substring(i + 2, i + 6).forall(c => Character.digit(c, 16) >= 0) =>
                b += Integer.parseInt(s.substring(i + 2, i + 6), 16).toChar; i += 6
              case c => fail(s"a bad escape '\\$c'", line, col)
          case c => b += c; i += 1
      if i >= s.length then fail("an unterminated string", l, k)
      i += 1
      b.toString

    /** a block string: raw, `\"""` the one escape, its common indentation and blank first and last lines removed */
    private def block(l: Int, k: Int): String =
      val b = StringBuilder()
      i += 3
      while i < s.length && !s.startsWith("\"\"\"", i) do
        if s.startsWith("\\\"\"\"", i) then { b ++= "\"\"\""; i += 4 }
        else
          val c = s.charAt(i)
          b += c; i += 1
          if c == '\n' then newline()
      if i >= s.length then fail("an unterminated block string", l, k)
      i += 3
      val lines = b.toString.split("\r\n|\n|\r", -1).toList
      val indents = lines.drop(1).filter(_.exists(c => c != ' ' && c != '\t')).map(_.takeWhile(c => c == ' ' || c == '\t').length)
      val common = if indents.isEmpty then 0 else indents.min
      val dedented = lines.headOption.toList ++ lines.drop(1).map(_.drop(common))
      def blank(x: String) = x.forall(c => c == ' ' || c == '\t')
      dedented.dropWhile(blank).reverse.dropWhile(blank).reverse.mkString("\n")

  /** the descent: `depth` the open nestings, at most `max` */
  private final class Descent(ts: IndexedSeq[Token], max: Int):
    private var p = 0
    private var depth = 0

    private def peek: Token = ts(p)
    private def advance(): Token = { val t = ts(p); if t.kind != Kind.End then p += 1; t }
    private def at(punct: String): Boolean = peek.kind == Kind.Punct && peek.text == punct
    private def atName(name: String): Boolean = peek.kind == Kind.Name && peek.text == name
    private def error(message: String): Nothing = fail(message, peek.line, peek.column)
    private def expect(punct: String): Unit =
      if at(punct) then advance(): Unit else error(s"expected '$punct', found '${peek.text}'")
    private def name(): String =
      if peek.kind == Kind.Name then advance().text else error(s"expected a name, found '${peek.text}'")

    /** one level deeper: refused past the bound */
    private def nested[A](body: => A): A =
      depth += 1
      if depth > max then error(s"nesting deeper than $max")
      try body finally depth -= 1

    def document(): Document =
      val ds = ArrayBuffer.empty[Definition]
      while peek.kind != Kind.End do ds += definition()
      if ds.isEmpty then error("an empty document")
      Document(ds.toList)

    private def pos: Pos = Pos(peek.line, peek.column)

    private def definition(): Definition =
      val p = pos
      if at("{") then Definition.Operation(OperationKind.Query, None, Nil, Nil, selectionSet(), p)
      else if atName("query") || atName("mutation") || atName("subscription") then
        val kind = advance().text match
          case "query" => OperationKind.Query
          case "mutation" => OperationKind.Mutation
          case _ => OperationKind.Subscription
        val n = if peek.kind == Kind.Name then Some(name()) else None
        val vs = if at("(") then variableDefinitions() else Nil
        Definition.Operation(kind, n, vs, directives(), selectionSet(), p)
      else if atName("fragment") then
        advance(): Unit
        val n = name()
        if n == "on" then error("a fragment named 'on'")
        if !atName("on") then error("expected 'on'")
        advance(): Unit
        val on = name()
        Definition.Fragment(n, on, directives(), selectionSet(), p)
      else error(s"expected a definition, found '${peek.text}'")

    private def variableDefinitions(): List[VariableDefinition] =
      expect("(")
      val vs = ArrayBuffer.empty[VariableDefinition]
      while !at(")") do
        val p = pos
        expect("$")
        val n = name()
        expect(":")
        val t = typeRef()
        val d = if at("=") then { advance(): Unit; Some(value(const = true)) } else None
        vs += VariableDefinition(n, t, d, p)
      expect(")")
      vs.toList

    private def typeRef(): TypeRef =
      val t =
        if at("[") then nested {
          advance(): Unit
          val of = typeRef()
          expect("]")
          TypeRef.ListOf(of)
        }
        else TypeRef.Named(name())
      if at("!") then { advance(): Unit; TypeRef.NonNull(t) } else t

    private def directives(): List[Directive] =
      val ds = ArrayBuffer.empty[Directive]
      while at("@") do
        val p = pos
        advance(): Unit
        val n = name()
        ds += Directive(n, if at("(") then arguments() else Nil, p)
      ds.toList

    private def arguments(): List[(String, Value)] =
      expect("(")
      val as = ArrayBuffer.empty[(String, Value)]
      while !at(")") do
        val n = name()
        expect(":")
        as += n -> value(const = false)
      if as.isEmpty then error("empty arguments")
      expect(")")
      as.toList

    private def selectionSet(): List[Selection] = nested {
      expect("{")
      val ss = ArrayBuffer.empty[Selection]
      while !at("}") do ss += selection()
      if ss.isEmpty then error("an empty selection set")
      expect("}")
      ss.toList
    }

    private def selection(): Selection =
      val p = pos
      if at("...") then
        advance(): Unit
        if peek.kind == Kind.Name && peek.text != "on" then Selection.Spread(name(), directives(), p)
        else
          val on = if atName("on") then { advance(): Unit; Some(name()) } else None
          Selection.Inline(on, directives(), selectionSet(), p)
      else
        val first = name()
        val (alias, n) = if at(":") then { advance(): Unit; (Some(first), name()) } else (None, first)
        val as = if at("(") then arguments() else Nil
        val ds = directives()
        Selection.Field(alias, n, as, ds, if at("{") then selectionSet() else Nil, p)

    private def value(const: Boolean): Value =
      val t = peek
      t.kind match
        case Kind.IntT => advance(): Unit; Value.IntV(t.text.toLongOption.getOrElse(error(s"an integer out of range: ${t.text}")))
        case Kind.FloatT => advance(): Unit; Value.FloatV(t.text.toDouble)
        case Kind.StringT => advance(): Unit; Value.StringV(t.text)
        case Kind.Name =>
          advance(): Unit
          t.text match
            case "true" => Value.BooleanV(true)
            case "false" => Value.BooleanV(false)
            case "null" => Value.NullV
            case n => Value.EnumV(n)
        case Kind.Punct if t.text == "$" =>
          if const then error("a variable in a constant value")
          advance(): Unit
          Value.Variable(name())
        case Kind.Punct if t.text == "[" => nested {
          advance(): Unit
          val vs = ArrayBuffer.empty[Value]
          while !at("]") do vs += value(const)
          expect("]")
          Value.ListV(vs.toList)
        }
        case Kind.Punct if t.text == "{" => nested {
          advance(): Unit
          val fs = ArrayBuffer.empty[(String, Value)]
          while !at("}") do
            val n = name()
            expect(":")
            fs += n -> value(const)
          expect("}")
          Value.ObjectV(fs.toList)
        }
        case _ => error(s"expected a value, found '${t.text}'")
