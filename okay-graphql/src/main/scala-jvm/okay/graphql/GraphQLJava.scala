package okay.graphql

import graphql.language as gl
import scala.jdk.CollectionConverters.*

/** THE STANDARD ONE behind the facade (OURS OR THE STANDARD ONE): graphql-java's parser, its document read into
 * ours. `given GraphQLParser = GraphQLJava()` is the one line that changes; graphql-java is an OPTIONAL dependency
 * of okay-graphql, refused by name when absent. Each reads the other's output (`TestGraphQLJava`) */
final class GraphQLJava(limits: Limits = Limits()) extends GraphQLParser:
  GraphQLJava.ready

  def parse(source: String): Either[ParseError, Document] =
    try Right(GraphQLJava.Read(limits.depth).document(graphql.parser.Parser.parse(source)))
    catch
      case e: graphql.parser.InvalidSyntaxException =>
        val at = Option(e.getLocation).getOrElse(gl.SourceLocation(0, 0))
        Left(ParseError(Option(e.getMessage).getOrElse("invalid syntax"), at.getLine, at.getColumn))
      case e: OkayGraphQLParser.Failed => Left(e.error)
      case e: graphql.parser.exceptions.ParseCancelledException =>
        Left(ParseError(Option(e.getMessage).getOrElse("parse cancelled"), 0, 0))

object GraphQLJava:
  /** why graphql-java cannot be used here, or nothing */
  def missing(className: String = "graphql.parser.Parser"): Option[String] =
    try { Class.forName(className, false, getClass.getClassLoader); None }
    catch case _: ClassNotFoundException => Some(
      s"okay.graphql.GraphQLJava needs com.graphql-java:graphql-java, an optional dependency of okay-graphql " +
        s"($className is not on the classpath): add com.graphql-java:graphql-java:22.3 — or use " +
        "okay.graphql.GraphQLParser.default, ours, which needs nothing")

  private lazy val ready: Unit = missing().foreach(why => throw IllegalStateException(why))

  /** graphql-java's nodes into ours: every nesting counted against `max`, as our parser counts them */
  private final class Read(max: Int):
    private var depth = 0
    private def nested[A](at: gl.Node[?])(body: => A): A =
      depth += 1
      if depth > max then
        val p = pos(at)
        throw OkayGraphQLParser.Failed(ParseError(s"nesting deeper than $max", p.line, p.column))
      try body finally depth -= 1

    private def pos(n: gl.Node[?]): Pos = Option(n.getSourceLocation).fold(Pos.None)(l => Pos(l.getLine, l.getColumn))

    def document(d: gl.Document): Document = Document(d.getDefinitions.asScala.toList.map {
      case o: gl.OperationDefinition =>
        val kind = o.getOperation match
          case gl.OperationDefinition.Operation.MUTATION => OperationKind.Mutation
          case gl.OperationDefinition.Operation.SUBSCRIPTION => OperationKind.Subscription
          case _ => OperationKind.Query
        Definition.Operation(kind, Option(o.getName), o.getVariableDefinitions.asScala.toList.map(variable),
          directives(o.getDirectives), selections(o.getSelectionSet), pos(o))
      case f: gl.FragmentDefinition =>
        Definition.Fragment(f.getName, f.getTypeCondition.getName, directives(f.getDirectives),
          selections(f.getSelectionSet), pos(f))
      case other => throw OkayGraphQLParser.Failed(ParseError(s"not an executable definition: ${other.getClass.getSimpleName}",
        pos(other).line, pos(other).column))
    })

    private def variable(v: gl.VariableDefinition): VariableDefinition =
      VariableDefinition(v.getName, tpe(v.getType), Option(v.getDefaultValue).map(value), pos(v))

    /** a type. Bounded: `nested`, as our parser */
    private def tpe(t: gl.Type[?]): TypeRef = t match
      case n: gl.TypeName => TypeRef.Named(n.getName)
      case l: gl.ListType => nested(l)(TypeRef.ListOf(tpe(l.getType)))
      case n: gl.NonNullType => TypeRef.NonNull(tpe(n.getType))
      case other => throw IllegalArgumentException(s"a type graphql-java has no more of: $other")

    private def directives(ds: java.util.List[gl.Directive]): List[Directive] =
      ds.asScala.toList.map(d => Directive(d.getName, arguments(d.getArguments), pos(d)))

    private def arguments(as: java.util.List[gl.Argument]): List[(String, Value)] =
      as.asScala.toList.map(a => a.getName -> value(a.getValue))

    /** a selection set. Bounded: `nested`, as our parser */
    private def selections(s: gl.SelectionSet | Null): List[Selection] =
      if s == null then Nil
      else nested(s)(s.getSelections.asScala.toList.map {
        case f: gl.Field => Selection.Field(Option(f.getAlias), f.getName, arguments(f.getArguments),
          directives(f.getDirectives), selections(f.getSelectionSet), pos(f))
        case s: gl.FragmentSpread => Selection.Spread(s.getName, directives(s.getDirectives), pos(s))
        case i: gl.InlineFragment => Selection.Inline(Option(i.getTypeCondition).map(_.getName),
          directives(i.getDirectives), selections(i.getSelectionSet), pos(i))
        case other => throw IllegalArgumentException(s"a selection graphql-java has no more of: $other")
      })

    /** a value. Bounded: `nested`, as our parser */
    private def value(v: gl.Value[?]): Value = v match
      case i: gl.IntValue => Value.IntV(i.getValue.longValueExact)
      case f: gl.FloatValue => Value.FloatV(f.getValue.doubleValue)
      case s: gl.StringValue => Value.StringV(s.getValue)
      case b: gl.BooleanValue => Value.BooleanV(b.isValue)
      case _: gl.NullValue => Value.NullV
      case e: gl.EnumValue => Value.EnumV(e.getName)
      case r: gl.VariableReference => Value.Variable(r.getName)
      case a: gl.ArrayValue => nested(a)(Value.ListV(a.getValues.asScala.toList.map(value)))
      case o: gl.ObjectValue => nested(o)(Value.ObjectV(o.getObjectFields.asScala.toList.map(f => f.getName -> value(f.getValue))))
      case other => throw IllegalArgumentException(s"a value graphql-java has no more of: $other")
