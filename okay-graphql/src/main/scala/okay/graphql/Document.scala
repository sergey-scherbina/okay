package okay.graphql

/** AN EXECUTABLE DOCUMENT: what a client sends — operations and fragments, parsed (`GraphQLParser`) */
final case class Document(definitions: List[Definition]):
  def operations: List[Definition.Operation] = definitions.collect { case o: Definition.Operation => o }
  def fragments: Map[String, Definition.Fragment] =
    definitions.collect { case f: Definition.Fragment => f.name -> f }.toMap

enum OperationKind:
  case Query, Mutation

enum Definition:
  case Operation(kind: OperationKind, name: Option[String], variables: List[VariableDefinition],
      directives: List[Directive], selections: List[Selection])
  case Fragment(name: String, on: String, directives: List[Directive], selections: List[Selection])

final case class VariableDefinition(name: String, tpe: TypeRef, default: Option[Value])

/** a type as a document writes it: `Int`, `[Int]`, `Int!` */
enum TypeRef:
  case Named(name: String)
  case ListOf(of: TypeRef)
  case NonNull(of: TypeRef)

final case class Directive(name: String, arguments: List[(String, Value)])

enum Selection:
  case Field(alias: Option[String], name: String, arguments: List[(String, Value)], directives: List[Directive],
      selections: List[Selection])
  case Spread(name: String, directives: List[Directive])
  case Inline(on: Option[String], directives: List[Directive], selections: List[Selection])

object Selection:
  extension (f: Selection.Field) def key: String = f.alias.getOrElse(f.name)

/** a value in a document: a literal, or a variable to be read from the request's */
enum Value:
  case IntV(n: Long)
  case FloatV(d: Double)
  case StringV(s: String)
  case BooleanV(b: Boolean)
  case NullV
  case EnumV(name: String)
  case ListV(vs: List[Value])
  case ObjectV(fs: List[(String, Value)])
  case Variable(name: String)
