package okay.graphql

/** AN EXECUTABLE DOCUMENT: what a client sends — operations and fragments, parsed (`GraphQLParser`) */
final case class Document(definitions: List[Definition]):
  def operations: List[Definition.Operation] = definitions.collect { case o: Definition.Operation => o }
  def fragments: Map[String, Definition.Fragment] =
    definitions.collect { case f: Definition.Fragment => f.name -> f }.toMap
  /** the document as text, parsed back to itself (`Printer`) */
  def print: String = Printer.document(this)

/** where in the document: line and column from 1; `Pos.None` for a node made by code */
final case class Pos(line: Int, column: Int)

object Pos:
  val None: Pos = Pos(0, 0)

enum OperationKind:
  case Query, Mutation, Subscription

enum Definition:
  case Operation(kind: OperationKind, name: Option[String], variables: List[VariableDefinition],
      directives: List[Directive], selections: List[Selection], at: Pos = Pos.None)
  case Fragment(name: String, on: String, directives: List[Directive], selections: List[Selection],
      at: Pos = Pos.None)

final case class VariableDefinition(name: String, tpe: TypeRef, default: Option[Value], at: Pos = Pos.None)

/** a type as a document writes it: `Int`, `[Int]`, `Int!` */
enum TypeRef:
  case Named(name: String)
  case ListOf(of: TypeRef)
  case NonNull(of: TypeRef)

  override def toString: String = this match
    case Named(n) => n
    case ListOf(of) => s"[$of]"
    case NonNull(of) => s"$of!"

final case class Directive(name: String, arguments: List[(String, Value)], at: Pos = Pos.None)

enum Selection:
  case Field(alias: Option[String], name: String, arguments: List[(String, Value)], directives: List[Directive],
      selections: List[Selection], at: Pos = Pos.None)
  case Spread(name: String, directives: List[Directive], at: Pos = Pos.None)
  case Inline(on: Option[String], directives: List[Directive], selections: List[Selection], at: Pos = Pos.None)

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
