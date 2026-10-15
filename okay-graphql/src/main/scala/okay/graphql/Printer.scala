package okay.graphql

import okay.codec.Json
import okay.codec.Json.JStr

/** A DOCUMENT AS TEXT: what any parser reads back to the same document (positions aside) */
object Printer:
  def document(d: Document): String = d.definitions.map(definition).mkString("\n\n")

  private def definition(d: Definition): String = d match
    case o: Definition.Operation =>
      val kind = o.kind match
        case OperationKind.Query => "query"
        case OperationKind.Mutation => "mutation"
        case OperationKind.Subscription => "subscription"
      val vars = if o.variables.isEmpty then "" else o.variables.map { v =>
        s"$$${v.name}: ${v.tpe}" + v.default.fold("")(d => s" = ${value(d)}")
      }.mkString("(", ", ", ")")
      s"$kind${o.name.fold("")(" " + _)}$vars${directives(o.directives)} ${selections(o.selections, 0)}"
    case f: Definition.Fragment =>
      s"fragment ${f.name} on ${f.on}${directives(f.directives)} ${selections(f.selections, 0)}"

  private def directives(ds: List[Directive]): String =
    ds.map(d => s" @${d.name}${arguments(d.arguments)}").mkString

  private def arguments(as: List[(String, Value)]): String =
    if as.isEmpty then "" else as.map((n, v) => s"$n: ${value(v)}").mkString("(", ", ", ")")

  /** a selection set, `depth` its indentation. Bounded: the document's nesting (a parsed one: `Limits.depth`) */
  private def selections(ss: List[Selection], depth: Int): String =
    val in = "  " * (depth + 1)
    ss.map {
      case f: Selection.Field =>
        val sub = if f.selections.isEmpty then "" else " " + selections(f.selections, depth + 1)
        s"$in${f.alias.fold("")(_ + ": ")}${f.name}${arguments(f.arguments)}${directives(f.directives)}$sub"
      case s: Selection.Spread => s"$in...${s.name}${directives(s.directives)}"
      case i: Selection.Inline =>
        s"$in...${i.on.fold("")(" on " + _)}${directives(i.directives)} ${selections(i.selections, depth + 1)}"
    }.mkString("{\n", "\n", s"\n${"  " * depth}}")

  /** a value as a literal. Bounded: the value's nesting (a parsed one: `Limits.depth`) */
  def value(v: Value): String = v match
    case Value.IntV(n) => n.toString
    case Value.FloatV(d) => d.toString
    case Value.StringV(s) => Json.print(JStr(s))
    case Value.BooleanV(b) => b.toString
    case Value.NullV => "null"
    case Value.EnumV(n) => n
    case Value.ListV(vs) => vs.map(value).mkString("[", ", ", "]")
    case Value.ObjectV(fs) => fs.map((k, x) => s"$k: ${value(x)}").mkString("{", ", ", "}")
    case Value.Variable(n) => s"$$$n"
