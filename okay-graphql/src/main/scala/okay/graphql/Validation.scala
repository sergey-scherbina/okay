package okay.graphql

import okay.codec.Json
import okay.codec.Json.*
import scala.collection.mutable

/** AN ERROR OF A RESPONSE: its message, where in the document (`locations`), where in the result (`path`), and what
 * a client may read besides (`extensions`: a `code` above all) */
final case class GraphQLError(message: String, locations: List[Pos] = Nil, path: List[Json] = Nil,
    extensions: Vector[(String, Json)] = Vector.empty):
  def toJson: Json =
    val locs = locations.filter(_ != Pos.None)
    JObj(Vector("message" -> JStr(message)) ++
      (if locs.isEmpty then Vector.empty
       else Vector("locations" -> JArr(locs.map(p => JObj(Vector("line" -> JNum(p.line.toDouble),
         "column" -> JNum(p.column.toDouble)))).toVector))) ++
      (if path.isEmpty then Vector.empty else Vector("path" -> JArr(path.toVector))) ++
      (if extensions.isEmpty then Vector.empty else Vector("extensions" -> JObj(extensions))))

  /** the same error with the code `code`, unless it has one */
  def coded(code: String): GraphQLError =
    if extensions.exists(_._1 == "code") then this else copy(extensions = ("code" -> JStr(code)) +: extensions)

object GraphQLError:
  val ParseFailed = "GRAPHQL_PARSE_FAILED"
  val ValidationFailed = "GRAPHQL_VALIDATION_FAILED"
  val BadUserInput = "BAD_USER_INPUT"
  val OperationResolutionFailure = "OPERATION_RESOLUTION_FAILURE"

/** A RESOLVER'S OWN FAILURE: its message and its extensions, as the response says them —
 * `FieldError("no such order", "code" -> JStr("NOT_FOUND"))`, thrown or raised (`FieldError.raise`) */
final class FieldError(message: String, val extensions: Vector[(String, Json)])
    extends Exception(message, null, false, false)

object FieldError:
  def apply(message: String, extensions: (String, Json)*): FieldError = new FieldError(message, extensions.toVector)
  /** a program of `Fetch` failing with it */
  def raise[A](message: String, extensions: (String, Json)*): okay.one.![A, Fetches] =
    okay.one.Cont.defer(throw FieldError(message, extensions*))

/** THE DOCUMENT CHECKED AGAINST THE SCHEMA BEFORE IT RUNS (GraphQL §5): every error said, with its locations. Walks
 * are work lists or bounded by the document's nesting (`Limits.depth`, the parser's) */
final class Validation(schema: Schema, doc: Document, limits: Limits):
  private val index = schema.index
  private val errors = mutable.ArrayBuffer.empty[GraphQLError]
  private val fragments = doc.fragments
  private def error(m: String, at: Pos*): Unit = errors += GraphQLError(m, at.toList)

  def run(): List[GraphQLError] =
    operations()
    fragmentDefinitions()
    doc.operations.foreach(operation)
    errors.distinct.toList

  // ── operations ──
  private def operations(): Unit =
    val ops = doc.operations
    if ops.isEmpty then error("the document has no operation")
    ops.groupBy(_.name).foreach { (n, os) =>
      n.foreach(name => if os.size > 1 then error(s"there can be only one operation named $name", os.map(_.at)*))
    }
    if ops.size > 1 then ops.filter(_.name.isEmpty).foreach(o => error("an anonymous operation must be alone", o.at))

  private def rootOf(o: Definition.Operation): Option[ObjectType[Unit]] = o.kind match
    case OperationKind.Query => Some(schema.executableQuery)
    case OperationKind.Mutation => schema.mutation
    case OperationKind.Subscription => schema.subscription.map(_.asObject)

  private def operation(o: Definition.Operation): Unit = rootOf(o) match
    case None => error(s"the schema has no ${o.kind.toString.toLowerCase}s", o.at)
    case Some(root) =>
      directives(o.directives, "an operation")
      selectionSet(root, o.selections, 0)
      if o.kind == OperationKind.Subscription then
        val roots = fieldsOf(root, o.selections)
        if roots.map(_._2.key).distinct.size != 1 then
          error(s"a subscription${o.name.fold("")(" " + _)} must select exactly one top-level field", o.at)
      variables(o)
      count(o)

  // ── fragments ──
  private def fragmentDefinitions(): Unit =
    doc.definitions.collect { case f: Definition.Fragment => f }.groupBy(_.name).foreach { (n, fs) =>
      if fs.size > 1 then error(s"there can be only one fragment named $n", fs.map(_.at)*)
    }
    val used = mutable.Set.empty[String]
    def spreads(ss: List[Selection]): List[Selection.Spread] =
      val out = mutable.ArrayBuffer.empty[Selection.Spread]
      val work = mutable.Stack.from(ss)
      while work.nonEmpty do work.pop() match
        case f: Selection.Field => work.pushAll(f.selections)
        case i: Selection.Inline => work.pushAll(i.selections)
        case s: Selection.Spread => out += s
      out.toList
    doc.operations.foreach(o => reached(spreads(o.selections).map(_.name)).foreach(used += _))
    fragments.values.foreach { f =>
      if !used(f.name) then error(s"fragment ${f.name} is never used", f.at)
      index.outputs.get(f.on) match
        case Some(c: Composite) =>
          directives(f.directives, "a fragment definition")
          selectionSet(c, f.selections, 0)
        case Some(_) => error(s"fragment ${f.name} cannot be on ${f.on}, a leaf type", f.at)
        case None => error(s"fragment ${f.name} is on an unknown type ${f.on}", f.at)
    }
    // cycles: a depth-first walk, a work list with the path
    val spreadsOf = fragments.view.mapValues(f => spreads(f.selections).map(_.name).distinct).toMap
    val done = mutable.Set.empty[String]
    for start <- fragments.keys.toList.sorted if !done(start) do
      val onPath = mutable.LinkedHashSet(start)
      val work = mutable.Stack((start, spreadsOf.getOrElse(start, Nil)))
      while work.nonEmpty do
        val (at, next) = work.pop()
        next match
          case Nil => onPath -= at; done += at
          case n :: rest =>
            work.push((at, rest))
            if onPath(n) then error(s"fragment $n spreads itself: ${(onPath.toList.dropWhile(_ != n) :+ n).mkString(" -> ")}",
              fragments(n).at)
            else if !done(n) && fragments.contains(n) then
              onPath += n
              work.push((n, spreadsOf.getOrElse(n, Nil)))

  /** the fragments reached from `names`, transitively: a work list, each once */
  private def reached(names: List[String]): Set[String] =
    val seen = mutable.Set.empty[String]
    val work = mutable.Stack.from(names)
    while work.nonEmpty do
      val n = work.pop()
      if seen.add(n) then fragments.get(n).foreach { f =>
        val inner = mutable.Stack.from(f.selections)
        while inner.nonEmpty do inner.pop() match
          case x: Selection.Field => inner.pushAll(x.selections)
          case i: Selection.Inline => inner.pushAll(i.selections)
          case s: Selection.Spread => work.push(s.name)
      }
    seen.toSet

  // ── selections ──
  /** a selection set in the scope of `on`: its fields, its fragments' applicability, its directives; the fields of
   * one response key the same field with the same arguments. Bounded: one level per level of the document */
  private def selectionSet(on: Composite, ss: List[Selection], depth: Int): Unit =
    if depth > limits.depth then error(s"selections nested deeper than ${limits.depth}")
    else
      ss.foreach {
        case f: Selection.Field =>
          directives(f.directives, "a field")
          if f.name == "__typename" then
            if f.selections.nonEmpty then error("__typename has no fields to select", f.at)
          else fieldDef(on, f.name) match
            case None => error(s"${on.name} has no field ${f.name}", f.at)
            case Some(d) =>
              arguments(s"${on.name}.${f.name}", d.args, f.arguments, f.at)
              index.outputs.get(Executor.named(d.tpe).name) match
                case Some(c: Composite) =>
                  if f.selections.isEmpty then error(s"${on.name}.${f.name} of type ${d.tpe.ref} needs a selection", f.at)
                  else selectionSet(c, f.selections, depth + 1)
                case _ => if f.selections.nonEmpty then
                  error(s"${on.name}.${f.name} of type ${d.tpe.ref} has no fields to select", f.at)
        case s: Selection.Spread =>
          directives(s.directives, "a fragment spread")
          fragments.get(s.name) match
            case None => error(s"unknown fragment ${s.name}", s.at)
            case Some(fr) => if index.outputs.contains(fr.on) && !overlap(on.name, fr.on) then
              error(s"fragment ${s.name} on ${fr.on} can never apply to ${on.name}", s.at)
        case i: Selection.Inline =>
          directives(i.directives, "an inline fragment")
          i.on match
            case None => selectionSet(on, i.selections, depth + 1)
            case Some(c) => index.outputs.get(c) match
              case Some(t: Composite) =>
                if !overlap(on.name, c) then error(s"an inline fragment on $c can never apply to ${on.name}", i.at)
                else selectionSet(t, i.selections, depth + 1)
              case Some(_) => error(s"an inline fragment cannot be on $c, a leaf type", i.at)
              case None => error(s"an inline fragment on an unknown type $c", i.at)
      }
      conflicts(fieldsOf(on, ss))

  private def fieldDef(on: Composite, name: String): Option[Field[?, ?]] = on.fieldDefs.find(_.name == name)

  private def overlap(a: String, b: String): Boolean =
    a == b || (index.possible(a) intersect index.possible(b)).nonEmpty

  /** the fields of a selection set by response key, inline fragments and spreads expanded (each fragment once):
   * with the type each is selected on */
  private def fieldsOf(on: Composite, ss: List[Selection]): List[(Composite, Selection.Field)] =
    val out = mutable.ArrayBuffer.empty[(Composite, Selection.Field)]
    val seen = mutable.Set.empty[String]
    val work = mutable.Stack.from(ss.map(on -> _))
    while work.nonEmpty do
      work.pop() match
        case (t, f: Selection.Field) => out += t -> f
        case (t, i: Selection.Inline) =>
          val scope = i.on.flatMap(index.outputs.get).collect { case c: Composite => c }.getOrElse(t)
          work.pushAll(i.selections.map(scope -> _))
        case (_, s: Selection.Spread) => if seen.add(s.name) then fragments.get(s.name).foreach { fr =>
          index.outputs.get(fr.on).collect { case c: Composite => c }.foreach(c => work.pushAll(fr.selections.map(c -> _)))
        }
    out.toList

  /** one response key, one field: the same name and arguments, unless the two are selected on different objects;
   * and the selections under one key merged, the same again (§5.3.2). Bounded: one level per level of the document */
  private def conflicts(fs: List[(Composite, Selection.Field)], depth: Int = 0): Unit =
    if depth <= limits.depth then fs.groupBy(_._2.key).foreach { (key, group) =>
      for case List((t1, f1), (t2, f2)) <- group.combinations(2) do
        val apart = t1.name != t2.name && isObject(t1) && isObject(t2)
        if !apart && (f1.name != f2.name || args(f1) != args(f2)) then
          error(s"fields $key conflict: ${show(f1)} and ${show(f2)}", f1.at, f2.at)
      val subs = group.filter(_._2.selections.nonEmpty)
      if subs.size > 1 then
        val (t, f) = subs.head
        fieldDef(t, f.name).flatMap(d => index.outputs.get(Executor.named(d.tpe).name)).foreach {
          case c: Composite => conflicts(fieldsOf(c, subs.flatMap(_._2.selections)), depth + 1)
          case _ => ()
        }
    }

  private def show(f: Selection.Field): String =
    if f.arguments.isEmpty then f.name else f.arguments.map((k, v) => s"$k: ${Printer.value(v)}").mkString(s"${f.name}(", ", ", ")")

  private def isObject(c: Composite) = c match
    case _: ObjectType[?] => true
    case _ => false

  private def args(f: Selection.Field) = f.arguments.sortBy(_._1).map((k, v) => k -> Printer.value(v))

  // ── arguments and values ──
  private def arguments(where: String, declared: List[Arg[?]], provided: List[(String, Value)], at: Pos): Unit =
    provided.groupBy(_._1).foreach((n, as) => if as.size > 1 then error(s"$where: argument $n given twice", at))
    provided.foreach { (n, v) =>
      declared.find(_.name == n) match
        case None => error(s"$where has no argument $n", at)
        case Some(a) => value(s"$where($n:)", v, a.tpe, a.default.isDefined, at)
    }
    declared.foreach { a =>
      if !a.tpe.nullable && a.default.isEmpty && !provided.exists(_._1 == a.name) then
        error(s"$where: argument ${a.name} of type ${a.tpe.ref} is required", at)
    }

  /** a value where the input type `t` is expected: a variable of a compatible type, or a literal of `t`. Bounded:
   * the value's nesting (the parser's `Limits.depth`) */
  private def value(where: String, v: Value, t: InType[?], hasDefault: Boolean, at: Pos): Unit = (v, t) match
    case (Value.Variable(n), _) => usages += ((n, t, hasDefault, at))
    case (Value.NullV, t) => if !t.nullable then error(s"$where: null for the non-null ${t.ref}", at)
    case (_, Optional(of)) => value(where, v, of, hasDefault, at)
    case (Value.ListV(vs), InList(of)) => vs.foreach(value(where, _, of, false, at))
    case (_, InList(of)) => value(where, v, of, false, at)
    case (Value.ObjectV(fs), o: InObject[?]) =>
      fs.foreach { (k, x) =>
        o.fields.find(_.name == k) match
          case None => error(s"$where: ${o.name} has no field $k", at)
          case Some(a) => value(s"$where.$k", x, a.tpe, a.default.isDefined, at)
      }
      o.fields.foreach { a =>
        if !a.tpe.nullable && a.default.isEmpty && !fs.exists(_._1 == a.name) then
          error(s"$where: ${o.name}.${a.name} of type ${a.tpe.ref} is required", at)
      }
    case (literal, t) => Executor.literalJson(literal) match
      case None => ()
      case Some(j) => t.decode(j).left.foreach(e => error(s"$where: $e", at))

  // ── variables ──
  private val usages = mutable.ArrayBuffer.empty[(String, InType[?], Boolean, Pos)]

  private def variables(o: Definition.Operation): Unit =
    o.variables.groupBy(_.name).foreach((n, vs) => if vs.size > 1 then error(s"there can be only one variable $$$n", vs.map(_.at)*))
    o.variables.foreach { v =>
      val base = Validation.base(v.tpe)
      if index.input(base).isEmpty then error(s"variable $$${v.name} of ${v.tpe}: $base is not an input type", v.at)
      v.default.foreach(d => if Validation.hasVariable(d) then error(s"variable $$${v.name}: a default cannot hold a variable", v.at))
    }
    // the usages of this operation: its own selections and the fragments it reaches, collected afresh
    val own = Validation(schema, Document(List(o) ++ reached(spreadsIn(o.selections)).toList.flatMap(fragments.get)), limits)
    own.collectUsages()
    val defined = o.variables.map(v => v.name -> v).toMap
    own.usages.foreach { (n, t, locationDefault, at) =>
      defined.get(n) match
        case None => error(s"variable $$$n is not defined${o.name.fold("")(" by operation " + _)}", at)
        case Some(d) =>
          val l = Validation.refOf(t)
          val allowed = Validation.compatible(d.tpe, l) || ((d.default.exists(_ != Value.NullV) || locationDefault) &&
            Validation.compatible(d.tpe, Validation.nullable(l)))
          if !allowed then error(s"variable $$$n of type ${d.tpe} where ${t.ref} is expected", at, d.at)
    }
    val used = own.usages.map(_._1).toSet
    o.variables.foreach(v => if !used(v.name) then error(s"variable $$${v.name} is never used", v.at))

  private def spreadsIn(ss: List[Selection]): List[String] =
    val out = mutable.ArrayBuffer.empty[String]
    val work = mutable.Stack.from(ss)
    while work.nonEmpty do work.pop() match
      case f: Selection.Field => work.pushAll(f.selections)
      case i: Selection.Inline => work.pushAll(i.selections)
      case s: Selection.Spread => out += s.name
    out.toList

  /** the variables' usages of this (one operation's) document, its errors dropped: the walks fill `usages` */
  private def collectUsages(): Unit =
    doc.operations.foreach(o => rootOf(o).foreach(r => selectionSet(r, o.selections, 0)))
    fragments.values.foreach(f => index.outputs.get(f.on).collect { case c: Composite => c }.foreach(selectionSet(_, f.selections, 0)))

  // ── directives ──
  private def directives(ds: List[Directive], where: String): Unit =
    ds.groupBy(_.name).foreach((n, xs) => if xs.size > 1 then error(s"@$n twice on $where", xs.map(_.at)*))
    ds.foreach { d =>
      if d.name != "skip" && d.name != "include" then error(s"unknown directive @${d.name}", d.at)
      else if where == "an operation" || where == "a fragment definition" then error(s"@${d.name} cannot be on $where", d.at)
      else arguments(s"@${d.name}", List(Arg("if", InScalar.boolean)), d.arguments, d.at)
    }

  // ── cost ──
  /** the fields the operation selects, fragments expanded, counted up to `limits.fields`: a work list that stops
   * past the limit, so a fragment bomb is refused without being expanded */
  private def count(o: Definition.Operation): Unit =
    if errors.isEmpty then
      var n = 0L
      val work = mutable.Stack.from(o.selections.map(_ -> 0))
      while work.nonEmpty && n <= limits.fields do
        work.pop() match
          case (f: Selection.Field, d) => n += 1; if d < limits.depth then work.pushAll(f.selections.map(_ -> (d + 1)))
          case (i: Selection.Inline, d) => if d < limits.depth then work.pushAll(i.selections.map(_ -> (d + 1)))
          case (s: Selection.Spread, d) =>
            if d < limits.depth then fragments.get(s.name).foreach(fr => work.pushAll(fr.selections.map(_ -> (d + 1))))
      if n > limits.fields then
        error(s"the operation selects more than ${limits.fields} fields (Limits.fields)", o.at)

object Validation:
  /** the named type at the bottom of a type reference */
  @scala.annotation.tailrec def base(t: TypeRef): String = t match
    case TypeRef.Named(n) => n
    case TypeRef.ListOf(of) => base(of)
    case TypeRef.NonNull(of) => base(of)

  /** an input type as a document writes it. Bounded: its lists and nulls, as the schema's code writes them */
  def refOf(t: InType[?]): TypeRef = t match
    case Optional(of) => nullable(refOf(of))
    case InList(of) => TypeRef.NonNull(TypeRef.ListOf(refOf(of)))
    case n: InNamed => TypeRef.NonNull(TypeRef.Named(n.name))

  def nullable(t: TypeRef): TypeRef = t match
    case TypeRef.NonNull(of) => of
    case other => other

  /** a variable of type `v` where `l` is expected (§5.8.5 AreTypesCompatible). Bounded: the two types' nesting */
  def compatible(v: TypeRef, l: TypeRef): Boolean = (l, v) match
    case (TypeRef.NonNull(l1), TypeRef.NonNull(v1)) => compatible(v1, l1)
    case (TypeRef.NonNull(_), _) => false
    case (_, TypeRef.NonNull(v1)) => compatible(v1, l)
    case (TypeRef.ListOf(l1), TypeRef.ListOf(v1)) => compatible(v1, l1)
    case (TypeRef.Named(a), TypeRef.Named(b)) => a == b
    case _ => false

  /** whether a value holds a variable. Bounded: the value's nesting */
  def hasVariable(v: Value): Boolean = v match
    case Value.Variable(_) => true
    case Value.ListV(vs) => vs.exists(hasVariable)
    case Value.ObjectV(fs) => fs.exists((_, x) => hasVariable(x))
    case _ => false
