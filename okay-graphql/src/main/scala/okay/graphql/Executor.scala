package okay.graphql

import okay.codec.Json
import okay.codec.Json.*
import okay.one.*
import okay.one.row.*
import okay.one.step.Step
import okay.one.step.Step.step
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.control.NonFatal

/** THE EXECUTOR: a document against a schema, to the GraphQL response — `data`, and `errors` with their paths.
 *
 * It runs the query as TASKS, each a resolver's program stepped by the kernel (`step`), in ROUNDS: every runnable
 * task runs to its value or to its next `Fetch`; a value completes its field — an object's fields, a list's items,
 * new tasks of this same round; when none is runnable, the fetches waiting are grouped by source and each source is
 * called ONCE with all its keys; every waiting task resumes with its value, runnable in the next round. Fields of an
 * object, items of a list, the parts of `Fetch.all`/`both`: independent, the applicative product — their fetches
 * meet in a round. A mutation's top-level fields run one after another, each to its end */
object Executor:
  /** `query` parsed and executed */
  def execute(schema: Schema, query: String, variables: Json = JObj(Vector.empty),
      operationName: Option[String] = None, limits: Limits = Limits())(using parser: GraphQLParser): Json =
    parser.parse(query) match
      case Left(e) => requestError(List(s"Syntax error: ${e.message} at ${e.line}:${e.column}"))
      case Right(doc) => execute(schema, doc, variables, operationName, limits)

  /** `document` executed: its operation chosen by `operationName`, or the only one */
  def execute(schema: Schema, document: Document, variables: Json, operationName: Option[String],
      limits: Limits): Json =
    val chosen = operationName match
      case Some(n) => document.operations.find(_.name.contains(n)).toRight(s"no operation named $n")
      case None => document.operations match
        case List(o) => Right(o)
        case Nil => Left("no operation in the document")
        case _ => Left("an operationName is required: the document has several operations")
    chosen.flatMap { op =>
      val root = op.kind match
        case OperationKind.Query => Right(schema.query)
        case OperationKind.Mutation => schema.mutation.toRight("the schema has no mutations")
      for
        r <- root
        vars <- coerceVariables(op.variables, variables)
        _ <- Validate(document.fragments, vars, limits).operation(r, op.selections)
      yield (op, r, vars)
    } match
      case Left(e) => requestError(List(e))
      case Right((op, root, vars)) =>
        Run(document.fragments, vars, limits).operation(root, op.selections, serial = op.kind == OperationKind.Mutation)

  private def requestError(messages: List[String]): Json =
    JObj(Vector("errors" -> JArr(messages.map(m => JObj(Vector("message" -> JStr(m)))).toVector)))

  /** the request's variables, as the operation declares them: a default for one absent, a non-null one required */
  private def coerceVariables(defs: List[VariableDefinition], request: Json): Either[String, Map[String, Json]] =
    val provided = request match
      case JObj(fs) => fs.toMap
      case _ => Map.empty[String, Json]
    defs.foldLeft[Either[String, Map[String, Json]]](Right(Map.empty)) { (acc, d) =>
      acc.flatMap { m =>
        (provided.get(d.name), d.default, d.tpe) match
          case (Some(JNull), _, TypeRef.NonNull(_)) => Left(s"variable $$${d.name} of a non-null type is null")
          case (Some(j), _, _) => Right(m + (d.name -> j))
          case (None, Some(v), _) => literal(v, Map.empty).map(j => m + (d.name -> j))
          case (None, None, TypeRef.NonNull(_)) => Left(s"variable $$${d.name} of a non-null type is required")
          case (None, None, _) => Right(m)
      }
    }

  /** a document's value as JSON, its variables read: an absent variable is `None` — the argument absent. Bounded:
   * a value nests at most `Limits.depth` (the parser's) */
  private[graphql] def value(v: Value, vars: Map[String, Json]): Option[Json] = v match
    case Value.Variable(n) => vars.get(n)
    case other => literal(other, vars).toOption

  private def literal(v: Value, vars: Map[String, Json]): Either[String, Json] = v match
    case Value.IntV(n) => Right(JNum(n.toDouble))
    case Value.FloatV(d) => Right(JNum(d))
    case Value.StringV(s) => Right(JStr(s))
    case Value.BooleanV(b) => Right(JBool(b))
    case Value.NullV => Right(JNull)
    case Value.EnumV(n) => Right(JStr(n))
    case Value.Variable(n) => vars.get(n).toRight(s"variable $$$n is not defined")
    case Value.ListV(vs) => Right(JArr(vs.flatMap(value(_, vars)).toVector))
    case Value.ObjectV(fs) => Right(JObj(fs.flatMap((k, x) => value(x, vars).map(k -> _)).toVector))

  /** the arguments a selection gives, as JSON: the absent left out */
  private[graphql] def argumentsOf(f: Selection.Field, vars: Map[String, Json]): Map[String, Json] =
    f.arguments.flatMap((k, v) => value(v, vars).map(k -> _)).toMap

  /** `@skip(if:)` / `@include(if:)`: whether the selection is in */
  private[graphql] def included(ds: List[Directive], vars: Map[String, Json]): Either[String, Boolean] =
    ds.foldLeft[Either[String, Boolean]](Right(true)) { (acc, d) =>
      acc.flatMap { in =>
        if d.name != "skip" && d.name != "include" then Left(s"unknown directive @${d.name}")
        else d.arguments.collectFirst { case ("if", v) => value(v, vars) } match
          case Some(Some(JBool(b))) => Right(in && (if d.name == "skip" then !b else b))
          case _ => Left(s"@${d.name} needs a Boolean argument 'if'")
      }
    }

  /** the named type a field's type is of, through lists and nulls */
  @tailrec private[graphql] def named(t: OutType[?]): Named = t match
    case Nullable(of) => named(of)
    case ListOf(of) => named(of)
    case Fetched(of) => named(of)
    case n: Named => n

  /** a selection set's fields on an object type, fragments expanded, `@skip`/`@include` applied, merged by response
   * key in order. Bounded: a fragment inside a fragment is one level deeper, refused past `limits.depth` (a cycle of
   * fragments is that) */
  private[graphql] def collect(on: String, sels: List[Selection], fragments: Map[String, Definition.Fragment],
      vars: Map[String, Json], limits: Limits): Either[String, mutable.LinkedHashMap[String, List[Selection.Field]]] =
    val out = mutable.LinkedHashMap.empty[String, List[Selection.Field]]
    def go(sels: List[Selection], depth: Int): Option[String] =
      if depth > limits.depth then Some(s"fragments nested deeper than ${limits.depth}")
      else sels.iterator.map {
        case f: Selection.Field => included(f.directives, vars) match
          case Left(e) => Some(e)
          case Right(false) => None
          case Right(true) => out(f.key) = out.getOrElse(f.key, Nil) :+ f; None
        case Selection.Spread(n, ds) => included(ds, vars) match
          case Left(e) => Some(e)
          case Right(false) => None
          case Right(true) => fragments.get(n) match
            case None => Some(s"unknown fragment $n")
            case Some(fr) if fr.on != on => Some(s"fragment $n on ${fr.on} cannot apply to $on")
            case Some(fr) => included(fr.directives, vars).fold(Some(_), in => if in then go(fr.selections, depth + 1) else None)
        case Selection.Inline(cond, ds, ss) => included(ds, vars) match
          case Left(e) => Some(e)
          case Right(false) => None
          case Right(true) => cond match
            case Some(c) if c != on => Some(s"an inline fragment on $c cannot apply to $on")
            case _ => go(ss, depth + 1)
      }.collectFirst { case Some(e) => e }
    go(sels, 0).toLeft(out)

  /** THE CHECK BEFORE RUNNING: every field on its type, its arguments declared, leaves without selections and objects
   * with them. A work list, not a recursion; every error said */
  private final class Validate(fragments: Map[String, Definition.Fragment], vars: Map[String, Json], limits: Limits):
    def operation(root: ObjectType[Unit], sels: List[Selection]): Either[String, Unit] =
      val errors = mutable.ArrayBuffer.empty[String]
      val work = mutable.Queue((root: ObjectType[?], sels, 1))
      while work.nonEmpty do
        val (o, ss, depth) = work.dequeue()
        if depth > limits.depth then errors += s"selections nested deeper than ${limits.depth}"
        else collect(o.name, ss, fragments, vars, limits) match
          case Left(e) => errors += e
          case Right(fields) =>
            for (_, fs) <- fields; f <- fs if f.name != "__typename" do
              o.field.get(f.name) match
                case None => errors += s"${o.name} has no field ${f.name}"
                case Some(d) =>
                  f.arguments.map(_._1).filterNot(n => d.args.exists(_.name == n))
                    .foreach(n => errors += s"${o.name}.${f.name} has no argument $n")
                  named(d.tpe) match
                    case t: ObjectType[?] =>
                      if f.selections.isEmpty then errors += s"${o.name}.${f.name} of type ${t.name} needs a selection"
                      else work += ((t, f.selections, depth + 1))
                    case t => if f.selections.nonEmpty then
                      errors += s"${o.name}.${f.name} of type ${t.name} has no fields to select"
      if errors.isEmpty then Right(()) else Left(errors.distinct.mkString("; "))

  /** A PLACE IN THE RESPONSE: filled by its field's task; `nullable` from the field's type */
  private final class Slot(val path: List[Json], val nullable: Boolean):
    var value: Res = Res.Pending

  private enum Res:
    case Pending, Null, Error
    case Leaf(j: Json)
    case Obj(fields: Vector[(String, Slot)])
    case Arr(items: Vector[Slot])

  /** a source's identity, for the round's map: a source's own `equals` is not asked */
  private final class Ident(val ref: AnyRef):
    override def hashCode: Int = System.identityHashCode(ref)
    override def equals(o: Any): Boolean = o match
      case i: Ident => i.ref eq ref
      case _ => false

  /** THE ROUND'S KEYS OF ONE SOURCE, with the tasks waiting on each */
  private final class Batch[K, V](source: Source[K, V]):
    private val waiting = mutable.ArrayBuffer.empty[(K, V => Unit, Throwable => Unit)]
    def add(key: K, ok: V => Unit, ko: Throwable => Unit): Unit = waiting += ((key, ok, ko))
    /** the source called once, its keys each once */
    def flush(): Unit =
      val answered =
        try Right(source.fetch(waiting.iterator.map(_._1).distinct.toSeq))
        catch case NonFatal(e) => Left(e)
      answered match
        case Left(e) => waiting.foreach(_._3(e))
        case Right(m) => waiting.foreach { (k, ok, ko) =>
          m.get(k) match
            case Some(v) => ok(v)
            case None => ko(java.util.NoSuchElementException(s"${source.name} has no value for $k"))
        }

  /** ONE EXECUTION: the run queue, the round's batches, the errors */
  private final class Run(fragments: Map[String, Definition.Fragment], vars: Map[String, Json], limits: Limits):
    private val runnable = mutable.Queue.empty[() => Unit]
    private var batches = mutable.LinkedHashMap.empty[Ident, Batch[?, ?]]
    private val errors = mutable.ArrayBuffer.empty[(String, List[Json])]

    def operation(root: ObjectType[Unit], sels: List[Selection], serial: Boolean): Json =
      val data = Slot(Nil, nullable = true)
      collect(root.name, sels, fragments, vars, limits) match
        case Left(e) => errors += ((e, Nil))
        case Right(fields) =>
          val slots = slotsOf(root, fields, Nil)
          data.value = Res.Obj(slots)
          fields.iterator.zip(slots).foreach { case ((_, fs), (_, slot)) =>
            startField(root, (), fs, slot)
            if serial then rounds()
          }
          rounds()
      val out = Vector("data" -> render(data).getOrElse(JNull))
      JObj(if errors.isEmpty then out else out :+ ("errors" -> JArr(errors.toVector.map { (m, p) =>
        JObj(Vector("message" -> JStr(m)) ++ (if p.isEmpty then Vector.empty else Vector("path" -> JArr(p.reverse.toVector))))
      })))

    /** rounds until no task is left: run what is runnable, then call each source of the round once */
    private def rounds(): Unit =
      var more = true
      while more do
        while runnable.nonEmpty do runnable.dequeue()()
        if batches.isEmpty then more = false
        else
          val round = batches
          batches = mutable.LinkedHashMap.empty
          round.valuesIterator.foreach(_.flush())

    /** the slots of an object's fields: nullable as their types are (`__typename` is not) */
    private def slotsOf(o: ObjectType[?], fields: mutable.LinkedHashMap[String, List[Selection.Field]],
        path: List[Json]): Vector[(String, Slot)] =
      fields.toVector.map((key, fs) => key -> Slot(JStr(key) :: path, o.field.get(fs.head.name).exists(_.tpe.nullable)))

    private def fieldError(slot: Slot, e: Throwable): Unit =
      slot.value = Res.Error
      errors += ((Option(e.getMessage).getOrElse(e.getClass.getName), slot.path))

    /** a field of `parent` (its selections merged under one key) resolved into `slot` */
    private def startField[A](o: ObjectType[A], parent: A, fs: List[Selection.Field], slot: Slot): Unit =
      val f = fs.head
      if f.name == "__typename" then slot.value = Res.Leaf(JStr(o.name))
      else o.field(f.name) match
        case d: Field[A, b] => Args.of(d.args, argumentsOf(f, vars)) match
          case Left(e) => fieldError(slot, IllegalArgumentException(e))
          case Right(args) =>
            val sub = fs.flatMap(_.selections)
            task[b](d.resolve(parent, args), b => complete(d.tpe, b, sub, slot), fieldError(slot, _))

    /** `b` of the type `t` written into `slot`: a leaf now; a list's items and an object's fields as tasks */
    @tailrec private def complete[B](t: OutType[B], b: B, sels: List[Selection], slot: Slot): Unit = t match
      case s: Scalar[b] =>
        try slot.value = Res.Leaf(s.encode(b)) catch case NonFatal(e) => fieldError(slot, e)
      case e: EnumType[b] => e.nameOf(b) match
        case Some(n) => slot.value = Res.Leaf(JStr(n))
        case None => fieldError(slot, IllegalArgumentException(s"${e.name} has no name for $b"))
      case n: Nullable[x] => b match
        case None => slot.value = Res.Null
        case Some(v) => complete(n.of, v, sels, slot)
      case f: Fetched[x] => task[x](b, x => again(f.of, x, sels, slot), fieldError(slot, _))
      case l: ListOf[x] =>
        val items = b.toVector
                val slots = items.indices.map(i => Slot(JNum(i.toDouble) :: slot.path, l.of.nullable)).toVector
        slot.value = Res.Arr(slots)
        items.iterator.zip(slots).foreach((v, s) => runnable += (() => again(l.of, v, sels, s)))
      case o: ObjectType[b] => collect(o.name, sels, fragments, vars, limits) match
        case Left(e) => fieldError(slot, IllegalArgumentException(e))
        case Right(fields) =>
          val slots = slotsOf(o, fields, slot.path)
          slot.value = Res.Obj(slots)
          fields.iterator.zip(slots).foreach { case ((_, fs), (_, s)) => startField(o, b, fs, s) }

    /** `complete` from a task: the loop's `@tailrec` kept */
    private def again[B](t: OutType[B], b: B, sels: List[Selection], slot: Slot): Unit = complete(t, b, sels, slot)

    /** a program run as a task: stepped now (in this round), `done` with its value, `failed` with its error */
    private def task[A](p: => A ! Fetches, done: A => Unit, failed: Throwable => Unit): Unit =
      runnable += (() => drive(try p.step catch case NonFatal(e) => Step.Failed(e), done, failed))

    /** a stepped program: its value or error delivered, or its fetch registered — the rest resumed later, as a task */
    private def drive[A](s: Step[Fetches, A], done: A => Unit, failed: Throwable => Unit): Unit = s match
      case Step.Done(a) => done(a)
      case Step.Failed(e) => failed(e)
      case Step.Cancelled() => failed(IllegalStateException("cancelled"))
      case y: Step.Yield[?, ?, ?, ?] => In.same(y.in, In.Here[Fetch, Pure]()) match
        case null => failed(IllegalStateException(s"an operation no handler takes: ${y.op}"))
        case same: In.Same[e, Fetch] =>
          def resumed(next: => Step[Fetches, A]): Unit = runnable += (() => drive(next, done, failed))
          same(y.op) match
            case g: Fetch.Get[?, ?] => batchOf(g.source).add(g.key, v => resumed(y.resume(v)), e => resumed(y.resume.fail(e)))
            case all: Fetch.All[b] => join(all.ps, vs => resumed(y.resume(vs)), e => resumed(y.resume.fail(e)))
            case both: Fetch.Both[a1, b1] =>
              var first: Option[a1] = None
              var second: Option[b1] = None
              var over = false
              def meet(): Unit = (first, second) match
                case (Some(a1), Some(b1)) => over = true; resumed(y.resume((a1, b1)))
                case _ => ()
              def fail(e: Throwable): Unit = if !over then { over = true; resumed(y.resume.fail(e)) }
              task[a1](both.a, a1 => if !over then { first = Some(a1); meet() }, fail)
              task[b1](both.b, b1 => if !over then { second = Some(b1); meet() }, fail)

    /** independent programs as tasks of their own: `done` with all their values in order, or `failed` with the
     * first error */
    private def join[A](ps: List[A ! Fetches], done: List[A] => Unit, failed: Throwable => Unit): Unit =
      if ps.isEmpty then runnable += (() => done(Nil))
      else
        val values = mutable.ArrayBuffer.fill[Option[A]](ps.size)(None)
        var left = ps.size
        var over = false
        ps.iterator.zipWithIndex.foreach { (p, i) =>
          task[A](p, a => if !over then
            values(i) = Some(a)
            left -= 1
            if left == 0 then { over = true; done(values.iterator.flatten.toList) }
          , e => if !over then { over = true; failed(e) })
        }

    /** the round's batch of `s`. THE ONE CAST: the map holds the batches of many sources, of as many key and value
     * types, keyed by identity — and a batch is put under its own source only, here, so the one under `s` is a
     * `Batch[K, V]` */
    private def batchOf[K, V](s: Source[K, V]): Batch[K, V] =
      batches.getOrElseUpdate(Ident(s), Batch[K, V](s)).asInstanceOf[Batch[K, V]]

    /** a slot as JSON, or `None`: a null where null may not be — given to the parent (the spec's null propagation).
     * Bounded: one level per level of the response, the document's nesting (`Limits.depth`) and its types' lists */
    private def render(s: Slot): Option[Json] =
      val v: Option[Json] = s.value match
        case Res.Leaf(j) => Some(j)
        case Res.Null | Res.Error | Res.Pending => None
        case Res.Obj(fs) =>
          val out = fs.map((k, c) => render(c).map(k -> _))
          if out.forall(_.isDefined) then Some(JObj(out.flatten)) else None
        case Res.Arr(items) =>
          val out = items.map(render)
          if out.forall(_.isDefined) then Some(JArr(out.flatten)) else None
      v.orElse(if s.nullable then Some(JNull) else None)
