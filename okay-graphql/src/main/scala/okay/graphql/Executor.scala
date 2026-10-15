package okay.graphql

import okay.{Async, AsyncCont, StreamCont}
import okay.codec.Json
import okay.codec.Json.*
import okay.one.*
import okay.one.Cont.pure
import okay.one.row.*
import okay.one.step.Step
import okay.one.step.Step.step
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.control.NonFatal

/** THE EXECUTOR: a document against a schema, to the GraphQL response — `data`, and `errors` with their locations
 * and paths.
 *
 * It runs the query as TASKS, each a resolver's program stepped by the kernel (`step`), in ROUNDS: every runnable
 * task runs to its value or to its next `Fetch`; a value completes its field — an object's fields, a list's items,
 * new tasks of this same round; when none is runnable, the fetches waiting are grouped by source and each source is
 * called ONCE with all its keys; every waiting task resumes with its value, runnable in the next round. Fields of an
 * object, items of a list, the parts of `Fetch.all`/`both`: independent, the applicative product — their fetches
 * meet in a round. A mutation's top-level fields run one after another, each to its end */
object Executor:
  /** `query` parsed, checked and executed */
  def execute(schema: Schema, query: String, variables: Json = JObj(Vector.empty),
      operationName: Option[String] = None, limits: Limits = Limits())(using parser: GraphQLParser): Json =
    prepare(schema, query, operationName, limits).fold(identity, _.execute(variables))

  /** `document` checked and executed: its operation chosen by `operationName`, or the only one */
  def execute(schema: Schema, document: Document, variables: Json, operationName: Option[String],
      limits: Limits): Json =
    prepare(schema, document, operationName, limits).fold(identity, _.execute(variables))

  /** `query` parsed, checked and executed, its async sources called — every one of a round at once */
  def executeAsync(schema: Schema, query: String, variables: Json = JObj(Vector.empty),
      operationName: Option[String] = None, limits: Limits = Limits())(using parser: GraphQLParser)
      : Json ! (Async +: Pure) =
    prepare(schema, query, operationName, limits).fold(pure, _.executeAsync(variables))

  /** `query` parsed and checked: a `Prepared` to execute with any variables, or the response refusing it */
  def prepare(schema: Schema, query: String, operationName: Option[String] = None, limits: Limits = Limits())
      (using parser: GraphQLParser): Either[Json, Prepared] =
    parser.parse(query) match
      case Left(e) => Left(errorsJson(List(GraphQLError(s"Syntax error: ${e.message}", List(Pos(e.line, e.column)))
        .coded(GraphQLError.ParseFailed))))
      case Right(doc) => prepare(schema, doc, operationName, limits)

  def prepare(schema: Schema, document: Document, operationName: Option[String], limits: Limits)
      : Either[Json, Prepared] =
    val problems = Validation(schema, document, limits).run()
    if problems.nonEmpty then Left(errorsJson(problems.map(_.coded(GraphQLError.ValidationFailed))))
    else
      val chosen = operationName match
        case Some(n) => document.operations.find(_.name.contains(n)).toRight(s"no operation named $n")
        case None => document.operations match
          case List(o) => Right(o)
          case _ => Left("an operationName is required: the document has several operations")
      chosen.left.map(m => errorsJson(List(GraphQLError(m).coded(GraphQLError.OperationResolutionFailure))))
        .map(Prepared(schema, document, _, limits))

  /** a request's subscription: the stream of its responses, or the response refusing it */
  def subscribe(schema: Schema, query: String, variables: Json = JObj(Vector.empty),
      operationName: Option[String] = None, limits: Limits = Limits())(using parser: GraphQLParser)
      : Either[Json, StreamCont.Src[Json]] =
    prepare(schema, query, operationName, limits).flatMap(_.subscribe(variables))

  private[graphql] def errorsJson(es: List[GraphQLError]): Json =
    JObj(Vector("errors" -> JArr(es.map(_.toJson).toVector)))

  /** A DOCUMENT PARSED AND CHECKED, its operation chosen: executed with any variables, as often as asked */
  final class Prepared private[Executor] (val schema: Schema, val document: Document,
      val operation: Definition.Operation, val limits: Limits):
    private val fragments = document.fragments

    def execute(variables: Json = JObj(Vector.empty)): Json = coerce(variables) match
      case Left(es) => errorsJson(es)
      case Right(vars) => operation.kind match
        case OperationKind.Subscription =>
          errorsJson(List(GraphQLError("a subscription is executed by subscribe", List(operation.at))))
        case kind =>
          val run = Run(schema, fragments, vars, limits)
          run.operation(root(kind), operation.selections, serial = kind == OperationKind.Mutation)
            .foreach { start => run.forget(); start(); run.sync() }
          run.sync()
          run.response()

    def executeAsync(variables: Json = JObj(Vector.empty)): Json ! (Async +: Pure) = coerce(variables) match
      case Left(es) => pure(errorsJson(es))
      case Right(vars) => operation.kind match
        case OperationKind.Subscription =>
          pure(errorsJson(List(GraphQLError("a subscription is executed by subscribe", List(operation.at)))))
        case kind =>
          val run = Run(schema, fragments, vars, limits)
          val fields = run.operation(root(kind), operation.selections, serial = kind == OperationKind.Mutation)
          fields.foldLeft[Unit ! (Async +: Pure)](run.asyncRounds()) { (before, start) =>
            before.flatMap(_ => { run.forget(); start(); run.asyncRounds() })
          }.map(_ => run.response())

    /** each value of the subscription's stream executed as its selection set: a response each */
    def subscribe(variables: Json = JObj(Vector.empty)): Either[Json, StreamCont.Src[Json]] =
      coerce(variables).left.map(errorsJson).flatMap { vars =>
        (operation.kind, schema.subscription) match
          case (OperationKind.Subscription, Some(sub)) =>
            val probe = Run(schema, fragments, vars, limits)
            probe.collect(sub.asObject, operation.selections) match
              case Left(e) => Left(errorsJson(List(e)))
              case Right(fields) =>
                val (key, fs) = fields.head
                val f = fs.head
                sub.field(f.name) match
                  case s: Subscription[b] => Args.of(s.args, argumentsOf(f, vars)) match
                    case Left(e) => Left(errorsJson(List(GraphQLError(e, List(f.at), List(JStr(key))))))
                    case Right(args) =>
                      val events =
                        try Right(s.source(args)) catch case NonFatal(e) => Left(errorsJson(List(GraphQLError(message(e), List(f.at), List(JStr(key))))))
                      events.map(_.evalMap { (b: b) =>
                        val run = Run(schema, fragments, vars, limits)
                        run.event(key, s.tpe, b, fs.flatMap(_.selections), f.at)
                        run.asyncRounds().map(_ => run.response())
                      })
          case _ => Left(errorsJson(List(GraphQLError("not a subscription", List(operation.at)))))
      }

    private def root(kind: OperationKind): ObjectType[Unit] = kind match
      case OperationKind.Mutation => schema.mutation.getOrElse(schema.executableQuery)
      case _ => schema.executableQuery

    /** the request's variables, as the operation declares them: each coerced to its type, a default for one absent,
     * a non-null one required */
    private def coerce(request: Json): Either[List[GraphQLError], Map[String, Json]] =
      val provided = request match
        case JObj(fs) => fs.toMap
        case _ => Map.empty[String, Json]
      val out = mutable.LinkedHashMap.empty[String, Json]
      val errs = mutable.ArrayBuffer.empty[GraphQLError]
      operation.variables.foreach { d =>
        (provided.get(d.name), d.default) match
          case (Some(j), _) => typed(d.tpe, j).fold(e => errs += GraphQLError(s"variable $$${d.name}: $e", List(d.at)),
            v => out(d.name) = v)
          case (None, Some(v)) => literalJson(v).foreach(j => out(d.name) = j)
          case (None, None) => d.tpe match
            case TypeRef.NonNull(_) => errs += GraphQLError(s"variable $$${d.name} of type ${d.tpe} is required", List(d.at))
            case _ => ()
      }
      if errs.isEmpty then Right(out.toMap) else Left(errs.toList.map(_.coded(GraphQLError.BadUserInput)))

    /** `j` read as the type `t`: refused, or the value — a single value where a list is, a list of one. Bounded: the
     * type's nesting, as the document writes it (`Limits.depth`) */
    private def typed(t: TypeRef, j: Json): Either[String, Json] = (t, j) match
      case (TypeRef.NonNull(_), JNull) => Left(s"null for $t")
      case (TypeRef.NonNull(of), _) => typed(of, j)
      case (_, JNull) => Right(JNull)
      case (TypeRef.ListOf(of), JArr(vs)) =>
        vs.iterator.map(typed(of, _)).foldLeft[Either[String, Vector[Json]]](Right(Vector.empty)) { (acc, x) =>
          acc.flatMap(a => x.map(a :+ _))
        }.map(JArr(_))
      case (TypeRef.ListOf(of), one) => typed(of, one).map(v => JArr(Vector(v)))
      case (TypeRef.Named(n), _) => schema.index.input(n) match
        case Some(it: InType[?]) => it.decode(j).map(_ => j)
        case _ => Left(s"$n is not an input type")
      case _ => Left(s"${Json.print(j)} is not a $t")

  /** a literal (no variable) as JSON. Bounded: the value's nesting (the parser's `Limits.depth`) */
  private[graphql] def literalJson(v: Value): Option[Json] = v match
    case Value.IntV(n) => Some(JNum(n.toDouble))
    case Value.FloatV(d) => Some(JNum(d))
    case Value.StringV(s) => Some(JStr(s))
    case Value.BooleanV(b) => Some(JBool(b))
    case Value.NullV => Some(JNull)
    case Value.EnumV(n) => Some(JStr(n))
    case Value.Variable(_) => None
    case Value.ListV(vs) => Some(JArr(vs.flatMap(literalJson).toVector))
    case Value.ObjectV(fs) => Some(JObj(fs.flatMap((k, x) => literalJson(x).map(k -> _)).toVector))

  /** a document's value as JSON, its variables read: an absent variable is `None` — the argument absent. Bounded:
   * a value nests at most `Limits.depth` (the parser's) */
  private[graphql] def value(v: Value, vars: Map[String, Json]): Option[Json] = v match
    case Value.Variable(n) => vars.get(n)
    case Value.ListV(vs) => Some(JArr(vs.map(x => value(x, vars).getOrElse(JNull)).toVector))
    case Value.ObjectV(fs) => Some(JObj(fs.flatMap((k, x) => value(x, vars).map(k -> _)).toVector))
    case other => literalJson(other)

  /** the arguments a selection gives, as JSON: the absent left out */
  private[graphql] def argumentsOf(f: Selection.Field, vars: Map[String, Json]): Map[String, Json] =
    f.arguments.flatMap((k, v) => value(v, vars).map(k -> _)).toMap

  /** `@skip(if:)` / `@include(if:)`: whether the selection is in (validation checked them) */
  private[graphql] def included(ds: List[Directive], vars: Map[String, Json]): Boolean =
    ds.forall { d =>
      d.arguments.collectFirst { case ("if", v) => value(v, vars) } match
        case Some(Some(JBool(b))) => if d.name == "skip" then !b else b
        case _ => true
    }

  /** the named type a field's type is of, through lists, nulls and what is still to be fetched */
  @tailrec private[graphql] def named(t: OutType[?]): Named = t match
    case Nullable(of) => named(of)
    case ListOf(of) => named(of)
    case Fetched(of) => named(of)
    case n: Named => n

  private def message(e: Throwable): String = Option(e.getMessage).getOrElse(e.getClass.getName)

  /** A PLACE IN THE RESPONSE: filled by its field's task; `nullable` from the field's type */
  private final class Slot(val path: List[Json], val nullable: Boolean, val at: Pos):
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

  /** ONE SOURCE IN ONE EXECUTION: the keys of this round with the tasks waiting on each, and what every earlier
   * round answered — the execution's cache (a source that opts out, none) */
  private final class Batch[K, V](val source: DataSource[K, V]):
    private val waiting = mutable.ArrayBuffer.empty[(K, V => Unit, Throwable => Unit)]
    private val cache = mutable.HashMap.empty[K, Either[Throwable, V]]

    /** a key asked: answered now from the cache, or waiting for the round's call */
    def add(key: K, ok: V => Unit, ko: Throwable => Unit): Unit = cache.get(key) match
      case Some(Right(v)) => ok(v)
      case Some(Left(e)) => ko(e)
      case None => waiting += ((key, ok, ko))

    def isWaiting: Boolean = waiting.nonEmpty
    /** the cache forgotten: a mutation's next field sees fresh data */
    def forget(): Unit = cache.clear()
    private def keys: Seq[K] = waiting.iterator.map(_._1).distinct.toSeq

    /** the source called, at once: a plain source; an async one outside `executeAsync` is an error of its fields */
    def call(): Either[Throwable, Map[K, V]] = source match
      case s: Source[K, V] => try Right(s.fetch(keys)) catch case NonFatal(e) => Left(e)
      case a: AsyncSource[K, V] =>
        Left(IllegalStateException(s"${a.name} is an AsyncSource: run the query by executeAsync"))

    /** the source called as a program: an async one's own, a plain one's at once */
    def callAsync(): Either[Throwable, Map[K, V]] ! (Async +: Pure) = source match
      case a: AsyncSource[K, V] =>
        Cont.defer(a.fetch(keys)).map[Either[Throwable, Map[K, V]]](Right(_)).recover(e => pure(Left(e)))
      case _: Source[K, V] => pure(call())

    /** the answer given to the tasks waiting — their value, or the error — and kept for later rounds */
    def deliver(answer: Either[Throwable, Map[K, V]]): Unit =
      val now = waiting.toList
      waiting.clear()
      now.foreach { (k, ok, ko) =>
        val one: Either[Throwable, V] = answer.flatMap(m =>
          m.get(k).toRight(java.util.NoSuchElementException(s"${source.name} has no value for $k")))
        if source.cached then cache(k) = one
        one.fold(ko, ok)
      }

    /** called and delivered now */
    def flush(): Unit = deliver(call())

    /** called as a program, its answer kept for `deliver` on the executor's thread */
    def answerAsync(): Unit ! (Async +: Pure) = callAsync().map(a => answered = a)
    var answered: Either[Throwable, Map[K, V]] = Left(IllegalStateException("not called"))
    def deliverAnswered(): Unit = deliver(answered)

  /** ONE EXECUTION: the run queue, its sources' batches and caches, the errors */
  private final class Run(schema: Schema, fragments: Map[String, Definition.Fragment], vars: Map[String, Json],
      limits: Limits):
    private val runnable = mutable.Queue.empty[() => Unit]
    private val batches = mutable.LinkedHashMap.empty[Ident, Batch[?, ?]]
    private val errors = mutable.ArrayBuffer.empty[GraphQLError]
    private val data = Slot(Nil, nullable = true, Pos.None)

    /** the operation's top-level fields: started now — or, a mutation's, given back to start one by one, each run
     * to its end before the next */
    def operation(root: ObjectType[Unit], sels: List[Selection], serial: Boolean): List[() => Unit] =
      collect(root, sels) match
        case Left(e) => errors += e; Nil
        case Right(fields) =>
          val slots = slotsOf(root, fields, Nil)
          data.value = Res.Obj(slots)
          val starts = fields.iterator.zip(slots).map { case ((_, fs), (_, slot)) => () => startField(root, (), fs, slot) }.toList
          if serial then starts else { starts.foreach(_()); Nil }

    /** a subscription's event: the value `b` of the root field `key`, its selection completed */
    def event[B](key: String, t: OutType[B], b: B, sels: List[Selection], at: Pos): Unit =
      val slot = Slot(List(JStr(key)), t.nullable, at)
      data.value = Res.Obj(Vector(key -> slot))
      complete(t, b, sels, slot)

    def response(): Json =
      val out = Vector("data" -> render(data).getOrElse(JNull))
      JObj(if errors.isEmpty then out else out :+ ("errors" -> JArr(errors.toVector.map(_.toJson))))

    /** what can run now, run: the sources this round asks — none, the end */
    private def round(): List[Batch[?, ?]] =
      while runnable.nonEmpty do runnable.dequeue()()
      batches.valuesIterator.filter(_.isWaiting).toList

    /** every source's cache forgotten: between a mutation's top-level fields */
    def forget(): Unit = batches.valuesIterator.foreach(_.forget())

    /** rounds until no task is left, each source called at once */
    def sync(): Unit =
      var bs = round()
      while bs.nonEmpty do
        bs.foreach(_.flush())
        bs = round()

    /** rounds until no task is left, as a program: each round's sources called ALL AT ONCE — every one started,
     * the round going on when the last answered; their answers delivered here, on one thread */
    def asyncRounds(): Unit ! (Async +: Pure) =
      round() match
        case Nil => pure(())
        case bs =>
          AsyncCont.await[Unit] { k =>
            val left = AtomicInteger(bs.size)
            bs.foreach(b => AsyncCont.runAsync(b.answerAsync()).onComplete { _ =>
              if left.decrementAndGet() == 0 then k(())
            }(using scala.concurrent.ExecutionContext.parasitic))
          }.at[Async +: Pure].flatMap { _ =>
            bs.foreach(_.deliverAnswered())
            asyncRounds()
          }

    /** a selection set's fields on the object `on`, fragments that apply expanded, `@skip`/`@include` applied,
     * merged by response key in order. Bounded: a fragment inside a fragment is one level deeper, refused past
     * `limits.depth` (validation refused cycles) */
    def collect(on: ObjectType[?], sels: List[Selection])
        : Either[GraphQLError, mutable.LinkedHashMap[String, List[Selection.Field]]] =
      val out = mutable.LinkedHashMap.empty[String, List[Selection.Field]]
      def go(sels: List[Selection], depth: Int): Option[GraphQLError] =
        if depth > limits.depth then Some(GraphQLError(s"fragments nested deeper than ${limits.depth}"))
        else sels.iterator.map {
          case f: Selection.Field =>
            if included(f.directives, vars) then out(f.key) = out.getOrElse(f.key, Nil) :+ f
            None
          case s: Selection.Spread =>
            if !included(s.directives, vars) then None
            else fragments.get(s.name) match
              case None => Some(GraphQLError(s"unknown fragment ${s.name}", List(s.at)))
              case Some(fr) => if schema.index.applies(fr.on, on.name) then go(fr.selections, depth + 1) else None
          case i: Selection.Inline =>
            if !included(i.directives, vars) then None
            else if i.on.forall(schema.index.applies(_, on.name)) then go(i.selections, depth + 1) else None
        }.collectFirst { case Some(e) => e }
      go(sels, 0).toLeft(out)

    /** the slots of an object's fields: nullable as their types are (`__typename` is not) */
    private def slotsOf(o: ObjectType[?], fields: mutable.LinkedHashMap[String, List[Selection.Field]],
        path: List[Json]): Vector[(String, Slot)] =
      fields.toVector.map((key, fs) =>
        key -> Slot(JStr(key) :: path, o.field.get(fs.head.name).exists(_.tpe.nullable), fs.head.at))

    private def fieldError(slot: Slot, e: Throwable): Unit =
      slot.value = Res.Error
      val extensions = e match
        case f: FieldError => f.extensions
        case _ => Vector.empty
      errors += GraphQLError(message(e), List(slot.at), slot.path.reverse, extensions)

    /** a field of `parent` (its selections merged under one key) resolved into `slot` */
    private def startField[A](o: ObjectType[A], parent: A, fs: List[Selection.Field], slot: Slot): Unit =
      val f = fs.head
      if f.name == "__typename" then slot.value = Res.Leaf(JStr(o.name))
      else o.field.get(f.name) match
        case None => fieldError(slot, IllegalArgumentException(s"${o.name} has no field ${f.name}"))
        case Some(d: Field[A, b]) => Args.of(d.args, argumentsOf(f, vars)) match
          case Left(e) => fieldError(slot, FieldError(e, "code" -> JStr(GraphQLError.BadUserInput)))
          case Right(args) =>
            val sub = fs.flatMap(_.selections)
            task[b](d.resolve(parent, args), b => complete(d.tpe, b, sub, slot), fieldError(slot, _))

    /** `complete` from a task: the loop's `@tailrec` kept */
    private def again[B](t: OutType[B], b: B, sels: List[Selection], slot: Slot): Unit = complete(t, b, sels, slot)

    /** `b` of the type `t` written into `slot`: a leaf now; a list's items, an object's fields and what is still to
     * be fetched as tasks; an interface's or a union's value as the member that picks it */
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
        val slots = items.indices.map(i => Slot(JNum(i.toDouble) :: slot.path, l.of.nullable, slot.at)).toVector
        slot.value = Res.Arr(slots)
        items.iterator.zip(slots).foreach((v, s) => runnable += (() => again(l.of, v, sels, s)))
      case o: ObjectType[b] => collect(o, sels) match
        case Left(e) => fieldError(slot, IllegalArgumentException(e.message))
        case Right(fields) =>
          val slots = slotsOf(o, fields, slot.path)
          slot.value = Res.Obj(slots)
          fields.iterator.zip(slots).foreach { case ((_, fs), (_, s)) => startField(o, b, fs, s) }
      case i: InterfaceType[b] => picked(i.members, b) match
        case Some(r: Resolved[c]) => complete(r.tpe, r.value, sels, slot)
        case None => fieldError(slot, IllegalArgumentException(s"no member of ${i.name} is the value $b"))
      case u: UnionType[b] => picked(u.members, b) match
        case Some(r: Resolved[c]) => complete(r.tpe, r.value, sels, slot)
        case None => fieldError(slot, IllegalArgumentException(s"no member of ${u.name} is the value $b"))

    /** the first member picking `a`, with its value as the member's */
    private def picked[A](ms: List[Member[A, ?]], a: A): Option[Resolved[?]] =
      ms.iterator.map { case m: Member[A, c] => m.pick(a).map(Resolved(m.tpe, _)) }.collectFirst { case Some(r) => r }

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

    /** the execution's batch of `s`. THE ONE CAST: the map holds the batches of many sources, of as many key and
     * value types, keyed by identity — and a batch is put under its own source only, here, so the one under `s` is
     * a `Batch[K, V]` */
    private def batchOf[K, V](s: DataSource[K, V]): Batch[K, V] =
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

  /** a value of an abstract type as its member's */
  private final case class Resolved[B](tpe: ObjectType[B], value: B)

/** A CACHE OF PREPARED DOCUMENTS by their text and operation name: parsed and checked once, the least recently used
 * dropped past `capacity` */
final class QueryCache(schema: Schema, capacity: Int = 256, limits: Limits = Limits())(using parser: GraphQLParser):
  private val cache = new java.util.LinkedHashMap[(String, Option[String]), Either[Json, Executor.Prepared]](16, 0.75f, true):
    override def removeEldestEntry(e: java.util.Map.Entry[(String, Option[String]), Either[Json, Executor.Prepared]]): Boolean =
      this.size() > capacity

  def prepare(query: String, operationName: Option[String] = None): Either[Json, Executor.Prepared] =
    cache.synchronized {
      val key = (query, operationName)
      val hit = cache.get(key)
      if hit != null then hit
      else
        val p = Executor.prepare(schema, query, operationName, limits)
        cache.put(key, p): Unit
        p
    }

  def execute(query: String, variables: Json = JObj(Vector.empty), operationName: Option[String] = None): Json =
    prepare(query, operationName).fold(identity, _.execute(variables))

  def size: Int = cache.synchronized(cache.size())
