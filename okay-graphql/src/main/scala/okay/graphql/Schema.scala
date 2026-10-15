package okay.graphql

import okay.codec.Json
import okay.codec.Json.*
import okay.one.*
import okay.one.Cont.pure
import okay.StreamCont
import scala.collection.mutable

/** AN OUTPUT TYPE for values of the Scala type `A`: non-null unless `Nullable` — a Scala value is there */
sealed trait OutType[A]:
  def nullable: Boolean = OutType.nullable(this)
  /** the type as SDL writes it: `[User!]!` */
  def ref: String = this match
    case Nullable(of) => of.ref.stripSuffix("!")
    case ListOf(of) => s"[${of.ref}]!"
    case Fetched(of) => of.ref
    case n: Named => s"${n.name}!"

object OutType:
  /** whether a value of `t` may be null: through what is still to be fetched */
  @scala.annotation.tailrec def nullable(t: OutType[?]): Boolean = t match
    case Nullable(_) => true
    case Fetched(of) => nullable(of)
    case _ => false

/** a type with a name: a scalar, an enum, an object, an interface, a union */
sealed trait Named:
  def name: String
  def description: Option[String]

/** a type with fields to select: an object, an interface, a union (`__typename` only, and fragments) */
sealed trait Composite extends Named:
  def fieldDefs: List[Field[?, ?]]

/** a scalar: its value as JSON */
final case class Scalar[A](name: String, encode: A => Json, description: Option[String] = None)
    extends OutType[A] with Named

object Scalar:
  val int: Scalar[Int] = Scalar("Int", i => JNum(i.toDouble))
  val float: Scalar[Double] = Scalar("Float", JNum(_))
  val string: Scalar[String] = Scalar("String", JStr(_))
  val boolean: Scalar[Boolean] = Scalar("Boolean", JBool(_))
  val id: Scalar[String] = Scalar("ID", JStr(_))
  val builtin: Set[String] = Set("Int", "Float", "String", "Boolean", "ID")

/** an enum: each value of `A` by its name; a value deprecated by its name, with the reason */
final case class EnumType[A](name: String, values: List[(String, A)], description: Option[String] = None,
    deprecated: Map[String, String] = Map.empty) extends OutType[A] with Named:
  def nameOf(a: A): Option[String] = values.collectFirst { case (n, v) if v == a => n }

object EnumType:
  /** the enum `E` of singleton cases: each value by its case's name */
  inline def derive[E]: EnumType[E] = EnumType[E](Derive.name[E], Derive.values[E])

/** a list: its items each of `of` */
final case class ListOf[A](of: OutType[A]) extends OutType[Seq[A]]

/** may be null: `None` */
final case class Nullable[A](of: OutType[A]) extends OutType[Option[A]]

/** A VALUE STILL TO BE FETCHED: a program, run as a task of its own where it stands — so its error is ITS position's
 * (a list's item: `["byIds", 1]`), not the field's, and its fetches meet its siblings' in the same round. The SDL
 * does not see it: `Fetched(of)` is `of` to a client */
final case class Fetched[A](of: OutType[A]) extends OutType[A ! Fetches]

/** AN OBJECT TYPE: fields of a parent `A`, each resolved by a program. Its fields are lazy, so types may refer to
 * each other (a user's orders, an order's user). The interfaces it implements are the schema's to know: those that
 * name it a member */
final class ObjectType[A](val name: String, val description: Option[String] = None)(fs: => List[Field[A, ?]])
    extends OutType[A] with Composite:
  lazy val fields: List[Field[A, ?]] = fs
  lazy val field: Map[String, Field[A, ?]] = fields.map(f => f.name -> f).toMap
  def fieldDefs: List[Field[?, ?]] = fields
  override def toString: String = s"ObjectType($name)"

object ObjectType:
  /** the object type of the case class `A`: a field per parameter, of the output type its Scala type has
   * (`Output`); `extra` — the fetched fields, by hand — added, each replacing a derived one of its name */
  inline def derive[A](inline extra: Field[A, ?]*): ObjectType[A] = ${ Derive.objectType[A]('extra) }

/** AN OBJECT TYPE A VALUE OF `A` MAY BE: `pick` says whether it is, and gives it as the object's — typed, no test of
 * a class */
final case class Member[A, B](tpe: ObjectType[B], pick: A => Option[B])

object Member:
  /** a member by a partial function: `Member(userT) { case u: User => u }` */
  def apply[A, B](tpe: ObjectType[B])(pick: PartialFunction[A, B]): Member[A, B] = Member(tpe, pick.lift)

/** AN INTERFACE: fields every member declares too (`Schema.check`), its members the object types a value may be */
final class InterfaceType[A](val name: String, val description: Option[String] = None)(fs: => List[Field[A, ?]],
    ms: => List[Member[A, ?]]) extends OutType[A] with Composite:
  lazy val fields: List[Field[A, ?]] = fs
  lazy val members: List[Member[A, ?]] = ms
  def fieldDefs: List[Field[?, ?]] = fields
  override def toString: String = s"InterfaceType($name)"

/** A UNION: one of its members, no field in common */
final class UnionType[A](val name: String, val description: Option[String] = None)(ms: => List[Member[A, ?]])
    extends OutType[A] with Composite:
  lazy val members: List[Member[A, ?]] = ms
  def fieldDefs: List[Field[?, ?]] = Nil
  override def toString: String = s"UnionType($name)"

/** A FIELD of a parent `A`, of the type `tpe`: its arguments, and the program that resolves it */
final class Field[A, B](val name: String, val tpe: OutType[B], val args: List[Arg[?]],
    val resolve: (A, Args) => B ! Fetches, val description: Option[String], val deprecated: Option[String] = None):
  def describe(d: String): Field[A, B] = new Field(name, tpe, args, resolve, Some(d), deprecated)
  def deprecate(reason: String): Field[A, B] = new Field(name, tpe, args, resolve, description, Some(reason))

object Field:
  /** a field read off its parent: no data to fetch */
  def apply[A, B](name: String, tpe: OutType[B])(get: A => B): Field[A, B] =
    new Field(name, tpe, Nil, (a, _) => pure(get(a)), None)
  /** a field resolved by a program of `Fetch`, of its parent and its arguments */
  def fetch[A, B](name: String, tpe: OutType[B], args: Arg[?]*)(resolve: (A, Args) => B ! Fetches): Field[A, B] =
    new Field(name, tpe, args.toList, resolve, None)

/** AN INPUT TYPE: a JSON value — an argument's, a variable's — read as `A`, or refused with a reason */
sealed trait InType[A]:
  def decode(j: Json): Either[String, A]
  /** the value as a GraphQL literal, for a default in the SDL */
  def literal(a: A): String
  /** what an absent value is: a nullable input's `None`, nothing for another */
  def absent: Option[A] = None
  def nullable: Boolean = this match
    case Optional(_) => true
    case _ => false
  def ref: String = this match
    case Optional(of) => of.ref.stripSuffix("!")
    case InList(of) => s"[${of.ref}]!"
    case n: InNamed => s"${n.name}!"

sealed trait InNamed:
  def name: String
  def description: Option[String]

final case class InScalar[A](name: String, read: Json => Either[String, A], show: A => String,
    description: Option[String] = None) extends InType[A] with InNamed:
  def decode(j: Json): Either[String, A] = read(j)
  def literal(a: A): String = show(a)

object InScalar:
  private def quote(s: String) = Json.print(JStr(s))
  val int: InScalar[Int] = InScalar("Int", {
    case JNum(n) if n.isWhole && n >= Int.MinValue && n <= Int.MaxValue => Right(n.toInt)
    case j => Left(s"Int cannot represent ${Json.print(j)}")
  }, _.toString)
  val float: InScalar[Double] = InScalar("Float", {
    case JNum(n) => Right(n)
    case j => Left(s"Float cannot represent ${Json.print(j)}")
  }, _.toString)
  val string: InScalar[String] = InScalar("String", {
    case JStr(s) => Right(s)
    case j => Left(s"String cannot represent ${Json.print(j)}")
  }, quote)
  val boolean: InScalar[Boolean] = InScalar("Boolean", {
    case JBool(b) => Right(b)
    case j => Left(s"Boolean cannot represent ${Json.print(j)}")
  }, _.toString)
  val id: InScalar[String] = InScalar("ID", {
    case JStr(s) => Right(s)
    case JNum(n) if n.isWhole => Right(n.toLong.toString)
    case j => Left(s"ID cannot represent ${Json.print(j)}")
  }, quote)

object InEnum:
  /** the enum `E` of singleton cases, as an input: each value by its case's name */
  inline def derive[E]: InEnum[E] = InEnum[E](Derive.name[E], Derive.values[E])

final case class InEnum[A](name: String, values: List[(String, A)], description: Option[String] = None)
    extends InType[A] with InNamed:
  def decode(j: Json): Either[String, A] = j match
    case JStr(s) => values.collectFirst { case (`s`, a) => a }.toRight(s"$name has no value $s")
    case other => Left(s"$name cannot represent ${Json.print(other)}")
  def literal(a: A): String = values.collectFirst { case (n, `a`) => n }.getOrElse(a.toString)

/** a list input: a single value is a list of one (the spec's input coercion) */
final case class InList[A](of: InType[A]) extends InType[List[A]]:
  def decode(j: Json): Either[String, List[A]] = j match
    case JNull => Left("a list cannot be null")
    case JArr(vs) =>
      val out = List.newBuilder[A]
      var refused: Option[String] = None
      val it = vs.iterator
      while refused.isEmpty && it.hasNext do
        of.decode(it.next()) match
          case Right(a) => out += a
          case Left(e) => refused = Some(e)
      refused.toLeft(out.result())
    case one => of.decode(one).map(List(_))
  def literal(as: List[A]): String = as.map(of.literal).mkString("[", ", ", "]")

final case class Optional[A](of: InType[A]) extends InType[Option[A]]:
  def decode(j: Json): Either[String, Option[A]] = j match
    case JNull => Right(None)
    case v => of.decode(v).map(Some(_))
  def literal(a: Option[A]): String = a.fold("null")(of.literal)
  override def absent: Option[Option[A]] = Some(None)

/** an input object: its fields read as arguments are, then built. Its fields are lazy: an input may hold itself */
final class InObject[A](val name: String, val description: Option[String] = None)(fs: => List[Arg[?]],
    build: Args => A, show: A => String = (a: A) => a.toString) extends InType[A] with InNamed:
  lazy val fields: List[Arg[?]] = fs
  def decode(j: Json): Either[String, A] = j match
    case JObj(fs) =>
      val unknown = fs.map(_._1).filterNot(n => fields.exists(_.name == n))
      if unknown.nonEmpty then Left(s"$name has no field ${unknown.mkString(", ")}")
      else Args.of(fields, fs.toMap).map(build)
    case other => Left(s"$name cannot represent ${Json.print(other)}")
  def literal(a: A): String = show(a)

object InObject:
  /** the input object of the case class `A`: an argument per parameter, of the input type its Scala type has
   * (`Input`), the value built by the class's mirror */
  inline def derive[A](using m: scala.deriving.Mirror.ProductOf[A]): InObject[A] = ${ Derive.inObject[A]('m) }

/** AN ARGUMENT: its name, its input type, a default */
final case class Arg[A](name: String, tpe: InType[A], default: Option[A] = None, description: Option[String] = None):
  private[graphql] def read(provided: Option[Json]): Either[String, A] = provided match
    case Some(j) => tpe.decode(j).left.map(e => s"argument $name: $e")
    case None => default.orElse(tpe.absent).toRight(s"argument $name of type ${tpe.ref} is required")

/** A FIELD'S ARGUMENTS, every one read already (`Args.of`): `args(arg)` its value */
final class Args private (values: Map[String, Json]):
  def apply[A](a: Arg[A]): A = a.read(values.get(a.name)) match
    case Right(v) => v
    case Left(e) => throw IllegalArgumentException(e)

object Args:
  val empty: Args = Args(Map.empty)
  /** the arguments `declared` read from `provided`: each refused or read, so `apply` cannot fail after */
  def of(declared: List[Arg[?]], provided: Map[String, Json]): Either[String, Args] =
    declared.iterator.map(a => a.read(provided.get(a.name))).collectFirst { case Left(e) => e } match
      case Some(e) => Left(e)
      case None => Right(Args(provided))

/** A SUBSCRIPTION FIELD: a stream of values of `tpe`, from its arguments — each value executed as the selection */
final class Subscription[B](val name: String, val tpe: OutType[B], val args: List[Arg[?]],
    val source: Args => StreamCont.Src[B], val description: Option[String] = None)

object Subscription:
  def apply[B](name: String, tpe: OutType[B], args: Arg[?]*)(source: Args => StreamCont.Src[B]): Subscription[B] =
    new Subscription(name, tpe, args.toList, source)

/** THE SUBSCRIPTION ROOT: its fields streams */
final class SubscriptionType(val name: String, val description: Option[String] = None)(fs: => List[Subscription[?]]):
  lazy val fields: List[Subscription[?]] = fs
  lazy val field: Map[String, Subscription[?]] = fields.map(f => f.name -> f).toMap
  /** the root as an object type, for validation, introspection and the SDL: its fields' types and arguments, no
   * resolver of its own — a subscription runs by `Executor.subscribe` */
  lazy val asObject: ObjectType[Unit] = ObjectType[Unit](name, description)(fields.map { case s: Subscription[b] =>
    new Field[Unit, b](s.name, s.tpe, s.args,
      (_, _) => okay.one.Cont.defer(throw IllegalStateException(s"${s.name} is a subscription: Executor.subscribe")),
      s.description)
  })

/** A SCHEMA: the root types, a query's, a mutation's, a subscription's */
final case class Schema(query: ObjectType[Unit], mutation: Option[ObjectType[Unit]] = None,
    subscription: Option[SubscriptionType] = None, description: Option[String] = None):
  /** the query root as executed: its fields and introspection's, `__schema` and `__type` */
  lazy val executableQuery: ObjectType[Unit] =
    ObjectType[Unit](query.name, query.description)(query.fields ++ Introspection.rootFields(this))

  /** every named type reached from the roots, introspection's own included */
  lazy val index: Schema.Index = Schema.Index(this)

  /** the schema's own errors: an object missing a field of an interface it is a member of, a field of another type
   * than the interface's, two types of one name */
  def check: List[String] = index.problems

  /** the schema in SDL: every type reached from the roots, in the order met; introspection's and the built-in
   * scalars are not printed */
  def sdl: String =
    def str(d: String) = Json.print(JStr(d))
    def doc(d: Option[String], indent: String) = d.fold("")(x => s"$indent${str(x)}\n")
    def dep(d: Option[String]) = d.fold("")(r => s" @deprecated(reason: ${str(r)})")
    def args(as: List[Arg[?]]): String =
      if as.isEmpty then ""
      else as.map { case a: Arg[t] =>
        s"${a.name}: ${a.tpe.ref}" + a.default.fold("")(d => s" = ${a.tpe.literal(d)}")
      }.mkString("(", ", ", ")")
    def fields(fs: List[Field[?, ?]]) = fs.map(f =>
      s"${doc(f.description, "  ")}  ${f.name}${args(f.args)}: ${f.tpe.ref}${dep(f.deprecated)}").mkString("\n")
    def enumValues(vs: List[String], deprecated: Map[String, String]) =
      vs.map(v => s"  $v${dep(deprecated.get(v))}").mkString("\n")
    val roots =
      val defaults = query.name == "Query" && mutation.forall(_.name == "Mutation") &&
        subscription.forall(_.name == "Subscription")
      if defaults then Nil
      else List((List("  query: " + query.name) ++ mutation.map("  mutation: " + _.name) ++
        subscription.map("  subscription: " + _.name)).mkString("schema {\n", "\n", "\n}"))
    val printed = mutable.LinkedHashSet.empty[String]
    val out = mutable.ArrayBuffer.empty[String]
    def emit(name: String, text: String): Unit = if printed.add(name) then out += text
    for n <- index.names if !n.startsWith("__") && !Scalar.builtin(n) do
      (index.outputs.get(n), index.inputs.get(n)) match
        case (Some(o: ObjectType[?]), _) =>
          val is = index.interfacesOf(o.name)
          val impl = if is.isEmpty then "" else is.map(_.name).mkString(" implements ", " & ", "")
          val fs = fields(o.fields.filterNot(_.name.startsWith("__")))
          emit(n, s"${doc(o.description, "")}type ${o.name}$impl {\n$fs\n}")
        case (Some(i: InterfaceType[?]), _) =>
          emit(n, s"${doc(i.description, "")}interface ${i.name} {\n${fields(i.fields)}\n}")
        case (Some(u: UnionType[?]), _) =>
          emit(n, s"${doc(u.description, "")}union ${u.name} = ${u.members.map(_.tpe.name).mkString(" | ")}")
        case (Some(e: EnumType[?]), _) =>
          emit(n, s"${doc(e.description, "")}enum ${e.name} {\n${enumValues(e.values.map(_._1), e.deprecated)}\n}")
        case (Some(s: Scalar[?]), _) => emit(n, s"${doc(s.description, "")}scalar ${s.name}")
        case (None, Some(e: InEnum[?])) =>
          emit(n, s"${doc(e.description, "")}enum ${e.name} {\n${enumValues(e.values.map(_._1), Map.empty)}\n}")
        case (None, Some(s: InScalar[?])) => emit(n, s"${doc(s.description, "")}scalar ${s.name}")
        case (None, Some(i: InObject[?])) =>
          val fs = i.fields.map { case a: Arg[t] =>
            s"${doc(a.description, "  ")}  ${a.name}: ${a.tpe.ref}" + a.default.fold("")(d => s" = ${a.tpe.literal(d)}")
          }
          emit(n, s"${doc(i.description, "")}input ${i.name} {\n${fs.mkString("\n")}\n}")
        case _ => ()
    (description.map(str).toList ++ roots ++ out).mkString("", "\n\n", "\n")

object Schema:
  /** THE TYPES OF A SCHEMA BY NAME, met from its roots (a work list): outputs and inputs apart — an enum or a scalar
   * may be both, one type to a client */
  final class Index(schema: Schema):
    val outputs: mutable.LinkedHashMap[String, Named] = mutable.LinkedHashMap.empty
    val inputs: mutable.LinkedHashMap[String, InNamed] = mutable.LinkedHashMap.empty
    private val clashes = mutable.ArrayBuffer.empty[String]

    locally {
      val work = mutable.Queue.empty[OutType[?] | InType[?]]
      work += schema.executableQuery
      work ++= schema.mutation
      work ++= schema.subscription.map(_.asObject)
      work += Scalar.string
      work += Scalar.boolean
      def out(n: Named & OutType[?]): Boolean = outputs.get(n.name) match
        case Some(o) =>
          if o != n then (o, n) match
            case (_: Scalar[?], _: Scalar[?]) => () // a scalar is its name: two values of one are one type
            case _ => clashes += s"two types named ${n.name}"
          false
        case None => outputs(n.name) = n; true
      def in(n: InNamed & InType[?]): Boolean = inputs.get(n.name) match
        case Some(_) => false
        case None => inputs(n.name) = n; true
      while work.nonEmpty do
        work.dequeue() match
          case Nullable(of) => work += of
          case ListOf(of) => work += of
          case Fetched(of) => work += of
          case Optional(of) => work += of
          case InList(of) => work += of
          case s: Scalar[?] => out(s): Unit
          case e: EnumType[?] => out(e): Unit
          case o: ObjectType[?] => if out(o) then o.fields.foreach { f => work += f.tpe; work ++= f.args.map(_.tpe) }
          case i: InterfaceType[?] => if out(i) then
            i.fields.foreach { f => work += f.tpe; work ++= f.args.map(_.tpe) }
            work ++= i.members.map(_.tpe)
          case u: UnionType[?] => if out(u) then work ++= u.members.map(_.tpe)
          case s: InScalar[?] => in(s): Unit
          case e: InEnum[?] => in(e): Unit
          case i: InObject[?] => if in(i) then work ++= i.fields.map(_.tpe)
    }

    /** every name, outputs first, in the order met */
    lazy val names: List[String] = (outputs.keys ++ inputs.keys.filterNot(outputs.contains)).toList

    /** the interfaces naming the object `name` a member */
    def interfacesOf(name: String): List[InterfaceType[?]] =
      outputs.valuesIterator.collect { case i: InterfaceType[?] if i.members.exists(_.tpe.name == name) => i }.toList

    /** the object types a value of the type `name` may be */
    def possible(name: String): Set[String] = outputs.get(name) match
      case Some(o: ObjectType[?]) => Set(o.name)
      case Some(i: InterfaceType[?]) => i.members.map(_.tpe.name).toSet
      case Some(u: UnionType[?]) => u.members.map(_.tpe.name).toSet
      case _ => Set.empty

    /** a fragment on `condition` applies to the object `obj` */
    def applies(condition: String, obj: String): Boolean = condition == obj || possible(condition).contains(obj)

    /** an input type by name: the built-in scalars, or one the schema reaches */
    def input(name: String): Option[InNamed] = inputs.get(name).orElse(name match
      case "Int" => Some(InScalar.int)
      case "Float" => Some(InScalar.float)
      case "String" => Some(InScalar.string)
      case "Boolean" => Some(InScalar.boolean)
      case "ID" => Some(InScalar.id)
      case _ => None)

    lazy val problems: List[String] =
      val memberErrors = outputs.valuesIterator.collect { case i: InterfaceType[?] => i }.flatMap { i =>
        i.members.flatMap { m =>
          i.fields.flatMap { f =>
            m.tpe.field.get(f.name) match
              case None => List(s"${m.tpe.name} is a member of ${i.name} but has no field ${f.name}")
              case Some(g) =>
                val ok = g.tpe.ref == f.tpe.ref || f.tpe.ref == g.tpe.ref.stripSuffix("!") ||
                  possible(Executor.named(f.tpe).name).contains(Executor.named(g.tpe).name)
                if ok then Nil else List(s"${m.tpe.name}.${f.name} is ${g.tpe.ref}, ${i.name}.${f.name} is ${f.tpe.ref}")
          }
        }
      }
      (clashes.toList ++ memberErrors).distinct
