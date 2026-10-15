package okay.graphql

import okay.codec.Json
import okay.codec.Json.*
import okay.one.*
import okay.one.Cont.pure
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

/** a type with a name: a scalar, an enum, an object */
sealed trait Named:
  def name: String

/** a scalar: its value as JSON */
final case class Scalar[A](name: String, encode: A => Json) extends OutType[A] with Named

object Scalar:
  val int: Scalar[Int] = Scalar("Int", i => JNum(i.toDouble))
  val float: Scalar[Double] = Scalar("Float", JNum(_))
  val string: Scalar[String] = Scalar("String", JStr(_))
  val boolean: Scalar[Boolean] = Scalar("Boolean", JBool(_))
  val id: Scalar[String] = Scalar("ID", JStr(_))

/** an enum: each value of `A` by its name */
final case class EnumType[A](name: String, values: List[(String, A)]) extends OutType[A] with Named:
  def nameOf(a: A): Option[String] = values.collectFirst { case (n, v) if v == a => n }

/** a list: its items each of `of` */
final case class ListOf[A](of: OutType[A]) extends OutType[Seq[A]]

/** may be null: `None` */
final case class Nullable[A](of: OutType[A]) extends OutType[Option[A]]

/** A VALUE STILL TO BE FETCHED: a program, run as a task of its own where it stands — so its error is ITS position's
 * (a list's item: `["byIds", 1]`), not the field's, and its fetches meet its siblings' in the same round. The SDL
 * does not see it: `Fetched(of)` is `of` to a client */
final case class Fetched[A](of: OutType[A]) extends OutType[A ! Fetches]

/** AN OBJECT TYPE: fields of a parent `A`, each resolved by a program. Its fields are lazy, so types may refer to
 * each other (a user's orders, an order's user) */
final class ObjectType[A](val name: String, val description: Option[String] = None)(fs: => List[Field[A, ?]])
    extends OutType[A] with Named:
  lazy val fields: List[Field[A, ?]] = fs
  lazy val field: Map[String, Field[A, ?]] = fields.map(f => f.name -> f).toMap
  override def toString: String = s"ObjectType($name)"

/** A FIELD of a parent `A`, of the type `tpe`: its arguments, and the program that resolves it */
final class Field[A, B](val name: String, val tpe: OutType[B], val args: List[Arg[?]],
    val resolve: (A, Args) => B ! Fetches, val description: Option[String])

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
  def ref: String = this match
    case Optional(of) => of.ref.stripSuffix("!")
    case InList(of) => s"[${of.ref}]!"
    case n: InNamed => s"${n.name}!"

sealed trait InNamed:
  def name: String

final case class InScalar[A](name: String, read: Json => Either[String, A], show: A => String)
    extends InType[A] with InNamed:
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

final case class InEnum[A](name: String, values: List[(String, A)]) extends InType[A] with InNamed:
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

/** an input object: its fields read as arguments are, then built */
final class InObject[A](val name: String, val fields: List[Arg[?]])(build: Args => A, show: A => String = (a: A) => a.toString)
    extends InType[A] with InNamed:
  def decode(j: Json): Either[String, A] = j match
    case JObj(fs) =>
      val unknown = fs.map(_._1).filterNot(n => fields.exists(_.name == n))
      if unknown.nonEmpty then Left(s"$name has no field ${unknown.mkString(", ")}")
      else Args.of(fields, fs.toMap).map(build)
    case other => Left(s"$name cannot represent ${Json.print(other)}")
  def literal(a: A): String = show(a)

/** AN ARGUMENT: its name, its input type, a default */
final case class Arg[A](name: String, tpe: InType[A], default: Option[A] = None):
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

/** A SCHEMA: the root types, a query's and a mutation's */
final case class Schema(query: ObjectType[Unit], mutation: Option[ObjectType[Unit]] = None):
  /** the schema in SDL: every type reached from the roots, in the order met */
  def sdl: String =
    val seen = mutable.LinkedHashMap.empty[String, String]
    val work = mutable.Queue.empty[OutType[?] | InType[?]]
    work ++= (query :: mutation.toList)
    def args(as: List[Arg[?]]): String =
      if as.isEmpty then ""
      else as.map { a =>
        work += a.tpe
        a match
          case a: Arg[t] => s"${a.name}: ${a.tpe.ref}" + a.default.fold("")(d => s" = ${a.tpe.literal(d)}")
      }.mkString("(", ", ", ")")
    while work.nonEmpty do
      work.dequeue() match
        case Nullable(of) => work += of
        case ListOf(of) => work += of
        case Fetched(of) => work += of
        case Optional(of) => work += of
        case InList(of) => work += of
        case s: Scalar[?] => if !builtin(s.name) && !seen.contains(s.name) then seen(s.name) = s"scalar ${s.name}"
        case s: InScalar[?] => if !builtin(s.name) && !seen.contains(s.name) then seen(s.name) = s"scalar ${s.name}"
        case e: EnumType[?] => if !seen.contains(e.name) then
          seen(e.name) = e.values.map("  " + _._1).mkString(s"enum ${e.name} {\n", "\n", "\n}")
        case e: InEnum[?] => if !seen.contains(e.name) then
          seen(e.name) = e.values.map("  " + _._1).mkString(s"enum ${e.name} {\n", "\n", "\n}")
        case o: ObjectType[?] => if !seen.contains(o.name) then
          seen(o.name) = ""
          val body = o.fields.map { f =>
            work += f.tpe
            val doc = f.description.fold("")(d => s"  ${Json.print(JStr(d))}\n")
            s"$doc  ${f.name}${args(f.args)}: ${f.tpe.ref}"
          }
          val doc = o.description.fold("")(d => s"${Json.print(JStr(d))}\n")
          seen(o.name) = body.mkString(s"${doc}type ${o.name} {\n", "\n", "\n}")
        case i: InObject[?] => if !seen.contains(i.name) then
          seen(i.name) = ""
          val body = i.fields.map { a =>
            work += a.tpe
            a match
              case a: Arg[t] => s"  ${a.name}: ${a.tpe.ref}" + a.default.fold("")(d => s" = ${a.tpe.literal(d)}")
          }
          seen(i.name) = body.mkString(s"input ${i.name} {\n", "\n", "\n}")
    val roots =
      if query.name == "Query" && mutation.forall(_.name == "Mutation") then Nil
      else List((("  query: " + query.name) :: mutation.toList.map("  mutation: " + _.name))
        .mkString("schema {\n", "\n", "\n}"))
    (roots ++ seen.values).mkString("", "\n\n", "\n")

  private def builtin(name: String) = Set("Int", "Float", "String", "Boolean", "ID")(name)
