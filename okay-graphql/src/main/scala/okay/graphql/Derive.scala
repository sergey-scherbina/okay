package okay.graphql

import okay.one.Cont.pure
import scala.deriving.Mirror
import scala.quoted.*

/** WHAT A SCALA TYPE IS IN A RESPONSE: its output type — of `B`, a supertype of `A` (a `List[T]` is a `Seq[T]` to
 * `ListOf`), so a field's getter returns the Scala value as it is */
trait Output[A]:
  type B >: A
  def tpe: OutType[B]

object Output:
  /** an output whose type is its Scala type's own */
  type Same[A] = Output[A] { type B = A }

  /** the output type `t`, lazily: types may refer to each other */
  def of[A](t: => OutType[A]): Output.Same[A] = new Output[A]:
    type B = A
    lazy val tpe: OutType[A] = t

  given int: Output.Same[Int] = of(Scalar.int)
  given double: Output.Same[Double] = of(Scalar.float)
  given string: Output.Same[String] = of(Scalar.string)
  given boolean: Output.Same[Boolean] = of(Scalar.boolean)
  given option[A](using o: Output[A]): Output[Option[A]] = new Output[Option[A]]:
    type B = Option[o.B]
    lazy val tpe: OutType[Option[o.B]] = Nullable(o.tpe)
  given list[A](using o: Output[A]): Output[List[A]] = new Output[List[A]]:
    type B = Seq[o.B]
    lazy val tpe: OutType[Seq[o.B]] = ListOf(o.tpe)
  given vector[A](using o: Output[A]): Output[Vector[A]] = new Output[Vector[A]]:
    type B = Seq[o.B]
    lazy val tpe: OutType[Seq[o.B]] = ListOf(o.tpe)
  given seq[A](using o: Output[A]): Output[Seq[A]] = new Output[Seq[A]]:
    type B = Seq[o.B]
    lazy val tpe: OutType[Seq[o.B]] = ListOf(o.tpe)
  /** an object type given — derived or written — is its type's output */
  given objectType[A](using t: ObjectType[A]): Output.Same[A] = of(t)
  given enumType[A](using t: EnumType[A]): Output.Same[A] = of(t)
  given interfaceType[A](using t: InterfaceType[A]): Output.Same[A] = of(t)
  given unionType[A](using t: UnionType[A]): Output.Same[A] = of(t)

/** WHAT A SCALA TYPE IS IN AN ARGUMENT: its input type */
trait Input[A]:
  def tpe: InType[A]

object Input:
  def of[A](t: => InType[A]): Input[A] = new Input[A]:
    lazy val tpe: InType[A] = t

  given int: Input[Int] = of(InScalar.int)
  given double: Input[Double] = of(InScalar.float)
  given string: Input[String] = of(InScalar.string)
  given boolean: Input[Boolean] = of(InScalar.boolean)
  given option[A](using i: Input[A]): Input[Option[A]] = of(Optional(i.tpe))
  given list[A](using i: Input[A]): Input[List[A]] = of(InList(i.tpe))
  given inEnum[A](using t: InEnum[A]): Input[A] = of(t)
  given inObject[A](using t: InObject[A]): Input[A] = of(t)

/** THE DERIVATIONS' MACROS: a case class's fields read by their own selectors, an enum's cases by their own
 * references — every getter and value typed by the compiler, no `productElement`, no cast */
object Derive:
  def objectType[A: Type](extra: Expr[Seq[Field[A, ?]]])(using Quotes): Expr[ObjectType[A]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[A]
    val sym = tpe.typeSymbol
    if !sym.flags.is(Flags.Case) then report.errorAndAbort(s"ObjectType.derive: ${tpe.show} is not a case class")
    val fields: List[Expr[Field[A, ?]]] = sym.caseFields.map { f =>
      tpe.memberType(f).asType match
        case '[t] =>
          val out = Expr.summon[Output[t]].getOrElse(report.errorAndAbort(
            s"ObjectType.derive[${sym.name}]: no Output for ${TypeRepr.of[t].show}, the type of ${f.name} — give one " +
              "(an ObjectType, an EnumType, Output.of(...))"))
          '{
            val o = $out
            new Field[A, o.B](${ Expr(f.name) }, o.tpe, Nil,
              (a: A, _: Args) => pure[Fetches, o.B](${ Select('a.asTerm, f).asExprOf[t] }), None)
          }
    }
    '{ ObjectType[A](${ Expr(sym.name) })(Derive.merged(${ Expr.ofList(fields) }, $extra.toList)) }

  /** the derived fields, each replaced by an extra one of its name, the other extras after */
  def merged[A](derived: List[Field[A, ?]], extra: List[Field[A, ?]]): List[Field[A, ?]] =
    derived.map(d => extra.find(_.name == d.name).getOrElse(d)) ++ extra.filterNot(e => derived.exists(_.name == e.name))

  /** a type's simple name */
  inline def name[E]: String = ${ nameOf[E] }
  def nameOf[E: Type](using Quotes): Expr[String] =
    import quotes.reflect.*
    Expr(TypeRepr.of[E].typeSymbol.name)

  /** an enum's values by their cases' names */
  inline def values[E]: List[(String, E)] = ${ cases[E] }

  /** an enum's cases, singletons all: each value by its reference, its name its own */
  def cases[E: Type](using Quotes): Expr[List[(String, E)]] =
    import quotes.reflect.*
    val sym = TypeRepr.of[E].typeSymbol
    val children = sym.children
    if children.isEmpty || children.exists(c => !c.isTerm) then
      report.errorAndAbort(s"derive: ${sym.name} is not an enum of singleton cases")
    Expr.ofList(children.map(c => '{ (${ Expr(c.name) }, ${ Ref(c).asExprOf[E] }) }))

  def inObject[A: Type](m: Expr[Mirror.ProductOf[A]])(using Quotes): Expr[InObject[A]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[A]
    val sym = tpe.typeSymbol
    val parts: List[(Expr[Arg[?]], Expr[Args] => Expr[Any])] = sym.caseFields.map { f =>
      tpe.memberType(f).asType match
        case '[t] =>
          val in = Expr.summon[Input[t]].getOrElse(report.errorAndAbort(
            s"InObject.derive[${sym.name}]: no Input for ${TypeRepr.of[t].show}, the type of ${f.name}"))
          val arg = '{ Arg[t](${ Expr(f.name) }, $in.tpe) }
          (arg, (args: Expr[Args]) => '{ $args($arg) })
    }
    val args = Expr.ofList(parts.map(_._1))
    '{
      InObject[A](${ Expr(sym.name) })($args,
        (as: Args) => $m.fromProduct(${ Expr.ofTupleFromSeq(parts.map(_._2('as))) }))
    }
