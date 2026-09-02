package okay.codec

import scala.compiletime.{constValueTuple, erasedValue, summonInline}
import scala.deriving.Mirror

/**
 * The reified shape of a datatype (specs/codecs.md): every derivation
 * — JSON, CBOR, a validator, a Spark encoder — is a CATAMORPHISM over
 * this one structure with its own algebra. Derived once per type via
 * Mirrors (inline, dependency-free); recursion is broken by thunked
 * fields (a self-referential type's schema refers to its own given
 * lazily, so construction terminates).
 */
enum Schema[A]:
  case SInt extends Schema[Int]
  case SLong extends Schema[Long]
  case SDouble extends Schema[Double]
  case SBool extends Schema[Boolean]
  case SString extends Schema[String]
  /** one character — surfaced by deriving okay-ui's Event, whose raw
   * key IS a Char; on the wire a char is a one-character string */
  case SChar extends Schema[Char]
  /**
   * Raw bytes. Not a convenience: CBOR has a first-class byte string
   * (major type 2) and JSON has no bytes at all, so without this every
   * binary payload has to be smuggled through a text field — which is
   * how an embedding came to travel as `List[Double]`, nine bytes and
   * one boxed object per component.
   *
   * `Array[Byte]` carries reference equality, so a product holding one
   * is not a value for `==`. That is the honest cost of not copying.
   */
  case SBytes extends Schema[Array[Byte]]
  case SOption[A](of: () => Schema[A]) extends Schema[Option[A]]
  case SList[A](of: () => Schema[A]) extends Schema[List[A]]
  /** the OTHER sequence this stack actually uses — Ui children,
   * codec fields, chunk contents are Vectors; smuggling them through
   * List cost a conversion at every derivation edge (codec-vector) */
  case SVector[A](of: () => Schema[A]) extends Schema[Vector[A]]
  case SProduct[A](name: String, fields: Vector[(String, () => Schema[?])],
                   make: Seq[Any] => A, parts: A => Seq[Any],
                   /** aligned with fields; a decoder falls back here
                    * when the wire lacks the field (codec-defaults) */
                   defaults: Vector[Option[() => Any]] = Vector.empty) extends Schema[A]
  case SSum[A](name: String, cases: Vector[(String, () => Schema[? <: A])],
               caseOf: A => Int) extends Schema[A]
  /** the newtype node (codec-iso): A travels as B — encode is `from`
   * then under's encode, decode is under's decode then `to`, and a
   * Left from `to` is a decode error like any other. To every
   * algebra the wrapper does not exist, which is the point. */
  case SIso[A, B](under: () => Schema[B],
                  to: B => Either[String, A],
                  from: A => B) extends Schema[A]

object Schema {

  /** the PRODUCT kernel, once: `parts` is the Mirror's productIterator
   * in field order, so the i-th value IS the i-th field's type — a
   * codec sees each field at that type through f and never casts */
  extension [A](p: SProduct[A])
    def eachField[R](a: A)(f: [X] => (String, Schema[X], X) => R): Vector[R] =
      def one[X](name: String, sc: Schema[X], v: Any): R = f(name, sc, v.asInstanceOf[X])
      p.parts(a).toVector.zip(p.fields).map((v, fld) => one(fld._1, fld._2(), v))

  /** the SUM kernel, once: `caseOf` is the Mirror's ordinal, so the
   * value IS that case's type — a codec sees it at that type through f */
  extension [A](su: SSum[A])
    def theCase[R](a: A)(f: [X <: A] => (String, Schema[X], X) => R): R =
      val (name, sc) = su.cases(su.caseOf(a))
      def one[X <: A](sc: Schema[X]): R = f(name, sc, a.asInstanceOf[X])
      one(sc())

  given Schema[Int] = Schema.SInt
  given Schema[Long] = Schema.SLong
  given Schema[Double] = Schema.SDouble
  given Schema[Boolean] = Schema.SBool
  given Schema[String] = Schema.SString
  given Schema[Char] = Schema.SChar
  given Schema[Array[Byte]] = Schema.SBytes

  /** a total wrapper — a newtype travels as what it wraps */
  def wrap[A, B](to: B => A, from: A => B)(using s: => Schema[B]): Schema[A] =
    Schema.SIso(() => s, b => Right(to(b)), from)

  /** a refining wrapper — a Left is a decode error naming itself */
  def refine[A, B](to: B => Either[String, A], from: A => B)(using s: => Schema[B]): Schema[A] =
    Schema.SIso(() => s, to, from)

  given [A](using s: => Schema[A]): Schema[Option[A]] = Schema.SOption(() => s)
  given [A](using s: => Schema[A]): Schema[List[A]] = Schema.SList(() => s)
  given [A](using s: => Schema[A]): Schema[Vector[A]] = Schema.SVector(() => s)

  private inline def thunks[T <: Tuple]: List[() => Schema[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t) => (() => summonInline[Schema[h]]) :: thunks[t]

  /** derive from the Mirror: products become named fields, sums named
   * cases; write `given Schema[T] = Schema.derived` (or `derives`) */
  inline given derived[A](using m: Mirror.Of[A]): Schema[A] =
    inline m match
      case p: Mirror.ProductOf[A] =>
        val labels = constValueTuple[p.MirroredElemLabels].toList.map(_.toString)
        val fields = labels.zip(thunks[p.MirroredElemTypes]).toVector
        Schema.SProduct(
          constValueTuple[Tuple1[p.MirroredLabel]].head.toString,
          fields,
          xs => p.fromProduct(Tuple.fromArray(xs.toArray)),
          a => a.asInstanceOf[Product].productIterator.toSeq,
          Defaults.of[A])
      case s: Mirror.SumOf[A] =>
        val labels = constValueTuple[s.MirroredElemLabels].toList.map(_.toString)
        // the Mirror's claim, once: a sum's element types are its
        // subtypes (the compiler derived them so; the inline match on
        // the tuple type cannot see the bound)
        val cases = thunks[s.MirroredElemTypes].map(_.asInstanceOf[() => Schema[? <: A]])
        Schema.SSum(
          constValueTuple[Tuple1[s.MirroredLabel]].head.toString,
          labels.zip(cases).toVector,
          a => s.ordinal(a))
}
