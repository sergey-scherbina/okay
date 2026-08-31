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
  case SProduct[A](name: String, fields: Vector[(String, () => Schema[?])],
                   make: Seq[Any] => A, parts: A => Seq[Any]) extends Schema[A]
  case SSum[A](name: String, cases: Vector[(String, () => Schema[?])],
               caseOf: A => Int) extends Schema[A]

object Schema {

  given Schema[Int] = Schema.SInt
  given Schema[Long] = Schema.SLong
  given Schema[Double] = Schema.SDouble
  given Schema[Boolean] = Schema.SBool
  given Schema[String] = Schema.SString
  given Schema[Array[Byte]] = Schema.SBytes

  given [A](using s: => Schema[A]): Schema[Option[A]] = Schema.SOption(() => s)
  given [A](using s: => Schema[A]): Schema[List[A]] = Schema.SList(() => s)

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
          a => a.asInstanceOf[Product].productIterator.toSeq)
      case s: Mirror.SumOf[A] =>
        val labels = constValueTuple[s.MirroredElemLabels].toList.map(_.toString)
        Schema.SSum(
          constValueTuple[Tuple1[s.MirroredLabel]].head.toString,
          labels.zip(thunks[s.MirroredElemTypes]).toVector,
          a => s.ordinal(a))
}
