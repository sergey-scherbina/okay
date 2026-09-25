package okay.r

import okay.{Cont, reset, />}
import okay.codec.Codecs
import okay.codec.Schema
import RValue.*

/**
 * A Scala value as an R value and back, through its `Schema`
 * (foreign-typed-calls, specs/foreign-highlevel.md stage 2) — okay-py's
 * `PyCodec` in R's vocabulary.
 *
 * A product is a NAMED LIST of its fields; a sum is the case's named list
 * with a `type` element naming the case; `None` is NULL (a typed NA where
 * the schema is a scalar R has a type for); a sequence is a vector (an
 * atomic one when its elements are scalars — R's shim simplifies — and a
 * list otherwise).
 *
 * Two things R does not have, and what crosses instead:
 *  - SCALARS. Every R value is a vector, so a number answered by R is a
 *    length-1 vector; the decoder takes a scalar from one.
 *  - A 64-BIT INTEGER. A `Long` is an R integer when it fits 32 bits, a
 *    double while the double is exact (|x| <= 2^53), and its digits
 *    beyond; it decodes from any of the three. `BigInt` is its digits.
 *
 * TYPED, like `PyCodec`: both directions match the `Schema` GADT, and the
 * product and sum kernels hold the only casts. Decoding refuses by path.
 */
object RCodec {

  /** the element a sum's named list names its case in */
  val TypeField = "type"

  /** the largest magnitude a double carries exactly */
  val Exact: Double = 9007199254740992.0
  private val ExactLong: Long = 1L << 53

  /** a Long in R's vocabulary: see the object comment */
  def long(x: Long): RValue =
    if x.isValidInt then I32(x.toInt)
    // compared as a LONG: `x.toDouble <= 2^53` let 2^53 + 1 through, because
    // the conversion rounds it to 2^53 before the comparison sees it
    else if x >= -ExactLong && x <= ExactLong then F64(x.toDouble)
    else Str(x.toString)

  def encode[A](a: A)(using s: Schema[A]): RValue = enc(s, a, 0)

  def decode[A](v: RValue)(using s: Schema[A]): Either[Condition, A] =
    dec(s, v, At.Root, 0).left.map(Condition("Decode", _))

  // Both roads recurse once per level of the VALUE: below
  // `Codecs.NativeThreshold` a direct call per level, at it the rest of
  // the value on the Cont trampoline (okay-py's PyCodec, over RValue;
  // stack-safety-py-r)

  private def enc[X](s: Schema[X], x: X, depth: Int): RValue =
    if depth >= Codecs.NativeThreshold then reset(encC[X, RValue](s, x))
    else encNative(s, x, depth)

  /** a sum's case as its named list, the case named in `type` */
  private def tagged(su: Schema.SSum[?], name: String, v: RValue): RValue = v match
    case Named(kv) =>
      if kv.exists(_._1 == TypeField) then throw IllegalArgumentException(
        s"okay.r: case $name of ${su.name} has a field named '$TypeField', which names the case on the wire")
      Named((TypeField -> Str(name)) +: kv)
    case other => Named(Vector(TypeField -> Str(name), "value" -> other))

  /** the leaves, shared by both roads: a scalar has no children, so this
   * is where every recursion ends */
  private def encScalar[X](s: Schema[X], x: X): RValue = s match
    case Schema.SInt => I32(x)
    case Schema.SLong => long(x)
    case Schema.SDouble => F64(x)
    case Schema.SBool => Bool(x)
    case Schema.SString => Str(x)
    case Schema.SChar => Str(x.toString)
    case Schema.SBytes => Bytes(x)
    case Schema.SBigInt => Str(x.toString)
    case other => throw IllegalStateException(s"okay.r: not a scalar schema: $other")

  private def encNative[X](s: Schema[X], x: X, depth: Int): RValue = s match
    case Schema.SInt | Schema.SLong | Schema.SDouble | Schema.SBool | Schema.SString
       | Schema.SChar | Schema.SBytes | Schema.SBigInt => encScalar(s, x)
    case o: Schema.SOption[a] => x match
      case Some(v) => enc(o.of(), v, depth + 1)
      case None => na(o.of())
    case l: Schema.SList[a] => Vec(x.iterator.map(enc(l.of(), _, depth + 1)).toVector)
    case v: Schema.SVector[a] => Vec(x.map(enc(v.of(), _, depth + 1)))
    case p: Schema.SProduct[X] =>
      Named(p.eachField(x)([Y] => (name: String, sc: Schema[Y], y: Y) => (name, enc(sc, y, depth + 1))))
    case su: Schema.SSum[X] =>
      su.theCase(x)([Y <: X] => (name: String, sc: Schema[Y], y: Y) => tagged(su, name, enc(sc, y, depth + 1)))
    case i: Schema.SIso[X, b] => enc(i.under(), i.from(x), depth + 1)

  /** a field's schema and value at ONE type, held for the trampoline
   * without a cast (PyCodec's Held) */
  private final class Held[Y](sc: Schema[Y], y: Y):
    def encC[Ans]: RValue /> Ans = RCodec.encC(sc, y)

  private def encC[X, Ans](s: Schema[X], x: X): RValue /> Ans = s match
    case Schema.SInt | Schema.SLong | Schema.SDouble | Schema.SBool | Schema.SString
       | Schema.SChar | Schema.SBytes | Schema.SBigInt => Cont.Pure(encScalar(s, x))
    case o: Schema.SOption[a] => x match
      case Some(v) => Cont.delay(() => encC(o.of(), v))
      case None => Cont.Pure(na(o.of()))
    case l: Schema.SList[a] => encAll(l.of(), x.toVector)
    case v: Schema.SVector[a] => encAll(v.of(), x)
    case p: Schema.SProduct[X] =>
      val held = p.eachField(x)([Y] => (name: String, sc: Schema[Y], y: Y) => (name, Held(sc, y): Held[?]))
      def loop(rest: List[(String, Held[?])], acc: Vector[(String, RValue)]): RValue /> Ans = rest match
        case Nil => Cont.Pure(Named(acc))
        case (name, h) :: more => Cont.defer(() => h.encC[Ans])(v => loop(more, acc :+ (name -> v)))
      loop(held.toList, Vector.empty)
    case su: Schema.SSum[X] =>
      val (name, h) = su.theCase(x)([Y <: X] => (name: String, sc: Schema[Y], y: Y) => (name, Held(sc, y): Held[?]))
      Cont.defer(() => h.encC[Ans])(v => Cont.Pure(tagged(su, name, v)))
    case i: Schema.SIso[X, b] => Cont.delay(() => encC(i.under(), i.from(x)))

  private def encAll[Y, Ans](sc: Schema[Y], xs: Vector[Y]): RValue /> Ans =
    def loop(i: Int, acc: Vector[RValue]): RValue /> Ans =
      if i >= xs.length then Cont.Pure(Vec(acc))
      else Cont.defer(() => encC[Y, Ans](sc, xs(i)))(v => loop(i + 1, acc :+ v))
    loop(0, Vector.empty)

  /** an absent value keeps its type where R has one for it */
  private def na[X](s: Schema[X]): RValue = s match
    case Schema.SInt => NA(RType.Integer)
    case Schema.SDouble => NA(RType.Double)
    case Schema.SString | Schema.SChar => NA(RType.Character)
    case Schema.SBool => NA(RType.Logical)
    case _ => RNull

  /** R has no scalars: a length-1 vector IS the scalar */
  private def one(v: RValue): RValue = v match
    case Vec(Vector(x)) => x
    case other => other

  private def absent(v: RValue): Boolean = one(v) match
    case RNull | NA(_) => true
    case Vec(xs) => xs.isEmpty
    case _ => false

  /** where in the value a decode is: a LINKED path, rendered only into a
   * message — a string grown per level was quadratic in the depth, and a
   * 200 000-deep value ran out of heap before it ran out of stack
   * (stack-safety-py-r) */
  private enum At:
    case Root
    case Field(parent: At, name: String)
    case Index(parent: At, i: Int)
    def render: String =
      val parts = scala.collection.mutable.ArrayBuffer[String]()
      var cur: At = this
      while cur != Root do cur match
        case Field(p, n) => parts += s"$$$n"; cur = p
        case Index(p, i) => parts += s"[${i + 1}]"; cur = p
        case Root => ()
      parts.reverseIterator.mkString
    /** the path in a message: "the value" at the root */
    def where: String = if this == Root then "the value" else render

  /** a value in a message without walking it: a deep one would recurse
   * in its own toString */
  private def describe(v: RValue): String = v match
    case Vec(xs) => s"a vector of ${xs.length}"
    case Named(kv) => s"a named list of ${kv.length}"
    case other => other.toString

  private def dec[X](s: Schema[X], v: RValue, at: At, depth: Int): Either[String, X] =
    if depth >= Codecs.NativeThreshold then reset(decC[X, Either[String, X]](s, v, at))
    else decNative(s, v, at, depth)

  /** the leaves, shared by both roads: a scalar has no children, so this
   * is where every recursion ends */
  private def decScalar[X](s: Schema[X], v: RValue, at: At): Either[String, X] =
    def no(what: String): Either[String, X] = Left(s"${at.where}: expected $what, got ${describe(v)}")
    s match
      case Schema.SInt => one(v) match
        case I32(n) => Right(n)
        case F64(d) if d == math.floor(d) && d.isValidInt => Right(d.toInt)
        case _ => no("an integer")
      case Schema.SLong => one(v) match
        case I32(n) => Right(n.toLong)
        case F64(d) if d == math.floor(d) && math.abs(d) <= Exact => Right(d.toLong)
        case Str(t) if t.toLongOption.isDefined => Right(t.toLong)
        case _ => no("an integer")
      case Schema.SDouble => one(v) match
        case F64(d) => Right(d)
        case I32(n) => Right(n.toDouble)
        case _ => no("a number")
      case Schema.SBool => one(v) match
        case Bool(b) => Right(b)
        case _ => no("a logical")
      case Schema.SString => one(v) match
        case Str(t) => Right(t)
        case _ => no("a character")
      case Schema.SChar => one(v) match
        case Str(t) if t.length == 1 => Right(t.charAt(0))
        case _ => no("a one-character string")
      case Schema.SBytes => v match
        case Bytes(b) => Right(b)
        case _ => no("raw bytes")
      case Schema.SBigInt => one(v) match
        case I32(n) => Right(BigInt(n))
        case F64(d) if d == math.floor(d) && math.abs(d) <= Exact => Right(BigInt(d.toLong))
        case Str(t) if t.nonEmpty && t.stripPrefix("-").forall(_.isDigit) => Right(BigInt(t))
        case _ => no("an integer")
      case other => Left(s"${at.where}: not a scalar schema: $other")

  private def decNative[X](s: Schema[X], v: RValue, at: At, depth: Int): Either[String, X] =
    def no(what: String): Either[String, X] = Left(s"${at.where}: expected $what, got ${describe(v)}")
    s match
      case Schema.SInt | Schema.SLong | Schema.SDouble | Schema.SBool | Schema.SString
         | Schema.SChar | Schema.SBytes | Schema.SBigInt => decScalar(s, v, at)
      case o: Schema.SOption[a] =>
        if absent(v) then Right(None) else dec(o.of(), v, at, depth + 1).map(Some(_))
      case l: Schema.SList[a] => seq(v) match
        case Some(xs) => each(xs, at)(dec(l.of(), _, _, depth + 1)).map(_.toList)
        case None => no("a vector")
      case sv: Schema.SVector[a] => seq(v) match
        case Some(xs) => each(xs, at)(dec(sv.of(), _, _, depth + 1))
        case None => no("a vector")
      case p: Schema.SProduct[X] => one(v) match
        case Named(kv) => product(p, kv.toMap, at, depth)
        case _ => no(s"a named list for ${p.name}")
      case su: Schema.SSum[X] => one(v) match
        case Named(kv) => caseOf(su, kv, at) match
          case Left(e) => Left(e)
          case Right((sc, rest)) => dec(sc, rest, at, depth + 1)
        case _ => no(s"a named list for ${su.name}")
      case i: Schema.SIso[X, b] =>
        dec(i.under(), v, at, depth + 1).flatMap(u => i.to(u).left.map(why => s"${at.where}: $why"))

  /** the case a sum's named list names, and the list it decodes from */
  private def caseOf[X](su: Schema.SSum[X], kv: Vector[(String, RValue)], at: At): Either[String, (Schema[? <: X], RValue)] =
    def no(what: String) = Left(s"${at.where}: expected $what, got ${describe(Named(kv))}")
    val m = kv.toMap
    m.get(TypeField).map(one) match
      case Some(Str(name)) =>
        su.cases.indexWhere(_._1 == name) match
          case -1 => no(s"one of ${su.cases.map(_._1).mkString(", ")} in '$TypeField'")
          case i =>
            val sc = su.cases(i)._2()
            val rest = sc match
              case _: Schema.SProduct[?] => Named(kv.filterNot(_._1 == TypeField))
              case _ => m.getOrElse("value", RNull)
            Right((sc, rest))
      case _ => no(s"a named list with a '$TypeField' naming a case of ${su.name}")

  /** the road past the threshold: the same decisions as `decNative`,
   * each child a `Cont.defer` (PyCodec's decC, over RValue) */
  private def decC[X, Ans](s: Schema[X], v: RValue, at: At): Either[String, X] /> Ans =
    def no(what: String): Either[String, X] = Left(s"${at.where}: expected $what, got ${describe(v)}")
    s match
      case Schema.SInt | Schema.SLong | Schema.SDouble | Schema.SBool | Schema.SString
         | Schema.SChar | Schema.SBytes | Schema.SBigInt => Cont.Pure(decScalar(s, v, at))
      case o: Schema.SOption[a] =>
        if absent(v) then Cont.Pure(Right(None))
        else Cont.defer(() => decC[a, Ans](o.of(), v, at))(r => Cont.Pure(r.map(Some(_))))
      case l: Schema.SList[a] => seq(v) match
        case Some(xs) => decAll[a, Ans](l.of(), xs, at).flatMap(r => Cont.Pure(r.map(_.toList)))
        case None => Cont.Pure(no("a vector"))
      case sv: Schema.SVector[a] => seq(v) match
        case Some(xs) => decAll[a, Ans](sv.of(), xs, at)
        case None => Cont.Pure(no("a vector"))
      case p: Schema.SProduct[X] => one(v) match
        case Named(kv) =>
          val m = kv.toMap
          def again(i: Int, acc: Vector[Any]): Either[String, Vector[Any]] /> Ans = loop(i, acc)
          @scala.annotation.tailrec def loop(i: Int, acc: Vector[Any]): Either[String, Vector[Any]] /> Ans =
            if i >= p.fields.length then Cont.Pure(Right(acc))
            else
              val (name, sc) = p.fields(i)
              val here = At.Field(at, name)
              m.get(name) match
                case Some(fv) => Cont.defer(() => fieldC[Ans](sc(), fv, here)) {
                  case Left(e) => Cont.Pure(Left(e))
                  case Right(x) => again(i + 1, acc :+ x)
                }
                case None => absent(p, i, sc, here) match
                  case Left(e) => Cont.Pure(Left(e))
                  case Right(d) => loop(i + 1, acc :+ d)
          loop(0, Vector.empty).flatMap(r => Cont.Pure(r.map(p.make)))
        case _ => Cont.Pure(no(s"a named list for ${p.name}"))
      case su: Schema.SSum[X] => one(v) match
        case Named(kv) => caseOf(su, kv, at) match
          case Left(e) => Cont.Pure(Left(e))
          case Right((sc, rest)) => sc match
            case sc: Schema[c] => Cont.defer(() => decC[c, Ans](sc, rest, at))(r => Cont.Pure(r: Either[String, X]))
        case _ => Cont.Pure(no(s"a named list for ${su.name}"))
      case i: Schema.SIso[X, b] =>
        Cont.defer(() => decC[b, Ans](i.under(), v, at))(r =>
          Cont.Pure(r.flatMap(u => i.to(u).left.map(why => s"${at.where}: $why"))))

  private def fieldC[Ans](sc: Schema[?], v: RValue, at: At): Either[String, Any] /> Ans = sc match
    case sc: Schema[y] => Cont.defer(() => decC[y, Ans](sc, v, at))(r => Cont.Pure(r: Either[String, Any]))

  private def decAll[Y, Ans](sc: Schema[Y], xs: Vector[RValue], at: At): Either[String, Vector[Y]] /> Ans =
    def loop(i: Int, acc: Vector[Y]): Either[String, Vector[Y]] /> Ans =
      if i >= xs.length then Cont.Pure(Right(acc))
      // R counts from 1, and so does the path an R user reads
      else Cont.defer(() => decC[Y, Ans](sc, xs(i), At.Index(at, i))) {
        case Left(e) => Cont.Pure(Left(e))
        case Right(y) => loop(i + 1, acc :+ y)
      }
    loop(0, Vector.empty)

  /** a field the list does not carry: its default, None for an option,
   * or named as missing */
  private def absent(p: Schema.SProduct[?], i: Int, sc: () => Schema[?], here: At): Either[String, Any] =
    p.defaultAt(i)([Y] => (_: Schema[Y], d: Y) => d: Any) match
      case Some(d) => Right(d)
      case None => sc() match
        case _: Schema.SOption[?] => Right(None)
        case _ => Left(s"${here.render}: missing")

  /** a vector's elements: NULL is the empty vector, a lone value one */
  private def seq(v: RValue): Option[Vector[RValue]] = v match
    case Vec(xs) => Some(xs)
    case RNull => Some(Vector.empty)
    case Named(_) => None
    case other => Some(Vector(other))

  private def each[Y](xs: Vector[RValue], at: At)(f: (RValue, At) => Either[String, Y]): Either[String, Vector[Y]] =
    val out = Vector.newBuilder[Y]
    var i = 0
    var bad: Option[String] = None
    while bad.isEmpty && i < xs.length do
      // R counts from 1, and so does the path an R user reads
      f(xs(i), At.Index(at, i)) match
        case Right(y) => out += y
        case Left(e) => bad = Some(e)
      i += 1
    bad.toLeft(out.result())

  private def product[X](p: Schema.SProduct[X], m: Map[String, RValue], at: At, depth: Int): Either[String, X] =
    val vals = Vector.newBuilder[Any]
    var bad: Option[String] = None
    var i = 0
    while bad.isEmpty && i < p.fields.length do
      val (name, sc) = p.fields(i)
      val here = At.Field(at, name)
      m.get(name) match
        case Some(v) => dec(sc(), v, here, depth + 1) match
          case Right(x) => vals += x
          case Left(e) => bad = Some(e)
        case None => absent(p, i, sc, here) match
          case Right(d) => vals += d
          case Left(e) => bad = Some(e)
      i += 1
    bad.toLeft(p.make(vals.result()))
}
