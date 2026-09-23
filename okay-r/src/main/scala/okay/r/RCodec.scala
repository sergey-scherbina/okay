package okay.r

import okay.{!, +, effect, pure}
import okay.RowLift.plus
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

  def encode[A](a: A)(using s: Schema[A]): RValue = enc(s, a)

  def decode[A](v: RValue)(using s: Schema[A]): Either[Condition, A] =
    dec(s, v, "").left.map(Condition("Decode", _))

  private def enc[X](s: Schema[X], x: X): RValue = s match
    case Schema.SInt => I32(x)
    case Schema.SLong => long(x)
    case Schema.SDouble => F64(x)
    case Schema.SBool => Bool(x)
    case Schema.SString => Str(x)
    case Schema.SChar => Str(x.toString)
    case Schema.SBytes => Bytes(x)
    case Schema.SBigInt => Str(x.toString)
    case o: Schema.SOption[a] => x match
      case Some(v) => enc(o.of(), v)
      case None => na(o.of())
    case l: Schema.SList[a] => Vec(x.iterator.map(enc(l.of(), _)).toVector)
    case v: Schema.SVector[a] => Vec(x.map(enc(v.of(), _)))
    case p: Schema.SProduct[X] =>
      Named(p.eachField(x)([Y] => (name: String, sc: Schema[Y], y: Y) => (name, enc(sc, y))))
    case su: Schema.SSum[X] =>
      su.theCase(x)([Y <: X] => (name: String, sc: Schema[Y], y: Y) => enc(sc, y) match
        case Named(kv) =>
          if kv.exists(_._1 == TypeField) then throw IllegalArgumentException(
            s"okay.r: case $name of ${su.name} has a field named '$TypeField', which names the case on the wire")
          Named((TypeField -> Str(name)) +: kv)
        case other => Named(Vector(TypeField -> Str(name), "value" -> other)))
    case i: Schema.SIso[X, b] => enc(i.under(), i.from(x))

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

  private def dec[X](s: Schema[X], v: RValue, at: String): Either[String, X] =
    def no(what: String): Either[String, X] = Left(s"${if at.isEmpty then "the value" else at}: expected $what, got $v")
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
      case o: Schema.SOption[a] =>
        if absent(v) then Right(None) else dec(o.of(), v, at).map(Some(_))
      case l: Schema.SList[a] => seq(v) match
        case Some(xs) => each(xs, at)(dec(l.of(), _, _)).map(_.toList)
        case None => no("a vector")
      case sv: Schema.SVector[a] => seq(v) match
        case Some(xs) => each(xs, at)(dec(sv.of(), _, _))
        case None => no("a vector")
      case p: Schema.SProduct[X] => one(v) match
        case Named(kv) => product(p, kv.toMap, at)
        case _ => no(s"a named list for ${p.name}")
      case su: Schema.SSum[X] => one(v) match
        case Named(kv) =>
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
                  dec(sc, rest, at)
            case _ => no(s"a named list with a '$TypeField' naming a case of ${su.name}")
        case _ => no(s"a named list for ${su.name}")
      case i: Schema.SIso[X, b] =>
        dec(i.under(), v, at).flatMap(u => i.to(u).left.map(why => s"${if at.isEmpty then "the value" else at}: $why"))

  /** a vector's elements: NULL is the empty vector, a lone value one */
  private def seq(v: RValue): Option[Vector[RValue]] = v match
    case Vec(xs) => Some(xs)
    case RNull => Some(Vector.empty)
    case Named(_) => None
    case other => Some(Vector(other))

  private def each[Y](xs: Vector[RValue], at: String)(f: (RValue, String) => Either[String, Y]): Either[String, Vector[Y]] =
    val out = Vector.newBuilder[Y]
    var i = 0
    var bad: Option[String] = None
    while bad.isEmpty && i < xs.length do
      // R counts from 1, and so does the path an R user reads
      f(xs(i), s"$at[${i + 1}]") match
        case Right(y) => out += y
        case Left(e) => bad = Some(e)
      i += 1
    bad.toLeft(out.result())

  private def product[X](p: Schema.SProduct[X], m: Map[String, RValue], at: String): Either[String, X] =
    val vals = Vector.newBuilder[Any]
    var bad: Option[String] = None
    var i = 0
    while bad.isEmpty && i < p.fields.length do
      val (name, sc) = p.fields(i)
      val here = s"$at$$$name"
      m.get(name) match
        case Some(v) => dec(sc(), v, here) match
          case Right(x) => vals += x
          case Left(e) => bad = Some(e)
        case None =>
          p.defaultAt(i)([Y] => (_: Schema[Y], d: Y) => d: Any) match
            case Some(d) => vals += d
            case None => sc() match
              case _: Schema.SOption[?] => vals += None
              case _ => bad = Some(s"$here: missing")
      i += 1
    bad.toLeft(p.make(vals.result()))
}

/**
 * An R function as a typed Scala function (foreign-typed-calls):
 *
 * {{{
 * val median = R.fn[Double]("stats::median")
 * median(Vector(3.0, 1.0, 2.0))   // Either[Condition, Double] ! REval
 * }}}
 *
 * okay-py's `Py.fn` in R: arguments through their `Schema`, the answer
 * through `Out`'s, an R condition and a wrong-shaped answer on one `Left`.
 */
object R {
  def fn[Out](address: String)(using Schema[Out]): Fn[Out] = Fn(address)

  /** call `address` and KEEP its result in the R process, answering a
   * handle (foreign-object-handles): `R.hold("stats::lm")(formula, data)` */
  def hold(address: String): Hold = Hold(address)

  final class Hold(address: String):
    def apply(): Either[Condition, RRef] ! REval = go(Vector.empty)
    def apply[A: ToR](a: A): Either[Condition, RRef] ! REval = go(Vector(ToR(a)))
    def apply[A: ToR, B: ToR](a: A, b: B): Either[Condition, RRef] ! REval = go(Vector(ToR(a), ToR(b)))
    def apply[A: ToR, B: ToR, C: ToR](a: A, b: B, c: C): Either[Condition, RRef] ! REval =
      go(Vector(ToR(a), ToR(b), ToR(c)))
    private def go(args: Vector[RValue]): Either[Condition, RRef] ! REval =
      effect[REval, Either[Condition, RRef]](REval.Hold(address, args))

  /**
   * A callback R may call by name while okay runs one of its functions
   * (foreign-callbacks): `okay_call("objective", x)` in R decodes `x` as
   * `Arg`, runs `f` as an okay program in `F` under the caller's
   * handlers, and answers `Res` as the value of `okay_call`. okay-py's
   * `Py.callback`, in R.
   */
  def callback[Arg: Schema, Res: Schema](name: String): CallbackOf[Arg, Res] = CallbackOf(name)

  def callbacks[F[+_]](cbs: Callback[F]*): Callbacks[F] = Callbacks(cbs.toVector)

  final class CallbackOf[Arg: Schema, Res: Schema](name: String):
    def apply[F[+_]](f: Arg => Res ! F): Callback[F] = Callback(name, args =>
      val in = args match
        case Vector(one) => RCodec.decode[Arg](one)
        case many => RCodec.decode[Arg](RValue.Vec(many))
      in match
        case Left(c) => pure[F, Either[Condition, RValue]](Left(c))
        case Right(i) => f(i).map(o => Right(RCodec.encode(o))))

  final class Callback[F[+_]](val name: String, val run: Vector[RValue] => Either[Condition, RValue] ! F)

  final class Callbacks[F[+_]](val all: Vector[Callback[F]]):
    def names: Vector[String] = all.map(_.name)
    def get(name: String): Option[Callback[F]] = all.find(_.name == name)

  final class Fn[Out](val address: String)(using out: Schema[Out]):
    def apply(): Either[Condition, Out] ! REval = call(Vector.empty)
    def apply[A: ToR](a: A): Either[Condition, Out] ! REval =
      call(Vector(ToR(a)))
    def apply[A: ToR, B: ToR](a: A, b: B): Either[Condition, Out] ! REval =
      call(Vector(ToR(a), ToR(b)))
    def apply[A: ToR, B: ToR, C: ToR](a: A, b: B, c: C): Either[Condition, Out] ! REval =
      call(Vector(ToR(a), ToR(b), ToR(c)))
    def apply[A: ToR, B: ToR, C: ToR, D: ToR](a: A, b: B, c: C, d: D): Either[Condition, Out] ! REval =
      call(Vector(ToR(a), ToR(b), ToR(c), ToR(d)))

    private def call(args: Vector[RValue]): Either[Condition, Out] ! REval =
      effect[REval, Either[Condition, RValue]](REval.Call(address, args))
        .map(_.flatMap(RCodec.decode[Out](_)))

    /** this function, offered `cbs` to call back into (foreign-callbacks) */
    def calling[F[+_]](cbs: Callbacks[F]): Calling[F] = Calling(cbs)

    final class Calling[F[+_]](cbs: Callbacks[F]):
      def apply(): Either[Condition, Out] ! (F + REval) = dialogue(Vector.empty)
      def apply[A: ToR](a: A): Either[Condition, Out] ! (F + REval) =
        dialogue(Vector(ToR(a)))
      def apply[A: ToR, B: ToR](a: A, b: B): Either[Condition, Out] ! (F + REval) =
        dialogue(Vector(ToR(a), ToR(b)))
      def apply[A: ToR, B: ToR, C: ToR](a: A, b: B, c: C): Either[Condition, Out] ! (F + REval) =
        dialogue(Vector(ToR(a), ToR(b), ToR(c)))

      /** start, then per ask run the callback's program and resume, until
       * the function answers — each step one okay node */
      private def dialogue(args: Vector[RValue]): Either[Condition, Out] ! (F + REval) =
        type Row = F + REval
        def go(step: RStep): Either[Condition, Out] ! Row = step match
          case RStep.Done(a) => pure[Row, Either[Condition, Out]](a.flatMap(RCodec.decode[Out](_)))
          case RStep.Ask(name, as, k) =>
            val answered: Either[Condition, RValue] ! Row = cbs.get(name) match
              case Some(cb) => cb.run(as).plus[REval]
              case None => pure[Row, Either[Condition, RValue]](Left(Condition("NoCallback",
                s"'$name' is not among this call's callbacks (${cbs.names.mkString(", ")})")))
            answered.flatMap(a => effect[Row, RStep](REval.Resume(k, a))).flatMap(go)
        effect[Row, RStep](REval.Start(address, args, cbs.names)).flatMap(go)
}
