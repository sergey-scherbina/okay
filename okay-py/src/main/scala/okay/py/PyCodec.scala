package okay.py

import okay.{!, +, effect, pure}
import okay.RowLift.plus
import okay.codec.Schema
import PyValue.*

/**
 * A Scala value as a Python value and back, through its `Schema`
 * (foreign-typed-calls, specs/foreign-highlevel.md stage 2).
 *
 * A product is a `dict` of its fields; a sum is the case's dict with a
 * `"type"` field naming the case (the discriminated-union shape Python's
 * own libraries use); `Option` is None; a sequence is a list; a `BigInt`
 * is an int (its digits past a Long, which the wire carries exactly).
 *
 * TYPED: both directions match the `Schema` GADT, so each branch sees the
 * value at its own type; the product and sum kernels (`eachField`,
 * `theCase`) hold the only casts, where every codec in okay-codec holds
 * them. Decoding refuses by PATH (`.orders[2].qty: expected an int, got
 * Str(x)`), never with a silent default.
 *
 * Recursion is the value's own nesting, on the native stack: a record
 * nested ten thousand deep would overflow here where the WIRE would not.
 * Records crossing to Python are shallow; say so rather than pretend.
 */
object PyCodec {

  /** the field a sum's dict names its case in */
  val TypeField = "type"

  def encode[A](a: A)(using s: Schema[A]): PyValue = enc(s, a)

  def decode[A](v: PyValue)(using s: Schema[A]): Either[Condition, A] =
    dec(s, v, "").left.map(Condition("Decode", _))

  private def enc[X](s: Schema[X], x: X): PyValue = s match
    case Schema.SInt => I64(x.toLong)
    case Schema.SLong => I64(x)
    case Schema.SDouble => F64(x)
    case Schema.SBool => Bool(x)
    case Schema.SString => Str(x)
    case Schema.SChar => Str(x.toString)
    case Schema.SBytes => Bytes(x)
    case Schema.SBigInt => if x.isValidLong then I64(x.toLong) else BigI(x)
    case o: Schema.SOption[a] => x match
      case Some(v) => enc(o.of(), v)
      case None => PyNone
    case l: Schema.SList[a] => Arr(x.iterator.map(enc(l.of(), _)).toVector)
    case v: Schema.SVector[a] => Arr(x.map(enc(v.of(), _)))
    case p: Schema.SProduct[X] =>
      Dict(p.eachField(x)([Y] => (name: String, sc: Schema[Y], y: Y) => (name, enc(sc, y))))
    case su: Schema.SSum[X] =>
      su.theCase(x)([Y <: X] => (name: String, sc: Schema[Y], y: Y) => enc(sc, y) match
        case Dict(kv) =>
          if kv.exists(_._1 == TypeField) then throw IllegalArgumentException(
            s"okay.py: case $name of ${su.name} has a field named '$TypeField', which names the case on the wire")
          Dict((TypeField -> Str(name)) +: kv)
        case other => Dict(Vector(TypeField -> Str(name), "value" -> other)))
    case i: Schema.SIso[X, b] => enc(i.under(), i.from(x))

  private def dec[X](s: Schema[X], v: PyValue, at: String): Either[String, X] =
    def no(what: String): Either[String, X] = Left(s"${if at.isEmpty then "the value" else at}: expected $what, got $v")
    s match
      case Schema.SInt => v match
        case I64(n) if n.isValidInt => Right(n.toInt)
        case _ => no("an int that fits 32 bits")
      case Schema.SLong => v match
        case I64(n) => Right(n)
        case _ => no("an int")
      case Schema.SDouble => v match
        case F64(d) => Right(d)
        case I64(n) => Right(n.toDouble)     // statistics.median of ints answers an int
        case _ => no("a number")
      case Schema.SBool => v match
        case Bool(b) => Right(b)
        case _ => no("a bool")
      case Schema.SString => v match
        case Str(t) => Right(t)
        case _ => no("a str")
      case Schema.SChar => v match
        case Str(t) if t.length == 1 => Right(t.charAt(0))
        case _ => no("a one-character str")
      case Schema.SBytes => v match
        case Bytes(b) => Right(b)
        case _ => no("bytes")
      case Schema.SBigInt => v match
        case I64(n) => Right(BigInt(n))
        case BigI(n) => Right(n)
        case _ => no("an int")
      case o: Schema.SOption[a] => v match
        case PyNone => Right(None)
        case other => dec(o.of(), other, at).map(Some(_))
      case l: Schema.SList[a] => v match
        case Arr(xs) => each(xs, at)(dec(l.of(), _, _)).map(_.toList)
        case _ => no("a list")
      case sv: Schema.SVector[a] => v match
        case Arr(xs) => each(xs, at)(dec(sv.of(), _, _))
        case _ => no("a list")
      case p: Schema.SProduct[X] => v match
        case Dict(kv) => product(p, kv.toMap, at)
        case _ => no(s"a dict for ${p.name}")
      case su: Schema.SSum[X] => v match
        case Dict(kv) =>
          val m = kv.toMap
          m.get(TypeField) match
            case Some(Str(name)) =>
              su.cases.indexWhere(_._1 == name) match
                case -1 => no(s"one of ${su.cases.map(_._1).mkString(", ")} in '$TypeField'")
                case i =>
                  val sc = su.cases(i)._2()
                  val rest = sc match
                    case _: Schema.SProduct[?] => Dict(kv.filterNot(_._1 == TypeField))
                    case _ => m.getOrElse("value", PyNone)
                  dec(sc, rest, at)
            case _ => no(s"a dict with a '$TypeField' naming a case of ${su.name}")
        case _ => no(s"a dict for ${su.name}")
      case i: Schema.SIso[X, b] =>
        dec(i.under(), v, at).flatMap(u => i.to(u).left.map(why => s"${if at.isEmpty then "the value" else at}: $why"))

  private def each[Y](xs: Vector[PyValue], at: String)(f: (PyValue, String) => Either[String, Y]): Either[String, Vector[Y]] =
    val out = Vector.newBuilder[Y]
    var i = 0
    var bad: Option[String] = None
    while bad.isEmpty && i < xs.length do
      f(xs(i), s"$at[$i]") match
        case Right(y) => out += y
        case Left(e) => bad = Some(e)
      i += 1
    bad.toLeft(out.result())

  private def product[X](p: Schema.SProduct[X], m: Map[String, PyValue], at: String): Either[String, X] =
    val vals = Vector.newBuilder[Any]
    var bad: Option[String] = None
    var i = 0
    while bad.isEmpty && i < p.fields.length do
      val (name, sc) = p.fields(i)
      val here = s"$at.$name"
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
 * A Python function as a typed Scala function (foreign-typed-calls):
 *
 * {{{
 * val median = Py.fn[Double]("statistics:median")
 * median(Vector(3.0, 1.0, 2.0))   // Either[Condition, Double] ! PyEval
 * }}}
 *
 * The arguments are encoded through their `Schema`, the answer decoded
 * through `Out`'s; a Python exception and an answer of the wrong shape
 * are both a `Left(Condition)`, so a caller matches one channel. The
 * result is an okay program over `PyEval`, run by whichever handler is
 * installed — a subprocess, a worker pool, a canned mock, or `Durable`
 * over any of them.
 */
object Py {
  def fn[Out](address: String)(using Schema[Out]): Fn[Out] = Fn(address)

  /** a Python function over a LIST as an okay stage over chunks
   * (foreign-streaming): see `PyStream` */
  def stage[I: ToPy, O: Schema](address: String, chunk: Int = 64): Unit ! PyStream.Row[I, O] =
    PyStream.chunked[I, O](chunk, buf => PyEval.Call(address, Vector(PyValue.Arr(buf))), None)

  /**
   * Python source beside the Scala that calls it (foreign-inline-modules):
   * the source must be a compile-time constant, and the engine ships it
   * when a worker starts — `PySubprocess.start(..., modules = Seq(m))`.
   */
  inline def module(inline name: String, inline source: String): PyModule =
    scala.compiletime.requireConst(name)
    scala.compiletime.requireConst(source)
    PyModule.fromConstant(name, source)

  /**
   * Call `address` and KEEP its result in the worker, answering a handle
   * (foreign-object-handles): `Py.hold("random:Random")(42)` is a seeded
   * generator living in Python, whose methods `ref.call` reaches.
   */
  def hold(address: String): Hold = Hold(address)

  final class Hold(address: String):
    def apply(): Either[Condition, PyRef] ! PyEval = go(Vector.empty)
    def apply[A: ToPy](a: A): Either[Condition, PyRef] ! PyEval = go(Vector(ToPy(a)))
    def apply[A: ToPy, B: ToPy](a: A, b: B): Either[Condition, PyRef] ! PyEval = go(Vector(ToPy(a), ToPy(b)))
    def apply[A: ToPy, B: ToPy, C: ToPy](a: A, b: B, c: C): Either[Condition, PyRef] ! PyEval =
      go(Vector(ToPy(a), ToPy(b), ToPy(c)))
    private def go(args: Vector[PyValue]): Either[Condition, PyRef] ! PyEval =
      effect[PyEval, Either[Condition, PyRef]](PyEval.Hold(address, args))

  /**
   * A callback Python may call by name while okay runs one of its
   * functions (foreign-callbacks): `okay.call("objective", x)` in Python
   * decodes `x` as `Arg`, runs `f` as an okay PROGRAM in `F` under the
   * caller's handlers, and hands the `Res` back as the value of
   * `okay.call`. More than one argument arrives as a list.
   *
   * {{{
   * val objective = Py.callback[Vector[Double], Double]("objective")(x => Reader.ask[Double].map(...))
   * }}}
   */
  def callback[Arg: Schema, Res: Schema](name: String): CallbackOf[Arg, Res] = CallbackOf(name)

  /** the callbacks one call offers, all in one row `F` (a union row for
   * several effects: callbacks are programs, and a program has one row) */
  def callbacks[F[+_]](cbs: Callback[F]*): Callbacks[F] = Callbacks(cbs.toVector)

  final class CallbackOf[Arg: Schema, Res: Schema](name: String):
    def apply[F[+_]](f: Arg => Res ! F): Callback[F] = Callback(name, args =>
      val in = args match
        case Vector(one) => PyCodec.decode[Arg](one)
        case many => PyCodec.decode[Arg](PyValue.Arr(many))
      in match
        case Left(c) => pure[F, Either[Condition, PyValue]](Left(c))
        case Right(i) => f(i).map(o => Right(PyCodec.encode(o))))

  final class Callback[F[+_]](val name: String, val run: Vector[PyValue] => Either[Condition, PyValue] ! F)

  final class Callbacks[F[+_]](val all: Vector[Callback[F]]):
    def names: Vector[String] = all.map(_.name)
    def get(name: String): Option[Callback[F]] = all.find(_.name == name)

  final class Fn[Out](val address: String)(using out: Schema[Out]):
    def apply(): Either[Condition, Out] ! PyEval = call(Vector.empty)
    def apply[A: ToPy](a: A): Either[Condition, Out] ! PyEval =
      call(Vector(ToPy(a)))
    def apply[A: ToPy, B: ToPy](a: A, b: B): Either[Condition, Out] ! PyEval =
      call(Vector(ToPy(a), ToPy(b)))
    def apply[A: ToPy, B: ToPy, C: ToPy](a: A, b: B, c: C): Either[Condition, Out] ! PyEval =
      call(Vector(ToPy(a), ToPy(b), ToPy(c)))
    def apply[A: ToPy, B: ToPy, C: ToPy, D: ToPy](a: A, b: B, c: C, d: D): Either[Condition, Out] ! PyEval =
      call(Vector(ToPy(a), ToPy(b), ToPy(c), ToPy(d)))

    private def call(args: Vector[PyValue]): Either[Condition, Out] ! PyEval =
      effect[PyEval, Either[Condition, PyValue]](PyEval.Call(address, args))
        .map(_.flatMap(PyCodec.decode[Out](_)))

    /** this function, offered `cbs` to call back into (foreign-callbacks) */
    def calling[F[+_]](cbs: Callbacks[F]): Calling[F] = Calling(cbs)

    final class Calling[F[+_]](cbs: Callbacks[F]):
      def apply(): Either[Condition, Out] ! (F + PyEval) = dialogue(Vector.empty)
      def apply[A: ToPy](a: A): Either[Condition, Out] ! (F + PyEval) =
        dialogue(Vector(ToPy(a)))
      def apply[A: ToPy, B: ToPy](a: A, b: B): Either[Condition, Out] ! (F + PyEval) =
        dialogue(Vector(ToPy(a), ToPy(b)))
      def apply[A: ToPy, B: ToPy, C: ToPy](a: A, b: B, c: C): Either[Condition, Out] ! (F + PyEval) =
        dialogue(Vector(ToPy(a), ToPy(b), ToPy(c)))

      /**
       * The dialogue as a program: start, then per ask run the callback's
       * program and resume, until the function answers. Each step is an
       * okay node, so a function that calls back a million times is a loop,
       * not a million frames.
       */
      private def dialogue(args: Vector[PyValue]): Either[Condition, Out] ! (F + PyEval) =
        type R = F + PyEval
        def go(step: PyStep): Either[Condition, Out] ! R = step match
          case PyStep.Done(a) => pure[R, Either[Condition, Out]](a.flatMap(PyCodec.decode[Out](_)))
          case PyStep.Ask(name, as, k) =>
            val answered: Either[Condition, PyValue] ! R = cbs.get(name) match
              case Some(cb) => cb.run(as).plus[PyEval]
              case None => pure[R, Either[Condition, PyValue]](Left(Condition("NoCallback",
                s"'$name' is not among this call's callbacks (${cbs.names.mkString(", ")})")))
            answered.flatMap(a => effect[R, PyStep](PyEval.Resume(k, a))).flatMap(go)
        effect[R, PyStep](PyEval.Start(address, args, cbs.names)).flatMap(go)
}
