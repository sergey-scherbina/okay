package okay.foreign

import okay.{!, +, effect, pure, Cont, reset, />}
import okay.codec.Codecs
import okay.Row.plus
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

  def encode[A](a: A)(using s: Schema[A]): PyValue = enc(s, a, 0)

  def decode[A](v: PyValue)(using s: Schema[A]): Either[Condition, A] =
    dec(s, v, At.Root, 0).left.map(Condition("Decode", _))

  // Both roads recurse once per level of the VALUE, which is as deep as
  // the program (encode) or the worker (decode) made it: below
  // `Codecs.NativeThreshold` a direct call per level, at it the rest of
  // the value on the Cont trampoline — Json's own road (encC/decC below),
  // so depth costs heap, not native stack (stack-safety-py-r).

  private def enc[X](s: Schema[X], x: X, depth: Int): PyValue =
    if depth >= Codecs.NativeThreshold then reset(encC[X, PyValue](s, x))
    else encNative(s, x, depth)

  /** a sum's case as its dict, the case named in `type` */
  private def tagged(su: Schema.SSum[?], name: String, v: PyValue): PyValue = v match
    case Dict(kv) =>
      if kv.exists(_._1 == TypeField) then throw IllegalArgumentException(
        s"okay.foreign: case $name of ${su.name} has a field named '$TypeField', which names the case on the wire")
      Dict((TypeField -> Str(name)) +: kv)
    case other => Dict(Vector(TypeField -> Str(name), "value" -> other))

  /** the leaves, shared by both roads: a scalar has no children, so this
   * is where every recursion ends */
  private def encScalar[X](s: Schema[X], x: X): PyValue = s match
    case Schema.SInt => I64(x.toLong)
    case Schema.SLong => I64(x)
    case Schema.SDouble => F64(x)
    case Schema.SBool => Bool(x)
    case Schema.SString => Str(x)
    case Schema.SChar => Str(x.toString)
    case Schema.SBytes => Bytes(x)
    case Schema.SBigInt => if x.isValidLong then I64(x.toLong) else BigI(x)
    case other => throw IllegalStateException(s"okay.foreign: not a scalar schema: $other")

  private def encNative[X](s: Schema[X], x: X, depth: Int): PyValue = s match
    case Schema.SInt | Schema.SLong | Schema.SDouble | Schema.SBool | Schema.SString
       | Schema.SChar | Schema.SBytes | Schema.SBigInt => encScalar(s, x)
    case o: Schema.SOption[a] => x match
      case Some(v) => enc(o.of(), v, depth + 1)
      case None => PyNone
    case l: Schema.SList[a] => Arr(x.iterator.map(enc(l.of(), _, depth + 1)).toVector)
    case v: Schema.SVector[a] => Arr(x.map(enc(v.of(), _, depth + 1)))
    case p: Schema.SProduct[X] =>
      Dict(p.eachField(x)([Y] => (name: String, sc: Schema[Y], y: Y) => (name, enc(sc, y, depth + 1))))
    case su: Schema.SSum[X] =>
      su.theCase(x)([Y <: X] => (name: String, sc: Schema[Y], y: Y) => tagged(su, name, enc(sc, y, depth + 1)))
    case i: Schema.SIso[X, b] => enc(i.under(), i.from(x), depth + 1)

  /** a field's schema and value at ONE type, held for the trampoline:
   * `eachField` hands them over erased, and this keeps them paired
   * without a cast (the typed-pair helper of no-casts-without-necessity) */
  private final class Held[Y](sc: Schema[Y], y: Y):
    def encC[R]: PyValue /> R = PyCodec.encC(sc, y)

  private def encC[X, R](s: Schema[X], x: X): PyValue /> R = s match
    case Schema.SInt | Schema.SLong | Schema.SDouble | Schema.SBool | Schema.SString
       | Schema.SChar | Schema.SBytes | Schema.SBigInt => Cont.Pure(encScalar(s, x))
    case o: Schema.SOption[a] => x match
      case Some(v) => Cont.delay(() => encC(o.of(), v))
      case None => Cont.Pure(PyNone)
    case l: Schema.SList[a] => encAll(l.of(), x.toVector)
    case v: Schema.SVector[a] => encAll(v.of(), x)
    case p: Schema.SProduct[X] =>
      val held = p.eachField(x)([Y] => (name: String, sc: Schema[Y], y: Y) => (name, Held(sc, y): Held[?]))
      def loop(rest: List[(String, Held[?])], acc: Vector[(String, PyValue)]): PyValue /> R = rest match
        case Nil => Cont.Pure(Dict(acc))
        case (name, h) :: more => Cont.defer(() => h.encC[R])(v => loop(more, acc :+ (name -> v)))
      loop(held.toList, Vector.empty)
    case su: Schema.SSum[X] =>
      val (name, h) = su.theCase(x)([Y <: X] => (name: String, sc: Schema[Y], y: Y) => (name, Held(sc, y): Held[?]))
      Cont.defer(() => h.encC[R])(v => Cont.Pure(tagged(su, name, v)))
    case i: Schema.SIso[X, b] => Cont.delay(() => encC(i.under(), i.from(x)))

  private def encAll[Y, R](sc: Schema[Y], xs: Vector[Y]): PyValue /> R =
    def loop(i: Int, acc: Vector[PyValue]): PyValue /> R =
      if i >= xs.length then Cont.Pure(Arr(acc))
      else Cont.defer(() => encC[Y, R](sc, xs(i)))(v => loop(i + 1, acc :+ v))
    loop(0, Vector.empty)

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
        case Field(p, n) => parts += s".$n"; cur = p
        case Index(p, i) => parts += s"[${i}]"; cur = p
        case Root => ()
      parts.reverseIterator.mkString
    /** the path in a message: "the value" at the root */
    def where: String = if this == Root then "the value" else render

  /** a value in a message without walking it: a deep one would recurse
   * in its own toString */
  private def describe(v: PyValue): String = v match
    case Arr(xs) => s"a list of ${xs.length}"
    case Dict(kv) => s"a dict of ${kv.length} keys"
    case other => other.toString

  private def dec[X](s: Schema[X], v: PyValue, at: At, depth: Int): Either[String, X] =
    if depth >= Codecs.NativeThreshold then reset(decC[X, Either[String, X]](s, v, at))
    else decNative(s, v, at, depth)

  /** the leaves, shared by both roads: a scalar has no children, so this
   * is where every recursion ends */
  private def decScalar[X](s: Schema[X], v: PyValue, at: At): Either[String, X] =
    def no(what: String): Either[String, X] = Left(s"${at.where}: expected $what, got ${describe(v)}")
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
      case other => Left(s"${at.where}: not a scalar schema: $other")

  private def decNative[X](s: Schema[X], v: PyValue, at: At, depth: Int): Either[String, X] =
    def no(what: String): Either[String, X] = Left(s"${at.where}: expected $what, got ${describe(v)}")
    s match
      case Schema.SInt | Schema.SLong | Schema.SDouble | Schema.SBool | Schema.SString
         | Schema.SChar | Schema.SBytes | Schema.SBigInt => decScalar(s, v, at)
      case o: Schema.SOption[a] => v match
        case PyNone => Right(None)
        case other => dec(o.of(), other, at, depth + 1).map(Some(_))
      case l: Schema.SList[a] => v match
        case Arr(xs) => each(xs, at)(dec(l.of(), _, _, depth + 1)).map(_.toList)
        case _ => no("a list")
      case sv: Schema.SVector[a] => v match
        case Arr(xs) => each(xs, at)(dec(sv.of(), _, _, depth + 1))
        case _ => no("a list")
      case p: Schema.SProduct[X] => v match
        case Dict(kv) => product(p, kv.toMap, at, depth)
        case _ => no(s"a dict for ${p.name}")
      case su: Schema.SSum[X] => v match
        case Dict(kv) => caseOf(su, kv, at) match
          case Left(e) => Left(e)
          case Right((sc, rest)) => dec(sc, rest, at, depth + 1)
        case _ => no(s"a dict for ${su.name}")
      case i: Schema.SIso[X, b] =>
        dec(i.under(), v, at, depth + 1).flatMap(u => i.to(u).left.map(why => s"${at.where}: $why"))

  /** the case a sum's dict names, and the dict it decodes from: the
   * fields without the tag for a product case, `value` otherwise */
  private def caseOf[X](su: Schema.SSum[X], kv: Vector[(String, PyValue)], at: At): Either[String, (Schema[? <: X], PyValue)] =
    def no(what: String) = Left(s"${at.where}: expected $what, got ${describe(Dict(kv))}")
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
            Right((sc, rest))
      case _ => no(s"a dict with a '$TypeField' naming a case of ${su.name}")

  /** the road past the threshold: the same decisions as `decNative`,
   * each child a `Cont.defer` (okay-codec's Edn.decodeC, over PyValue) */
  private def decC[X, R](s: Schema[X], v: PyValue, at: At): Either[String, X] /> R =
    def no(what: String): Either[String, X] = Left(s"${at.where}: expected $what, got ${describe(v)}")
    s match
      case Schema.SInt | Schema.SLong | Schema.SDouble | Schema.SBool | Schema.SString
         | Schema.SChar | Schema.SBytes | Schema.SBigInt => Cont.Pure(decScalar(s, v, at))
      case o: Schema.SOption[a] => v match
        case PyNone => Cont.Pure(Right(None))
        case other => Cont.defer(() => decC[a, R](o.of(), other, at))(r => Cont.Pure(r.map(Some(_))))
      case l: Schema.SList[a] => v match
        case Arr(xs) => decAll[a, R](l.of(), xs, at).flatMap(r => Cont.Pure(r.map(_.toList)))
        case _ => Cont.Pure(no("a list"))
      case sv: Schema.SVector[a] => v match
        case Arr(xs) => decAll[a, R](sv.of(), xs, at)
        case _ => Cont.Pure(no("a list"))
      case p: Schema.SProduct[X] => v match
        case Dict(kv) =>
          val m = kv.toMap
          // a field decoded inside Cont.defer continues from its continuation,
          // a call that cannot be a jump; `again` takes it, so this stays a loop
          def again(i: Int, acc: Vector[Any]): Either[String, Vector[Any]] /> R = loop(i, acc)
          @scala.annotation.tailrec def loop(i: Int, acc: Vector[Any]): Either[String, Vector[Any]] /> R =
            if i >= p.fields.length then Cont.Pure(Right(acc))
            else
              val (name, sc) = p.fields(i)
              val here = At.Field(at, name)
              m.get(name) match
                case Some(fv) => Cont.defer(() => fieldC[R](sc(), fv, here)) {
                  case Left(e) => Cont.Pure(Left(e))
                  case Right(x) => again(i + 1, acc :+ x)
                }
                case None => absent(p, i, sc, here) match
                  case Left(e) => Cont.Pure(Left(e))
                  case Right(d) => loop(i + 1, acc :+ d)
          loop(0, Vector.empty).flatMap(r => Cont.Pure(r.map(p.make)))
        case _ => Cont.Pure(no(s"a dict for ${p.name}"))
      case su: Schema.SSum[X] => v match
        case Dict(kv) => caseOf(su, kv, at) match
          case Left(e) => Cont.Pure(Left(e))
          // the case's schema at its own type c <: X, so the Cont-wrapped
          // answer can be restated at X (Cont is invariant, Either is not)
          case Right((sc, rest)) => sc match
            case sc: Schema[c] => Cont.defer(() => decC[c, R](sc, rest, at))(r => Cont.Pure(r: Either[String, X]))
        case _ => Cont.Pure(no(s"a dict for ${su.name}"))
      case i: Schema.SIso[X, b] =>
        Cont.defer(() => decC[b, R](i.under(), v, at))(r =>
          Cont.Pure(r.flatMap(u => i.to(u).left.map(why => s"${at.where}: $why"))))

  private def fieldC[R](sc: Schema[?], v: PyValue, at: At): Either[String, Any] /> R = sc match
    case sc: Schema[y] => Cont.defer(() => decC[y, R](sc, v, at))(r => Cont.Pure(r: Either[String, Any]))

  private def decAll[Y, R](sc: Schema[Y], xs: Vector[PyValue], at: At): Either[String, Vector[Y]] /> R =
    def loop(i: Int, acc: Vector[Y]): Either[String, Vector[Y]] /> R =
      if i >= xs.length then Cont.Pure(Right(acc))
      else Cont.defer(() => decC[Y, R](sc, xs(i), At.Index(at, i))) {
        case Left(e) => Cont.Pure(Left(e))
        case Right(y) => loop(i + 1, acc :+ y)
      }
    loop(0, Vector.empty)

  /** a field the dict does not carry: its default, None for an option, or
   * named as missing */
  private def absent(p: Schema.SProduct[?], i: Int, sc: () => Schema[?], here: At): Either[String, Any] =
    p.defaultAt(i)([Y] => (_: Schema[Y], d: Y) => d: Any) match
      case Some(d) => Right(d)
      case None => sc() match
        case _: Schema.SOption[?] => Right(None)
        case _ => Left(s"${here.render}: missing")

  private def each[Y](xs: Vector[PyValue], at: At)(f: (PyValue, At) => Either[String, Y]): Either[String, Vector[Y]] =
    val out = Vector.newBuilder[Y]
    var i = 0
    var bad: Option[String] = None
    while bad.isEmpty && i < xs.length do
      f(xs(i), At.Index(at, i)) match
        case Right(y) => out += y
        case Left(e) => bad = Some(e)
      i += 1
    bad.toLeft(out.result())

  private def product[X](p: Schema.SProduct[X], m: Map[String, PyValue], at: At, depth: Int): Either[String, X] =
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

/**
 * A Python function as a typed Scala function (foreign-typed-calls):
 *
 * {{{
 * val median = Py.fn[Double]("statistics:median")
 * median(Vector(3.0, 1.0, 2.0))   // Either[Condition, Double] ! ForeignEval
 * }}}
 *
 * The arguments are encoded through their `Schema`, the answer decoded
 * through `Out`'s; a Python exception and an answer of the wrong shape
 * are both a `Left(Condition)`, so a caller matches one channel. The
 * result is an okay program over `ForeignEval`, run by whichever handler is
 * installed — a subprocess, a worker pool, a canned mock, or `Durable`
 * over any of them.
 */
object Py {
  // every entry below takes the caller's `Shape` (Python's unless one is
  // given): the value rules are the CALL SITE's, so the same API serves
  // R at `R.shape` and TypeScript at `Shape.json` (foreign-one-value)
  def fn[Out](address: String)(using Schema[Out], Shape): Fn[Out] = Fn(address)

  /**
   * A Python PROGRAM-AS-DATA (remote-foreign, specs/remote-foreign.md): the
   * function returns `okay.done(v)` or `okay.perform(name, ...).then(f)`,
   * each name a callback of `cbs`. The far side keeps every continuation
   * of the run by id, so a handler that resumes twice (`Choice`) continues
   * the same pure Python function twice — multi-shot across a process. The
   * run holds its continuations until `forget`.
   */
  def program[Out: Schema](address: String)(using Shape): ProgramOf[Out] = ProgramOf(address)

  private val runIds = java.util.concurrent.atomic.AtomicLong()

  final class ProgramOf[Out: Schema](address: String)(using shape: Shape):
    def calling[F[+_]](cbs: Callbacks[F]): Starting[F] = Starting(cbs)

    final class Starting[F[+_]](cbs: Callbacks[F]):
      def apply(): PyRun[F, Out] = PyRun(runIds.incrementAndGet(), address, Vector.empty, cbs)
      def apply[A: ToPy](a: A): PyRun[F, Out] = PyRun(runIds.incrementAndGet(), address, Vector(ToPy(a)), cbs)
      def apply[A: ToPy, B: ToPy](a: A, b: B): PyRun[F, Out] =
        PyRun(runIds.incrementAndGet(), address, Vector(ToPy(a), ToPy(b)), cbs)

  /** one run of a program-as-data: the okay program that walks it, and
   * the release of the continuations the far side keeps for it */
  final class PyRun[F[+_], Out: Schema](val id: Long, address: String, args: Vector[PyValue], cbs: Callbacks[F])(using shape: Shape):
    type R = F + ForeignEval

    /** walk the far program node by node; each named operation is a
     * callback of `cbs`, run under the caller's handlers */
    def program: Either[Condition, Out] ! R =
      def step(e: Either[Condition, PyNode]): Either[Condition, Out] ! R = e match
        case Left(c) => pure[R, Either[Condition, Out]](Left(c))
        case Right(PyNode.Done(v)) => pure[R, Either[Condition, Out]](shape.decode[Out](v))
        case Right(PyNode.Perform(name, as, k, _)) => cbs.get(name) match
          case None => pure[R, Either[Condition, Out]](Left(Condition("NoCallback",
            s"'$name' is not among this program's callbacks (${cbs.names.mkString(", ")})")))
          case Some(cb) => cb.run(as).plus[ForeignEval].flatMap {
            case Left(c) => pure[R, Either[Condition, Out]](Left(c))
            case Right(a) => effect[R, Either[Condition, PyNode]](ForeignEval.Continue(id, k, Right(a))).flatMap(step)
          }
      effect[R, Either[Condition, PyNode]](ForeignEval.Program(id, address, args, cbs.names)).flatMap(step)

    /** drop every continuation the far side keeps for this run */
    def forget: Unit ! ForeignEval = effect[ForeignEval, Unit](ForeignEval.Forget(id))

  /**
   * A Python GENERATOR as an okay source (foreign-one-mux): `address` is a
   * function returning an iterator whose items are CHUNKS (lists of `O`);
   * each chunk is one call of its `__next__`, `StopIteration` its end.
   * `Py.releasing(through(Py.source[Row]("m:rows")(path))(stage))` reads a
   * far-side file, cursor or generator at the consumer's pace, and gives
   * the iterator back however the consumer ended (foreign-source-early-stop).
   */
  def source[O: Schema](address: String)(using shape: Shape): SourceOf[O] = SourceOf(address)

  /**
   * A STREAM the far side drives (foreign-mux-duplex part 3): `address`'s
   * function sends chunks (lists of `O`) as it makes them, up to `credit`
   * ahead of the consumer. Run it as a source, inside `Py.releasing`:
   * `Writer.run(Py.releasing(Py.stream[Long]("numbers", credit = 2)(100L)))`.
   * Go and Rust workers on a multiplexed wire stream; others refuse by name.
   */
  def stream[O: Schema](address: String, credit: Int = 4)(using shape: Shape): StreamOf[O] = StreamOf(address, credit)

  final class StreamOf[O: Schema](address: String, credit: Int)(using shape: Shape):
    def apply(): Unit ! PyStream.SourceRow[O] = go(Vector.empty)
    def apply[A: ToPy](a: A): Unit ! PyStream.SourceRow[O] = go(Vector(ToPy(a)))
    def apply[A: ToPy, B: ToPy](a: A, b: B): Unit ! PyStream.SourceRow[O] = go(Vector(ToPy(a), ToPy(b)))
    def apply[A: ToPy, B: ToPy, C: ToPy](a: A, b: B, c: C): Unit ! PyStream.SourceRow[O] = go(Vector(ToPy(a), ToPy(b), ToPy(c)))
    private def go(args: Vector[PyValue]): Unit ! PyStream.SourceRow[O] =
      PyStream.driven[O](address, args, credit, () => runIds.incrementAndGet())

  /** the scope a source runs in: whatever it still holds when the program
   * ends — a consumer that stopped early — is released
   * (foreign-source-early-stop; `PyStream.releasing`) */
  def releasing[A, O](p: A ! PyStream.SourceRow[O]): A ! PyStream.Released[O] = PyStream.releasing(p)

  final class SourceOf[O: Schema](address: String)(using shape: Shape):
    def apply(): Unit ! PyStream.SourceRow[O] = go(Vector.empty)
    def apply[A: ToPy](a: A): Unit ! PyStream.SourceRow[O] = go(Vector(ToPy(a)))
    def apply[A: ToPy, B: ToPy](a: A, b: B): Unit ! PyStream.SourceRow[O] = go(Vector(ToPy(a), ToPy(b)))
    private def go(args: Vector[PyValue]): Unit ! PyStream.SourceRow[O] =
      PyStream.pulled[O](ForeignEval.Call(address, args, held = true),
        r => ForeignEval.Call(Address.Method(r, "__next__"), Vector.empty),
        _.kind == "StopIteration", _ => false)

  /** a Python function over a LIST as an okay stage over chunks
   * (foreign-streaming): see `PyStream` */
  def stage[I: ToPy, O: Schema](address: String, chunk: Int = 64): Unit ! PyStream.Row[I, O] =
    PyStream.chunked[I, O](chunk, buf => ForeignEval.Call(address, Vector(PyValue.Arr(buf))), None)

  /**
   * Python source beside the Scala that calls it (foreign-inline-modules):
   * the source must be a compile-time constant, and the engine ships it
   * when a worker starts — `ForeignWorker.start(..., modules = Seq(m))`.
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
  def hold(address: String)(using Shape): Hold = Hold(address)

  final class Hold(address: String)(using shape: Shape):
    def apply(): Either[Condition, PyRef] ! ForeignEval = go(Vector.empty)
    def apply[A: ToPy](a: A): Either[Condition, PyRef] ! ForeignEval = go(Vector(ToPy(a)))
    def apply[A: ToPy, B: ToPy](a: A, b: B): Either[Condition, PyRef] ! ForeignEval = go(Vector(ToPy(a), ToPy(b)))
    def apply[A: ToPy, B: ToPy, C: ToPy](a: A, b: B, c: C): Either[Condition, PyRef] ! ForeignEval =
      go(Vector(ToPy(a), ToPy(b), ToPy(c)))
    private def go(args: Vector[PyValue]): Either[Condition, PyRef] ! ForeignEval =
      effect[ForeignEval, Either[Condition, PyValue]](ForeignEval.Call(address, args, held = true))
        .map(_.flatMap(Wire.asRef).map(_.copy(shape = shape)))

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
  def callback[Arg: Schema, Res: Schema](name: String)(using Shape): CallbackOf[Arg, Res] = CallbackOf(name)

  /** the callbacks one call offers, all in one row `F` (a union row for
   * several effects: callbacks are programs, and a program has one row) */
  def callbacks[F[+_]](cbs: Callback[F]*): Callbacks[F] = Callbacks(cbs.toVector)

  final class CallbackOf[Arg: Schema, Res: Schema](name: String)(using shape: Shape):
    def apply[F[+_]](f: Arg => Res ! F): Callback[F] = Callback(name, args =>
      val in = args match
        case Vector(one) => shape.decode[Arg](one)
        case many => shape.decode[Arg](PyValue.Arr(many))
      in match
        case Left(c) => pure[F, Either[Condition, PyValue]](Left(c))
        case Right(i) => f(i).map(o => Right(shape.encode(o))),
      Some((summon[Schema[Arg]], summon[Schema[Res]])))

  /** `types`: the argument's and the answer's Schemas, when the callback was
   * made by `callback[Arg, Res]` — what `Ts.ops` writes a TypeScript
   * signature from (typescript-types T12) */
  final class Callback[F[+_]](val name: String, val run: Vector[PyValue] => Either[Condition, PyValue] ! F,
                              val types: Option[(Schema[?], Schema[?])] = None)

  final class Callbacks[F[+_]](val all: Vector[Callback[F]]):
    def names: Vector[String] = all.map(_.name)
    def get(name: String): Option[Callback[F]] = all.find(_.name == name)

  final class Fn[Out](val address: String)(using out: Schema[Out], shape: Shape):
    def apply(): Either[Condition, Out] ! ForeignEval = call(Vector.empty)
    def apply[A: ToPy](a: A): Either[Condition, Out] ! ForeignEval =
      call(Vector(ToPy(a)))
    def apply[A: ToPy, B: ToPy](a: A, b: B): Either[Condition, Out] ! ForeignEval =
      call(Vector(ToPy(a), ToPy(b)))
    def apply[A: ToPy, B: ToPy, C: ToPy](a: A, b: B, c: C): Either[Condition, Out] ! ForeignEval =
      call(Vector(ToPy(a), ToPy(b), ToPy(c)))
    def apply[A: ToPy, B: ToPy, C: ToPy, D: ToPy](a: A, b: B, c: C, d: D): Either[Condition, Out] ! ForeignEval =
      call(Vector(ToPy(a), ToPy(b), ToPy(c), ToPy(d)))

    private def call(args: Vector[PyValue]): Either[Condition, Out] ! ForeignEval =
      effect[ForeignEval, Either[Condition, PyValue]](ForeignEval.Call(address, args))
        .map(_.flatMap(shape.decode[Out](_)))

    /** this function, offered `cbs` to call back into (foreign-callbacks) */
    def calling[F[+_]](cbs: Callbacks[F]): Calling[F] = Calling(cbs)

    final class Calling[F[+_]](cbs: Callbacks[F]):
      def apply(): Either[Condition, Out] ! F + ForeignEval = dialogue(Vector.empty)
      def apply[A: ToPy](a: A): Either[Condition, Out] ! F + ForeignEval =
        dialogue(Vector(ToPy(a)))
      def apply[A: ToPy, B: ToPy](a: A, b: B): Either[Condition, Out] ! F + ForeignEval =
        dialogue(Vector(ToPy(a), ToPy(b)))
      def apply[A: ToPy, B: ToPy, C: ToPy](a: A, b: B, c: C): Either[Condition, Out] ! F + ForeignEval =
        dialogue(Vector(ToPy(a), ToPy(b), ToPy(c)))

      /**
       * The call as a PROGRAM (foreign-one-program): started under a run of
       * its own, each `okay_call` a node whose callback runs under the
       * caller's handlers and whose answer — or failure, raised in the far
       * side's code where it may be caught — continues it, until the
       * function answers. Each step is an okay node, so a function that
       * calls back a million times is a loop, not a million frames.
       */
      private def dialogue(args: Vector[PyValue]): Either[Condition, Out] ! F + ForeignEval =
        type R = F + ForeignEval
        val run = runIds.incrementAndGet()
        def go(node: Either[Condition, PyNode]): Either[Condition, Out] ! R = node match
          case Left(c) => pure[R, Either[Condition, Out]](Left(c))
          case Right(PyNode.Done(v)) => pure[R, Either[Condition, Out]](shape.decode[Out](v))
          case Right(PyNode.Perform(name, as, k, _)) =>
            val answered: Either[Condition, PyValue] ! R = cbs.get(name) match
              case Some(cb) => cb.run(as).plus[ForeignEval]
              case None => pure[R, Either[Condition, PyValue]](Left(Condition("NoCallback",
                s"'$name' is not among this call's callbacks (${cbs.names.mkString(", ")})")))
            answered.flatMap(a => effect[R, Either[Condition, PyNode]](ForeignEval.Continue(run, k, a))).flatMap(go)
        effect[R, Either[Condition, PyNode]](ForeignEval.Program(run, address, args, cbs.names, direct = true)).flatMap(go)
}
