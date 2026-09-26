package okay.r

import okay.!
import okay.codec.Schema
import okay.foreign.{ForeignEval, Foreign, PyFrame, PyNode, PyRef, PyStream, PyValue, Shape, ToPy}

/*
 * R as a handler (specs/r.md): calls are OPERATIONS — journalled by
 * Durable, mockable by handler swap, supervised. Named functions only:
 * there is deliberately NO operation that evals a string, so untrusted
 * input reaches R only as data.
 *
 * Since foreign-one-value (specs/foreign-one.md, stage 2a) R has no value
 * tree, effect, API or engine of its own: an R value IS the one tree
 * (`PyValue`, which carries R's typed NA), an R call IS a `ForeignEval`, and
 * `R` is the one API at R's value rules (`R.shape`). What stays R's is what
 * is R's: the NAMES an R user thinks in (a vector, NULL, a typed NA, a named
 * list), the rules a Scala value becomes an R value by (`RCodec`: no
 * scalars, no 64-bit integer, NA per type), and a handle to a held R object.
 */

/** an R value: the one value tree (foreign-one-value), in R's names — see
 * `object RValue` */
type RValue = PyValue

/**
 * R's names for the one value tree. `RNull` is R's NULL (the absence of an
 * OBJECT), `NA(t)` a missing value INSIDE a vector and TYPED
 * (`NA_integer_`, `NA_real_`, `NA_character_`, logical `NA` are four values;
 * `mean(c(1, NA))` is NA and `mean(c(1, NULL))` is 1), `I32` R's integer
 * (32-bit), `Vec` a vector or list, `Named` a named list that is not a
 * data.frame (a record).
 */
object RValue:
  val RNull: PyValue.PyNone.type = PyValue.PyNone
  val Bool: PyValue.Bool.type = PyValue.Bool
  val F64: PyValue.F64.type = PyValue.F64
  val Str: PyValue.Str.type = PyValue.Str
  val Bytes: PyValue.Bytes.type = PyValue.Bytes
  val Vec: PyValue.Arr.type = PyValue.Arr
  val Named: PyValue.Dict.type = PyValue.Dict

  /** R's integer: 32 bits, an integral number on the wire */
  object I32:
    def apply(v: Int): PyValue = PyValue.I64(v.toLong)
    def unapply(v: PyValue): Option[Int] = v match
      case PyValue.I64(n) if n.isValidInt => Some(n.toInt)
      case _ => None

  /** a missing value of an R type */
  object NA:
    def apply(of: RType): PyValue = PyValue.NA(of.rName)
    def unapply(v: PyValue): Option[RType] = v match
      case PyValue.NA(t) => Some(RType.byName(t).getOrElse(RType.Logical))
      case _ => None

  /** an object held in the R process, as an argument */
  object Ref:
    def apply(r: RRef): PyValue = PyValue.Ref(r.py)
    def unapply(v: PyValue): Option[RRef] = v match
      case PyValue.Ref(p) => Some(RRef.of(p))
      case _ => None

/**
 * A handle to an R object kept in its process (foreign-object-handles).
 * R applies functions TO objects rather than calling methods on them, so
 * a ref is used as an argument: `R.fn[Vector[Double]]("stats::predict")(fit, newdata)`.
 * It names state in ONE process, so a recovery onto a fresh process meets a
 * ref it never held and is refused by name. On the wire it is the one
 * handle (`PyRef`), read by R's rules.
 */
final case class RRef(id: Long, rClass: String):
  private[r] def py: PyRef = PyRef(id, rClass, R.shape)
  /** drop the object in the R process; idempotent */
  def release: Unit ! REval = okay.effect[REval, Unit](ForeignEval.Release(py))
  /** this held CLOSURE as a stateful stage over chunks (foreign-streaming);
   * `finish`, another held closure, is called with nothing at the end */
  def stage[I: ToR, O: Schema](chunk: Int = 64, finish: Option[RRef] = None): Unit ! RStream.Row[I, O] =
    given Shape = R.shape
    PyStream.chunked[I, O](chunk, buf => RStream.viaClosure(this, Vector(RValue.Vec(buf))),
      finish.map(f => RStream.viaClosure(f, Vector.empty)))

object RRef:
  def of(p: PyRef): RRef = RRef(p.id, p.pyType)
  /** a handle is an argument like any other */
  given ToPy[RRef] with
    def py(a: RRef)(using Shape): PyValue = PyValue.Ref(a.py)

/** how an argument becomes an R value: through its `Schema` by R's rules,
 * or as the handle it is — the one `ToPy`, at `R.shape` */
type ToR[A] = ToPy[A]

object ToR:
  def apply[A](a: A)(using t: ToPy[A]): PyValue = t.py(a)(using R.shape)

enum RType:
  case Logical, Integer, Double, Character

  def rName: String = this match
    case Logical => "logical"
    case Integer => "integer"
    case Double => "double"
    case Character => "character"

object RType:
  def byName(s: String): Option[RType] =
    RType.values.find(_.rName == s)

/** a data.frame as columns: the one frame, read by R's rules */
type RFrame = PyFrame

object RFrame:
  /** a frame of these columns, read by R's rules */
  def apply(cols: Vector[(String, Vector[PyValue])]): PyFrame = PyFrame(cols, R.shape)

  /** every row, decoded at `A` by R's rules — fields match columns BY
   * NAME, and a mismatch either way is a `Condition` naming it */
  def rowsOf[A](f: PyFrame)(using Schema[A]): Either[Condition, Vector[A]] = R.shape.rows[A](f)

  /** the rows as a frame: the field order IS the column order */
  def of[A](rows: Seq[A])(using Schema[A]): Either[Condition, PyFrame] = R.shape.frame(rows)

  /** its columns, whatever rules read it */
  def unapply(f: PyFrame): Some[Vector[(String, Vector[PyValue])]] = Some(f.cols)

/**
 * R's view of the one wire codec (`okay.foreign.Wire`): a value's tree, and a
 * frame in the COLUMNAR shape R's shim reads fastest, read back by R's
 * rules. There is no second codec behind it (foreign-one-value).
 */
private[r] object Wire:
  def enc(v: PyValue): okay.codec.Json = okay.foreign.Wire.enc(v)
  def dec(j: okay.codec.Json): PyValue = okay.foreign.Wire.dec(j)
  def encFrame(f: PyFrame): okay.codec.Json = okay.foreign.Wire.encFrameColumnar(f)
  def decFrame(j: okay.codec.Json): Either[Condition, PyFrame] = okay.foreign.Wire.decFrame(j).map(_.ruledBy(R.shape))

/** what a failing call answers: the R condition's class and its message —
 * data, and the process survives to take the next call */
type Condition = okay.foreign.Condition
val Condition: okay.foreign.Condition.type = okay.foreign.Condition

/** the effect an R call is: the one foreign effect (foreign-one-value) */
type REval[+A] = ForeignEval[A]
val REval: ForeignEval.type = ForeignEval

/** one node of an R program-as-data (remote-foreign) */
type RNode = PyNode
val RNode: PyNode.type = PyNode

/**
 * R's VALUE RULES as a `Shape` (foreign-one-value): a Scala value becomes
 * an R value by `RCodec` (a product is a named list, `None` a typed NA
 * where R has a type for the field, a `Long` an integer, an exact double
 * or its digits), and a frame's rows by name, NA and NULL both absent.
 */
object RShape extends Shape:
  def encode[A](a: A)(using Schema[A]): PyValue = RCodec.encode(a)
  def decode[A](v: PyValue)(using Schema[A]): Either[Condition, A] = RCodec.decode[A](v)

  override def rows[A](f: PyFrame)(using s: Schema[A]): Either[Condition, Vector[A]] =
    product(s).flatMap { p =>
      val names = p.fields.map(_._1)
      val cols = f.cols.toMap
      val extra = f.cols.map(_._1).filterNot(names.contains)
      val missing = names.filterNot(cols.contains)
      if extra.nonEmpty then
        Left(Condition("FrameSchema",
          s"column '${extra.head}' is not a field of ${p.name}" +
            (if extra.length > 1 then s" (nor ${extra.tail.mkString("'", "', '", "'")})" else "")))
      else if missing.nonEmpty then
        Left(Condition("FrameSchema",
          s"${p.name} names field '${missing.head}', and the frame has no such column"))
      else
        val height = f.cols.headOption.map(_._2.length).getOrElse(0)
        var bad: Option[Condition] = None
        val out = Vector.tabulate(height) { i =>
          val parts = p.fields.map { (name, sc) =>
            cell(sc(), cols(name)(i)) match
              case Right(v) => v
              case Left(why) =>
                if bad.isEmpty then bad = Some(Condition("FrameSchema", s"column '$name', row $i: $why"))
                null
          }
          p.make(parts)
        }
        bad.toLeft(out)
    }

  override def frame[A](rows: Seq[A])(using s: Schema[A]): Either[Condition, PyFrame] =
    product(s).map { p =>
      val cells = rows.toVector.map(a => p.parts(a).toVector)
      PyFrame(p.fields.zipWithIndex.map { case ((name, sc), i) =>
        name -> cells.map(row => encodeCell(sc(), row(i)))
      }, this)
    }

  private def product[A](s: Schema[A]): Either[Condition, Schema.SProduct[A]] = s match
    case p: Schema.SProduct[A] => Right(p)
    case other => Left(Condition("FrameSchema", s"a frame row is a flat case class; this Schema is $other"))

  /** an R cell at a field's type. R has no 64-bit integer and no nesting
   * inside a data.frame column, so the vocabulary is small and stated: the
   * four scalars, raw bytes, and Option for NA/NULL. */
  private def cell[X](s: Schema[X], v: PyValue): Either[String, Any] =
    import RValue.*
    (s, v) match
      case (_: Schema.SOption[?], NA(_) | RNull) => Right(None)
      case (o: Schema.SOption[?], other) => cell(o.of(), other).map(Some(_))
      case (Schema.SInt, I32(x)) => Right(x)
      case (Schema.SLong, I32(x)) => Right(x.toLong)
      // R has no 64-bit integer: a Long crossed as an exact double or as its digits
      case (Schema.SLong, F64(x)) if x == math.floor(x) && math.abs(x) <= RCodec.Exact => Right(x.toLong)
      case (Schema.SLong, Str(x)) if x.toLongOption.isDefined => Right(x.toLong)
      case (Schema.SDouble, F64(x)) => Right(x)
      case (Schema.SDouble, I32(x)) => Right(x.toDouble)
      case (Schema.SBool, Bool(x)) => Right(x)
      case (Schema.SString, Str(x)) => Right(x)
      case (Schema.SBytes, Bytes(x)) => Right(x)
      // R has no integer past 32 bits: a BigInt travels as its digits (schema-bigint)
      case (Schema.SBigInt, Str(x)) =>
        scala.util.Try(BigInt(x)).toOption.toRight(s"not an integer: \"$x\"")
      case (Schema.SBigInt, I32(x)) => Right(BigInt(x))
      case (Schema.SIso(under, to, _), other) =>
        cell(under(), other).flatMap(u => to(u.asInstanceOf) match
          case Right(a) => Right(a)
          case Left(why) => Left(why))
      case (sc, other) => Left(s"$other does not fit $sc")

  @scala.annotation.tailrec private def encodeCell[X](s: Schema[X], v: Any): PyValue =
    import RValue.*
    (s, v) match
      case (o: Schema.SOption[?], None) => naOf(o.of())
      case (o: Schema.SOption[?], Some(x)) => encodeCell(o.of(), x)
      case (Schema.SInt, x: Int) => I32(x)
      // was `I32(x.toInt)`, which truncated any Long past 32 bits SILENTLY
      case (Schema.SLong, x: Long) => RCodec.long(x)
      case (Schema.SDouble, x: Double) => F64(x)
      case (Schema.SBool, x: Boolean) => Bool(x)
      case (Schema.SString, x: String) => Str(x)
      case (Schema.SBytes, x: Array[Byte]) => Bytes(x)
      case (Schema.SBigInt, x: BigInt) => Str(x.toString)
      case (Schema.SIso(under, _, from), x) => encodeCell(under(), from.asInstanceOf[Any => Any](x))
      case (_, other) => Str(String.valueOf(other))

  /** an absent cell keeps its COLUMN's type: R's four NAs are four
   * values, and a column of NA_character_ is not a logical column */
  private def naOf[X](s: Schema[X]): PyValue =
    import RValue.*
    s match
      case Schema.SInt | Schema.SLong => NA(RType.Integer)
      case Schema.SDouble => NA(RType.Double)
      case Schema.SString => NA(RType.Character)
      case Schema.SBool => NA(RType.Logical)
      case _ => RNull

/**
 * R functions from Scala — the one foreign API (`okay.foreign.Py`) at R's value
 * rules (foreign-one-value; before it, a copy of that API over `RValue`).
 *
 * {{{
 * val median = R.fn[Double]("stats::median")
 * median(Vector(3.0, 1.0, 2.0))   // Either[Condition, Double] ! REval
 * }}}
 */
object R:
  /** R's value rules: every call below encodes and decodes by them */
  given shape: Shape = RShape

  type Fn[Out] = Foreign.Fn[Out]
  type ProgramOf[Out] = Foreign.ProgramOf[Out]
  type RRun[F[+_], Out] = Foreign.PyRun[F, Out]
  type CallbackOf[Arg, Res] = Foreign.CallbackOf[Arg, Res]
  type Callback[F[+_]] = Foreign.Callback[F]
  type Callbacks[F[+_]] = Foreign.Callbacks[F]

  def fn[Out](address: String)(using Schema[Out]): Fn[Out] = Foreign.Fn(address)

  /** an R PROGRAM-AS-DATA (remote-foreign): the function returns
   * `okay_done(v)` or `okay_then(okay_perform(name, ...), f)`; R keeps each
   * continuation (a closure) by id, so a Choice handler continues one twice */
  def program[Out: Schema](address: String): ProgramOf[Out] = Foreign.ProgramOf(address)

  /** an R function over a vector as an okay stage over chunks
   * (foreign-streaming) */
  def stage[I: ToR, O: Schema](address: String, chunk: Int = 64): Unit ! RStream.Row[I, O] =
    PyStream.chunked[I, O](chunk, buf => ForeignEval.Call(address, Vector(RValue.Vec(buf))), None)

  /**
   * An R SOURCE (foreign-one-mux): `address` returns a CLOSURE, each call
   * of which answers the next chunk (a vector of `O`), and NULL at the end
   * — R's iterator, which R has no generators for. The closure is held on
   * the far side and called through `base::do.call`, one chunk per call.
   */
  def source[O: Schema](address: String): SourceOf[O] = SourceOf(address)

  /** the scope an R source runs in: what it still holds at the end — a
   * consumer that stopped early — is released (foreign-source-early-stop) */
  def releasing[A, O](p: A ! PyStream.SourceRow[O]): A ! PyStream.Released[O] = PyStream.releasing(p)

  final class SourceOf[O: Schema](address: String):
    def apply(): Unit ! PyStream.SourceRow[O] = go(Vector.empty)
    def apply[A: ToR](a: A): Unit ! PyStream.SourceRow[O] = go(Vector(ToR(a)))
    def apply[A: ToR, B: ToR](a: A, b: B): Unit ! PyStream.SourceRow[O] = go(Vector(ToR(a), ToR(b)))
    private def go(args: Vector[PyValue]): Unit ! PyStream.SourceRow[O] =
      PyStream.pulled[O](ForeignEval.Call(address, args, held = true),
        r => ForeignEval.Call("base::do.call", Vector(PyValue.Ref(r), PyValue.Arr(Vector.empty))),
        _ => false, v => v == PyValue.PyNone || v == PyValue.Arr(Vector.empty))

  /** R source beside the Scala that calls it (foreign-inline-modules):
   * a compile-time constant, shipped when R starts —
   * `RSubprocess.start(..., modules = Seq(m))` */
  inline def module(inline name: String, inline source: String): RModule =
    scala.compiletime.requireConst(name)
    scala.compiletime.requireConst(source)
    RModule.fromConstant(name, source)

  /** call `address` and KEEP its result in the R process, answering a
   * handle (foreign-object-handles): `R.hold("stats::lm")(formula, data)` */
  def hold(address: String): Hold = Hold(address)

  final class Hold(address: String):
    private val held = Foreign.Hold(address)
    def apply(): Either[Condition, RRef] ! REval = held().map(_.map(RRef.of))
    def apply[A: ToR](a: A): Either[Condition, RRef] ! REval = held(a).map(_.map(RRef.of))
    def apply[A: ToR, B: ToR](a: A, b: B): Either[Condition, RRef] ! REval = held(a, b).map(_.map(RRef.of))
    def apply[A: ToR, B: ToR, C: ToR](a: A, b: B, c: C): Either[Condition, RRef] ! REval =
      held(a, b, c).map(_.map(RRef.of))

  /** a callback R may call by name while okay runs one of its functions
   * (foreign-callbacks): `okay_call("objective", x)` in R decodes `x` as
   * `Arg` by R's rules, runs `f` under the caller's handlers, and answers */
  def callback[Arg: Schema, Res: Schema](name: String): CallbackOf[Arg, Res] = Foreign.CallbackOf(name)

  def callbacks[F[+_]](cbs: Callback[F]*): Callbacks[F] = Foreign.callbacks(cbs*)
