package okay.foreign

import okay.codec.{Json, Schema}

/**
 * How a Scala value LOOKS to the other side (typescript-types, T1): the
 * codec a call encodes its arguments with and decodes its answer with.
 *
 *  - `Shape.python` (the default): okay-py's `PyCodec` — a case class is a
 *    dict, a sum is its case's dict plus a `"type"` field; what a Python
 *    `TypedDict` (`Stubs.python`) describes.
 *  - `Shape.json`: okay's JSON codec — a sum is `{ "Case": {...} }`, `None`
 *    is null, a `BigInt` a string of digits; what `Stubs.typescript`
 *    describes, and what an HTTP client and okay-ts see. The `Ts` API uses
 *    it, so ONE TypeScript declaration serves all three scenarios.
 */
trait Shape:
  def encode[A](a: A)(using Schema[A]): PyValue
  def decode[A](v: PyValue)(using Schema[A]): Either[Condition, A]

  /** a frame as rows of a case class, a row being the dict of its cells —
   * this shape's value rules per cell (foreign-one-value: R's frame rules
   * are `okay.r`'s shape, not a frame type of their own) */
  def rows[A](f: PyFrame)(using Schema[A]): Either[Condition, Vector[A]] =
    val n = f.cols.headOption.fold(0)(_._2.length)
    f.cols.find(_._2.length != n) match
      case Some((name, c)) =>
        Left(Condition("FrameShape", s"column '$name' has ${c.length} cells, the first has $n"))
      case None =>
        val out = Vector.newBuilder[A]
        var bad: Option[Condition] = None
        var i = 0
        while bad.isEmpty && i < n do
          decode[A](PyValue.Dict(f.cols.map((k, c) => (k, c(i))))) match
            case Right(a) => out += a
            case Left(c) => bad = Some(c.copy(message = s"row $i: ${c.message}"))
          i += 1
        bad.toLeft(out.result())

  /** rows of a flat case class as a frame, a column per field */
  def frame[A](rows: Seq[A])(using s: Schema[A]): Either[Condition, PyFrame] = s match
    case p: Schema.SProduct[A] =>
      val names = p.fields.map(_._1)
      val cells = rows.toVector.map(encode(_) match
        case PyValue.Dict(kv) => kv.map(_._2)
        case other => Vector(other))
      Right(PyFrame(names.zipWithIndex.map((n, j) => (n, cells.map(_(j)))), this))
    case other => Left(Condition("FrameSchema", s"a frame row is a case class; this Schema is $other"))

object Shape:
  given python: Shape with
    def encode[A](a: A)(using Schema[A]): PyValue = PyCodec.encode(a)
    def decode[A](v: PyValue)(using Schema[A]): Either[Condition, A] = PyCodec.decode[A](v)

  /** okay's JSON codec's shape, carried on the wire as plain values */
  val json: Shape = new Shape:
    def encode[A](a: A)(using s: Schema[A]): PyValue = fromJson(Json.parse(Json.encode(s)(a)))
    def decode[A](v: PyValue)(using s: Schema[A]): Either[Condition, A] =
      Json.decode(s)(toJson(v)).left.map(Condition("Decode", _))

  private val Exact = 9007199254740992.0

  // both conversions bottom-up on an explicit stack (Walk.up): the Json a
  // worker sent is as deep as the worker made it (stack-safety-py-r)
  private def fromJson(j: Json): PyValue = Walk.up[Json, PyValue](j) {
    case Json.JArr(xs) => Right((xs, PyValue.Arr(_)))
    case Json.JObj(fs) => Right((fs.map(_._2), vs => PyValue.Dict(fs.map(_._1).zip(vs))))
    case Json.JNull | Json.JErr(_) => Left(PyValue.PyNone)
    case Json.JBool(b) => Left(PyValue.Bool(b))
    case Json.JNum(n) if n == math.floor(n) && !n.isInfinite && math.abs(n) < Exact => Left(PyValue.I64(n.toLong))
    case Json.JNum(n) => Left(PyValue.F64(n))
    case Json.JStr(s) => Left(PyValue.Str(s))
  }

  private def toJson(v: PyValue): Json = Walk.up[PyValue, Json](v) {
    case PyValue.Arr(xs) => Right((xs, Json.JArr(_)))
    case PyValue.Dict(kv) => Right((kv.map(_._2), vs => Json.JObj(kv.map(_._1).zip(vs))))
    case PyValue.PyNone => Left(Json.JNull)
    case PyValue.Bool(b) => Left(Json.JBool(b))
    case PyValue.I64(n) => Left(Json.JNum(n.toDouble))
    case PyValue.BigI(n) => Left(Json.JNum(n.toDouble))
    case PyValue.F64(d) => Left(Json.JNum(d))
    case PyValue.Str(s) => Left(Json.JStr(s))
    case PyValue.Bytes(b) => Left(Json.JStr(java.util.Base64.getEncoder.encodeToString(b)))
    case PyValue.NA(_) => Left(Json.JNull)
    case PyValue.Ref(r) => throw IllegalArgumentException(s"okay.foreign: a held object has no JSON shape: $r")
  }
