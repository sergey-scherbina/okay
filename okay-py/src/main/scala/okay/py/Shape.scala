package okay.py

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
    case PyValue.Ref(r) => throw IllegalArgumentException(s"okay.py: a held object has no JSON shape: $r")
  }
