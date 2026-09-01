package okay.py

import okay.Handler
import okay.codec.Json

/**
 * Python as a handler (specs/py.md; the model is specs/r.md's):
 * calls are OPERATIONS — journalable by Durable, mockable by
 * handler swap, supervised by dead-process-throws. Named functions
 * only: there is deliberately NO operation that evals a string, so
 * untrusted input reaches Python only as data.
 */
enum PyValue:
  case PyNone                          // Python None — DISTINCT from NaN
  case Bool(v: Boolean)
  case I64(v: Long)
  case F64(v: Double)                  // NaN is a value here, not an absence
  case Str(v: String)
  case Bytes(v: Array[Byte])
  case Arr(v: Vector[PyValue])

/** a columnar frame — dict-of-lists on the far side */
final case class PyFrame(cols: Vector[(String, Vector[PyValue])])

/** what a failing call answers: the exception's type name and text
 * — data, and the worker survives to take the next call */
final case class Condition(kind: String, message: String)

enum PyEval[A]:
  case Call(fn: String, args: Vector[PyValue])
    extends PyEval[Either[Condition, PyValue]]
  case Frame(fn: String, in: PyFrame, args: Vector[PyValue])
    extends PyEval[Either[Condition, PyFrame]]

object PyEval:
  given okay.TypeableK[PyEval] = okay.typeableK(classOf[PyEval[?]])

/** the wire halves shared by every engine: PyValue <-> the tagged
 * JSON the shim speaks (None = null; NaN and bytes ride tagged
 * objects, because JSON has neither) */
private[py] object Wire {

  def enc(v: PyValue): Json = v match
    case PyValue.PyNone => Json.JNull
    case PyValue.Bool(b) => Json.JBool(b)
    case PyValue.I64(n) => Json.JNum(n.toDouble)
    case PyValue.F64(d) if d.isNaN => Json.JObj(Vector("t" -> Json.JStr("nan")))
    // an integral F64 would merge with I64 on the json wire; tagged
    case PyValue.F64(d) if d == math.floor(d) && !d.isInfinite && math.abs(d) < 1e15 =>
      Json.JObj(Vector("t" -> Json.JStr("f"), "v" -> Json.JNum(d)))
    case PyValue.F64(d) => Json.JNum(d)
    case PyValue.Str(s) => Json.JStr(s)
    case PyValue.Bytes(bs) => Json.JObj(Vector("t" -> Json.JStr("bytes"),
      "b64" -> Json.JStr(java.util.Base64.getEncoder.encodeToString(bs))))
    case PyValue.Arr(xs) => Json.JArr(xs.map(enc))

  def encFrame(f: PyFrame): Json = Json.JObj(Vector(
    "t" -> Json.JStr("frame"),
    "cols" -> Json.JArr(f.cols.map((n, col) =>
      Json.JArr(Vector(Json.JStr(n), Json.JArr(col.map(enc))))))))

  def dec(j: Json): PyValue = j match
    case Json.JNull => PyValue.PyNone
    case Json.JBool(b) => PyValue.Bool(b)
    case Json.JNum(n) if n.isValidInt || (n == math.floor(n) && !n.isInfinite && math.abs(n) < 1e15) =>
      PyValue.I64(n.toLong)
    case Json.JNum(n) => PyValue.F64(n)
    case Json.JStr(s) => PyValue.Str(s)
    case Json.JArr(xs) => PyValue.Arr(xs.map(dec))
    case Json.JObj(fs) =>
      val m = fs.toMap
      m.get("t") match
        case Some(Json.JStr("nan")) => PyValue.F64(Double.NaN)
        case Some(Json.JStr("f")) => m.get("v") match
          case Some(Json.JNum(d)) => PyValue.F64(d)
          case _ => PyValue.PyNone
        case Some(Json.JStr("bytes")) => m.get("b64") match
          case Some(Json.JStr(b)) => PyValue.Bytes(java.util.Base64.getDecoder.decode(b))
          case _ => PyValue.PyNone
        case _ => PyValue.PyNone   // an untagged object has no PyValue shape
    case _ => PyValue.PyNone

  def decFrame(j: Json): Either[Condition, PyFrame] = j match
    case Json.JObj(fs) if fs.toMap.get("t").contains(Json.JStr("frame")) =>
      fs.toMap.get("cols") match
        case Some(Json.JArr(cols)) =>
          Right(PyFrame(cols.collect {
            case Json.JArr(Vector(Json.JStr(n), Json.JArr(vals))) => (n, vals.map(dec))
          }))
        case _ => Left(Condition("WireError", "a frame without cols"))
    case other => Left(Condition("WireError", s"expected a frame, got $other"))
}
