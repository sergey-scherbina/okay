package okay.r

import okay.codec.Json

/**
 * R as a handler (specs/r.md): calls are OPERATIONS — journalable by
 * Durable, mockable by handler swap, supervised by
 * dead-process-throws. Named functions only: there is deliberately NO
 * operation that evals a string, so untrusted input reaches R only as
 * data.
 *
 * okay-py built this shape first; this is the same model with R's own
 * two absences, which is the one place the twin is not a copy.
 */
enum RValue:
  /** R's NULL — the absence of an OBJECT. `c(1L, NULL)` is a
   * one-element vector: the NULL vanished. */
  case RNull
  /** a missing value INSIDE a vector, and TYPED: NA_integer_,
   * NA_real_, NA_character_ and a logical NA are four values.
   * `mean(c(1, NA))` is NA and `mean(c(1, NULL))` is 1, which is why
   * this is not one case with RNull. */
  case NA(of: RType)
  // named as okay-py's twin names them, and for a second reason: a
  // case called `Int` shadows scala.Int for every caller who writes
  // `import RValue.*`, which is every caller. R's integer IS 32-bit.
  case Bool(v: Boolean)
  case I32(v: Int)
  case F64(v: Double)                  // NaN is a value here, not an absence
  case Str(v: String)
  case Bytes(v: Array[Byte])           // R's raw
  case Vec(v: Vector[RValue])

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

/** a data.frame as columns — the same columnar shape okay-py's frame
 * has, because a data.frame IS a list of equal-length vectors */
final case class RFrame(cols: Vector[(String, Vector[RValue])])

/** what a failing call answers: the R condition's class and its
 * message — data, and the process survives to take the next call */
final case class Condition(kind: String, message: String)

enum REval[A] derives okay.Effect:
  case Call(fn: String, args: Vector[RValue])
    extends REval[Either[Condition, RValue]]
  case Frame(fn: String, in: RFrame, args: Vector[RValue])
    extends REval[Either[Condition, RFrame]]


/**
 * The wire halves every engine shares: `RValue` <-> the tagged JSON
 * the shim speaks.
 *
 * NULL is JSON null. Everything JSON cannot say rides a TAGGED
 * object: the four NAs, NaN, an integral double (which would merge
 * with an integer on this wire), a raw vector, and an integer (which
 * JSON has no way to keep distinct from a double).
 */
private[r] object Wire {

  def enc(v: RValue): Json = v match
    case RValue.RNull => Json.JNull
    case RValue.NA(t) => tagged("na", "of" -> Json.JStr(t.rName))
    case RValue.Bool(b) => Json.JBool(b)
    // R's integer and double are different types and stay so
    case RValue.I32(n) => tagged("i", "v" -> Json.JNum(n.toDouble))
    case RValue.F64(d) if d.isNaN => tagged("nan")
    case RValue.F64(d) => Json.JNum(d)
    case RValue.Str(s) => Json.JStr(s)
    case RValue.Bytes(bs) => tagged("raw",
      "b64" -> Json.JStr(java.util.Base64.getEncoder.encodeToString(bs)))
    case RValue.Vec(xs) => Json.JArr(xs.map(enc))

  private def tagged(t: String, fields: (String, Json)*): Json =
    Json.JObj(("t" -> Json.JStr(t)) +: fields.toVector)

  def encFrame(f: RFrame): Json = Json.JObj(Vector(
    "t" -> Json.JStr("frame"),
    "cols" -> Json.JArr(f.cols.map((n, col) =>
      Json.JArr(Vector(Json.JStr(n), Json.JArr(col.map(enc))))))))

  def dec(j: Json): RValue = j match
    case Json.JNull => RValue.RNull
    case Json.JBool(b) => RValue.Bool(b)
    case Json.JNum(n) => RValue.F64(n)
    case Json.JStr(s) => RValue.Str(s)
    case Json.JArr(xs) => RValue.Vec(xs.map(dec))
    case Json.JObj(fs) =>
      val m = fs.toMap
      def str(k: String) = m.get(k).collect { case Json.JStr(s) => s }
      def num(k: String) = m.get(k).collect { case Json.JNum(n) => n }
      str("t") match
        case Some("na") => RValue.NA(str("of").flatMap(RType.byName).getOrElse(RType.Logical))
        case Some("nan") => RValue.F64(Double.NaN)
        case Some("i") => num("v").map(n => RValue.I32(n.toInt)).getOrElse(RValue.RNull)
        case Some("raw") => str("b64").map(b => RValue.Bytes(java.util.Base64.getDecoder.decode(b)))
          .getOrElse(RValue.RNull)
        case _ => RValue.RNull      // an untagged object has no RValue shape
    case _ => RValue.RNull

  def decFrame(j: Json): Either[Condition, RFrame] = j match
    case Json.JObj(fs) if fs.toMap.get("t").contains(Json.JStr("frame")) =>
      fs.toMap.get("cols") match
        case Some(Json.JArr(cols)) =>
          Right(RFrame(cols.collect {
            case Json.JArr(Vector(Json.JStr(n), Json.JArr(vals))) => (n, vals.map(dec))
          }))
        case _ => Left(Condition("WireError", "a frame without cols"))
    case other => Left(Condition("WireError", s"expected a frame, got $other"))
}
