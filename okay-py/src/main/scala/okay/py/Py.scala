package okay.py


import okay.codec.Json

/**
 * Python as a handler (specs/py.md; the model is specs/r.md's):
 * calls are OPERATIONS — journalled by Durable (`PyEval`'s given), mockable by
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

enum PyEval[A] derives okay.Effect:
  case Call(fn: String, args: Vector[PyValue])
    extends PyEval[Either[Condition, PyValue]]
  case Frame(fn: String, in: PyFrame, args: Vector[PyValue])
    extends PyEval[Either[Condition, PyFrame]]

object PyEval:
  /**
   * `Durable` journals a Python call (foreign-journalled,
   * specs/foreign-highlevel.md stage 1). Found without an import: this
   * companion is in the implicit scope of `Journalled[PyEval]`.
   *
   * The journal's `op` is the function's address. The fingerprint is
   * the address plus a SHA-256 of the encoded arguments (and frame):
   * what the program ASKED, cheap to store however big the frame, and a
   * replay whose inputs drifted is refused by Durable's drift check. The
   * answer is written in this module's own wire JSON — a value OR a
   * condition — so a replay reads back exactly what the call answered,
   * None and NaN still distinct.
   *
   * `withKey` returns the call unchanged: a subprocess call has nowhere
   * to carry an idempotency key, so a Python call must not be declared
   * `OnRepeat.WithKey`.
   */
  given okay.codec.Journalled[PyEval] with
    def name[A](op: PyEval[A]): String = op match
      case Call(fn, _) => fn
      case Frame(fn, _, _) => fn
    def fingerprint[A](op: PyEval[A]): String = op match
      case Call(fn, args) => s"$fn#${Wire.digest(Json.JArr(args.map(Wire.enc)))}"
      case Frame(fn, in, args) =>
        s"$fn#${Wire.digest(Json.JArr(Vector(Wire.encFrame(in), Json.JArr(args.map(Wire.enc)))))}"
    def withKey[A](op: PyEval[A], key: String): PyEval[A] = op
    def perform[A](op: PyEval[A], inner: okay.Handler[PyEval]): (A, String) = op match
      case Call(fn, args) =>
        val answer = inner.handle(Call(fn, args))
        (answer, Wire.written(answer.map(Wire.enc)))
      case Frame(fn, in, args) =>
        val answer = inner.handle(Frame(fn, in, args))
        (answer, Wire.written(answer.map(Wire.encFrame)))
    def decode[A](op: PyEval[A], written: String): A = op match
      case Call(_, _) => Wire.read(written).map(Wire.dec)
      case Frame(_, _, _) => Wire.read(written).flatMap(Wire.decFrame)


/** the wire halves shared by every engine: PyValue <-> the tagged
 * JSON the shim speaks (None = null; NaN and bytes ride tagged
 * objects, because JSON has neither) */
private[py] object Wire {

  /** a journalled answer: `{"ok": ...}` or `{"condition": {...}}` */
  def written(answer: Either[Condition, Json]): String = Json.print(answer match
    case Right(j) => Json.JObj(Vector("ok" -> j))
    case Left(c) => Json.JObj(Vector("condition" -> Json.JObj(Vector(
      "kind" -> Json.JStr(c.kind), "message" -> Json.JStr(c.message))))))

  /** a journalled answer back: the value's JSON, or its condition */
  def read(written: String): Either[Condition, Json] = Json.parse(written) match
    case Json.JObj(Vector(("ok", j))) => Right(j)
    case Json.JObj(Vector(("condition", Json.JObj(fs)))) =>
      def field(n: String) = fs.collectFirst { case (`n`, Json.JStr(v)) => v }.getOrElse("")
      Left(Condition(field("kind"), field("message")))
    case other => throw IllegalStateException(s"okay.py: not a journalled answer: ${Json.print(other)}")

  /** SHA-256 of a value's printed JSON, hex */
  def digest(j: Json): String =
    java.security.MessageDigest.getInstance("SHA-256").nn
      .digest(Json.print(j).getBytes(java.nio.charset.StandardCharsets.UTF_8)).nn
      .map(b => f"${b & 0xff}%02x").mkString

  /**
   * `PyValue.Arr`/`Json.JArr` recurse on the VALUE's own nesting —
   * Python's `list` nests as deep as a script chooses, and this is
   * the boundary an arbitrarily-deep Python return value crosses
   * (subprocess-wire-depth-safety, the same defect shape
   * `okay-mcp/Rpc.damaged` and `okay-demo/StateMcp.damaged` already
   * had fixed twice). A lower-severity threat model than raw network
   * bytes (usually the caller's own subprocess, not an attacker) but
   * the same mechanism — an explicit work-list, not native recursion:
   * `todo` holds nodes still to visit and `Combine(n)` markers saying
   * "the last n results belong to one array, in order"; `results`
   * accumulates finished values, most recent first, so `Combine`
   * reverses its slice before rebuilding the array.
   */
  def enc(v0: PyValue): Json =
    def leaf(v: PyValue): Json = v match
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
      case PyValue.Arr(_) => throw IllegalStateException("unreachable: Arr is handled by the work-list")

    enum Step:
      case Todo(v: PyValue)
      case Combine(n: Int)

    var todo = List[Step](Step.Todo(v0))
    var results = List.empty[Json]
    while todo.nonEmpty do
      todo.head match
        case Step.Todo(PyValue.Arr(xs)) =>
          todo = xs.toList.map(Step.Todo(_)) ::: Step.Combine(xs.length) :: todo.tail
        case Step.Todo(other) =>
          results = leaf(other) :: results
          todo = todo.tail
        case Step.Combine(n) =>
          val (items, rest) = results.splitAt(n)
          results = Json.JArr(items.reverse.toVector) :: rest
          todo = todo.tail
    results.head

  def encFrame(f: PyFrame): Json = Json.JObj(Vector(
    "t" -> Json.JStr("frame"),
    "cols" -> Json.JArr(f.cols.map((n, col) =>
      Json.JArr(Vector(Json.JStr(n), Json.JArr(col.map(enc))))))))

  /** `enc`'s mirror — same work-list, same reasoning */
  def dec(j0: Json): PyValue =
    def leaf(j: Json): PyValue = j match
      case Json.JNull => PyValue.PyNone
      case Json.JBool(b) => PyValue.Bool(b)
      case Json.JNum(n) if n.isValidInt || (n == math.floor(n) && !n.isInfinite && math.abs(n) < 1e15) =>
        PyValue.I64(n.toLong)
      case Json.JNum(n) => PyValue.F64(n)
      case Json.JStr(s) => PyValue.Str(s)
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
      case Json.JErr(_) => PyValue.PyNone
      case Json.JArr(_) => throw IllegalStateException("unreachable: JArr is handled by the work-list")

    enum Step:
      case Todo(j: Json)
      case Combine(n: Int)

    var todo = List[Step](Step.Todo(j0))
    var results = List.empty[PyValue]
    while todo.nonEmpty do
      todo.head match
        case Step.Todo(Json.JArr(xs)) =>
          todo = xs.toList.map(Step.Todo(_)) ::: Step.Combine(xs.length) :: todo.tail
        case Step.Todo(other) =>
          results = leaf(other) :: results
          todo = todo.tail
        case Step.Combine(n) =>
          val (items, rest) = results.splitAt(n)
          results = PyValue.Arr(items.reverse.toVector) :: rest
          todo = todo.tail
    results.head

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
