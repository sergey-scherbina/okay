package okay.py


import okay.!
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
  /** a `dict` with string keys, in insertion order — a record. Wire v2
   * (foreign-typed-calls): before it a dict answered by a call was sent
   * as a FRAME and reached okay as None, or failed in the shim */
  case Dict(kv: Vector[(String, PyValue)])
  /** an object HELD in the worker (foreign-object-handles): not its
   * value, a handle to it — an argument like any other, which the shim
   * turns back into the object */
  case Ref(ref: PyRef)

/**
 * A handle to a Python object kept in its worker (foreign-object-handles):
 * a fitted model, a tokenizer, an open dataset. Its methods are called by
 * name, its attributes read, and it is passed to any function as an
 * argument; `release` drops it on the far side.
 *
 * NOT a value a replay can rebuild: it names state inside ONE process. A
 * whole program replays from its journal (every step answered there), but
 * a recovery that continues live on a fresh process meets a ref that
 * process never held, and is refused by name. Durable programs keep
 * values, not handles.
 */
final case class PyRef(id: Long, pyType: String):
  /** a method of the held object, answering its value */
  def call[Out: okay.codec.Schema](method: String): PyRef.Method[Out] = PyRef.Method(this, method)
  /** a method of the held object whose result is HELD in turn */
  def hold(method: String): PyRef.HoldMethod = PyRef.HoldMethod(this, method)
  /** an attribute of the held object */
  def attr[Out: okay.codec.Schema](name: String): Either[Condition, Out] ! PyEval =
    okay.effect[PyEval, Either[Condition, PyValue]](PyEval.Attr(this, name))
      .map(_.flatMap(PyCodec.decode[Out](_)))
  /** drop the object in the worker; idempotent */
  def release: Unit ! PyEval = okay.effect[PyEval, Unit](PyEval.Release(this))

object PyRef:
  final class Method[Out: okay.codec.Schema](ref: PyRef, name: String):
    def apply(): Either[Condition, Out] ! PyEval = go(Vector.empty)
    def apply[A: ToPy](a: A): Either[Condition, Out] ! PyEval = go(Vector(ToPy(a)))
    def apply[A: ToPy, B: ToPy](a: A, b: B): Either[Condition, Out] ! PyEval = go(Vector(ToPy(a), ToPy(b)))
    def apply[A: ToPy, B: ToPy, C: ToPy](a: A, b: B, c: C): Either[Condition, Out] ! PyEval =
      go(Vector(ToPy(a), ToPy(b), ToPy(c)))
    private def go(args: Vector[PyValue]): Either[Condition, Out] ! PyEval =
      okay.effect[PyEval, Either[Condition, PyValue]](PyEval.Method(ref, name, args, hold = false))
        .map(_.flatMap(PyCodec.decode[Out](_)))

  final class HoldMethod(ref: PyRef, name: String):
    def apply(): Either[Condition, PyRef] ! PyEval = go(Vector.empty)
    def apply[A: ToPy](a: A): Either[Condition, PyRef] ! PyEval = go(Vector(ToPy(a)))
    def apply[A: ToPy, B: ToPy](a: A, b: B): Either[Condition, PyRef] ! PyEval = go(Vector(ToPy(a), ToPy(b)))
    def apply[A: ToPy, B: ToPy, C: ToPy](a: A, b: B, c: C): Either[Condition, PyRef] ! PyEval =
      go(Vector(ToPy(a), ToPy(b), ToPy(c)))
    private def go(args: Vector[PyValue]): Either[Condition, PyRef] ! PyEval =
      okay.effect[PyEval, Either[Condition, PyValue]](PyEval.Method(ref, name, args, hold = true))
        .map(_.flatMap(Wire.asRef))

/** how an argument becomes a `PyValue`: through its `Schema`, or as the
 * handle it is */
trait ToPy[A]:
  def py(a: A): PyValue

object ToPy:
  def apply[A](a: A)(using t: ToPy[A]): PyValue = t.py(a)
  given ref: ToPy[PyRef] = PyValue.Ref(_)
  given schema[A](using s: okay.codec.Schema[A]): ToPy[A] = PyCodec.encode(_)

/** a columnar frame — dict-of-lists on the far side */
final case class PyFrame(cols: Vector[(String, Vector[PyValue])]):
  /** the frame as rows of a case class, a row being the dict of its
   * cells (foreign-typed-calls): the pair okay-r's `RFrame` has */
  def rows[A](using okay.codec.Schema[A]): Either[Condition, Vector[A]] =
    val n = cols.headOption.fold(0)(_._2.length)
    cols.find(_._2.length != n) match
      case Some((name, c)) =>
        Left(Condition("FrameShape", s"column '$name' has ${c.length} cells, the first has $n"))
      case None =>
        val out = Vector.newBuilder[A]
        var bad: Option[Condition] = None
        var i = 0
        while bad.isEmpty && i < n do
          PyCodec.decode[A](PyValue.Dict(cols.map((k, c) => (k, c(i))))) match
            case Right(a) => out += a
            case Left(c) => bad = Some(c.copy(message = s"row $i: ${c.message}"))
          i += 1
        bad.toLeft(out.result())

object PyFrame:
  /** rows of a flat case class as a frame, a column per field */
  def of[A](rows: Seq[A])(using s: okay.codec.Schema[A]): Either[Condition, PyFrame] = s match
    case p: okay.codec.Schema.SProduct[A] =>
      val names = p.fields.map(_._1)
      val cells = rows.toVector.map(PyCodec.encode(_) match
        case PyValue.Dict(kv) => kv.map(_._2)
        case other => Vector(other))
      Right(PyFrame(names.zipWithIndex.map((n, j) => (n, cells.map(_(j))))))
    case other => Left(Condition("FrameSchema", s"a frame row is a case class; this Schema is $other"))

/** what a failing call answers: the exception's type name and text
 * — data, and the worker survives to take the next call */
final case class Condition(kind: String, message: String)

enum PyEval[+A] derives okay.Effect:
  case Call(fn: String, args: Vector[PyValue])
    extends PyEval[Either[Condition, PyValue]]
  case Frame(fn: String, in: PyFrame, args: Vector[PyValue])
    extends PyEval[Either[Condition, PyFrame]]
  /** a call that may CALL BACK (foreign-callbacks): Python may use
   * `okay.call(name, ...)` for the names offered here, and each use comes
   * back as a `PyStep.Ask` rather than as the call's answer */
  case Start(fn: String, args: Vector[PyValue], callbacks: Vector[String])
    extends PyEval[PyStep]
  /** the answer to an `Ask`, which resumes the Python frame waiting in
   * `okay.call`; the next step is another ask or the call's answer */
  case Resume(k: Long, answer: Either[Condition, PyValue])
    extends PyEval[PyStep]
  /** call `fn` and KEEP its result in the worker (foreign-object-handles) */
  case Hold(fn: String, args: Vector[PyValue]) extends PyEval[Either[Condition, PyRef]]
  /** a method of a held object: its value, or held in turn when `hold` */
  case Method(ref: PyRef, name: String, args: Vector[PyValue], hold: Boolean)
    extends PyEval[Either[Condition, PyValue]]
  /** an attribute of a held object */
  case Attr(ref: PyRef, name: String) extends PyEval[Either[Condition, PyValue]]
  /** drop a held object; idempotent */
  case Release(ref: PyRef) extends PyEval[Unit]

/** where a call with callbacks stands (foreign-callbacks) */
enum PyStep:
  /** the function returned (or raised) */
  case Done(answer: Either[Condition, PyValue])
  /** the function called `okay.call(callback, *args)`; `k` resumes it */
  case Ask(callback: String, args: Vector[PyValue], k: Long)

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
      case Start(fn, _, _) => fn
      case Resume(_, _) => "resume"
      case Hold(fn, _) => s"hold:$fn"
      case Method(_, name, _, _) => s"method:$name"
      case Attr(_, name) => s"attr:$name"
      case Release(_) => "release"
    def fingerprint[A](op: PyEval[A]): String = op match
      case Call(fn, args) => s"$fn#${Wire.digest(Json.JArr(args.map(Wire.enc)))}"
      case Frame(fn, in, args) =>
        s"$fn#${Wire.digest(Json.JArr(Vector(Wire.encFrame(in), Json.JArr(args.map(Wire.enc)))))}"
      case Start(fn, args, cbs) =>
        s"$fn#${Wire.digest(Json.JArr(Vector(Json.JArr(args.map(Wire.enc)), Json.JArr(cbs.map(Json.JStr(_))))))}"
      case Resume(k, answer) => s"resume/$k#${Wire.digest(Json.parse(Wire.written(answer.map(Wire.enc))))}"
      case Hold(fn, args) => s"hold:$fn#${Wire.digest(Json.JArr(args.map(Wire.enc)))}"
      case Method(r, name, args, h) => s"method:${r.id}.$name/$h#${Wire.digest(Json.JArr(args.map(Wire.enc)))}"
      case Attr(r, name) => s"attr:${r.id}.$name"
      case Release(r) => s"release:${r.id}"
    def withKey[A](op: PyEval[A], key: String): PyEval[A] = op
    def perform[A](op: PyEval[A], inner: okay.Handler[PyEval]): (A, String) = op match
      case Call(fn, args) =>
        val answer = inner.handle(Call(fn, args))
        (answer, Wire.written(answer.map(Wire.enc)))
      case Frame(fn, in, args) =>
        val answer = inner.handle(Frame(fn, in, args))
        (answer, Wire.written(answer.map(Wire.encFrame)))
      case Start(fn, args, cbs) =>
        val step = inner.handle(Start(fn, args, cbs))
        (step, Wire.writtenStep(step))
      case Resume(k, a) =>
        val step = inner.handle(Resume(k, a))
        (step, Wire.writtenStep(step))
      case Hold(fn, args) =>
        val answer = inner.handle(Hold(fn, args))
        (answer, Wire.written(answer.map(r => Wire.enc(PyValue.Ref(r)))))
      case Method(r, name, args, h) =>
        val answer = inner.handle(Method(r, name, args, h))
        (answer, Wire.written(answer.map(Wire.enc)))
      case Attr(r, name) =>
        val answer = inner.handle(Attr(r, name))
        (answer, Wire.written(answer.map(Wire.enc)))
      case Release(r) =>
        inner.handle(Release(r))
        ((), "released")
    def decode[A](op: PyEval[A], written: String): A = op match
      case Call(_, _) => Wire.read(written).map(Wire.dec)
      case Frame(_, _, _) => Wire.read(written).flatMap(Wire.decFrame)
      case Start(_, _, _) => Wire.readStep(written)
      case Resume(_, _) => Wire.readStep(written)
      case Hold(_, _) => Wire.read(written).flatMap(j => Wire.asRef(Wire.dec(j)))
      case Method(_, _, _, _) => Wire.read(written).map(Wire.dec)
      case Attr(_, _) => Wire.read(written).map(Wire.dec)
      case Release(_) => ()


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

  /** a journalled step: `{"done": <written answer>}` or `{"ask": {...}}` */
  def writtenStep(step: PyStep): String = Json.print(step match
    case PyStep.Done(a) => Json.JObj(Vector("done" -> Json.JStr(written(a.map(enc)))))
    case PyStep.Ask(cb, args, k) => Json.JObj(Vector("ask" -> Json.JObj(Vector(
      "cb" -> Json.JStr(cb), "args" -> Json.JArr(args.map(enc)), "k" -> Json.JStr(k.toString))))))

  def readStep(w: String): PyStep = Json.parse(w) match
    case Json.JObj(Vector(("done", Json.JStr(a)))) => PyStep.Done(read(a).map(dec))
    case j => step(j).getOrElse(throw IllegalStateException(s"okay.py: not a journalled step: $w"))

  /** an `ask` message from the shim (or the journal), if this is one */
  def step(j: Json): Option[PyStep.Ask] = j match
    case Json.JObj(fs) => fs.collectFirst { case ("ask", Json.JObj(a)) =>
      val m = a.toMap
      PyStep.Ask(
        m.get("cb").collect { case Json.JStr(c) => c }.getOrElse(""),
        m.get("args").collect { case Json.JArr(xs) => xs.map(dec) }.getOrElse(Vector.empty),
        m.get("k").collect {
          case Json.JNum(n) => n.toLong
          case Json.JStr(n) => n.toLong
        }.getOrElse(-1L))
    }
    case _ => None

  /** a held object's handle, or the refusal of an answer that is not one */
  def asRef(v: PyValue): Either[Condition, PyRef] = v match
    case PyValue.Ref(r) => Right(r)
    case other => Left(Condition("WireError", s"expected a held object, got $other"))

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
      // exact on the JSON wire only up to 2^53; past it, the digits
      case PyValue.I64(n) if math.abs(n.toDouble) >= Exact =>
        Json.JObj(Vector("t" -> Json.JStr("int"), "v" -> Json.JStr(n.toString)))
      case PyValue.I64(n) => Json.JNum(n.toDouble)
      case PyValue.F64(d) if d.isNaN => Json.JObj(Vector("t" -> Json.JStr("nan")))
      // an integral F64 would merge with I64 on the json wire; tagged
      case PyValue.F64(d) if d == math.floor(d) && !d.isInfinite && math.abs(d) < 1e15 =>
        Json.JObj(Vector("t" -> Json.JStr("f"), "v" -> Json.JNum(d)))
      case PyValue.F64(d) => Json.JNum(d)
      case PyValue.Str(s) => Json.JStr(s)
      case PyValue.Bytes(bs) => Json.JObj(Vector("t" -> Json.JStr("bytes"),
        "b64" -> Json.JStr(java.util.Base64.getEncoder.encodeToString(bs))))
      case PyValue.Ref(r) => Json.JObj(Vector("t" -> Json.JStr("ref"),
        "id" -> Json.JNum(r.id.toDouble), "type" -> Json.JStr(r.pyType)))
      case PyValue.Arr(_) | PyValue.Dict(_) =>
        throw IllegalStateException("unreachable: containers are handled by the work-list")

    enum Step:
      case Todo(v: PyValue)
      case Combine(n: Int)
      case CombineDict(keys: Vector[String])

    var todo = List[Step](Step.Todo(v0))
    var results = List.empty[Json]
    while todo.nonEmpty do
      todo.head match
        case Step.Todo(PyValue.Arr(xs)) =>
          todo = xs.toList.map(Step.Todo(_)) ::: Step.Combine(xs.length) :: todo.tail
        case Step.Todo(PyValue.Dict(kv)) =>
          todo = kv.toList.map(p => Step.Todo(p._2)) ::: Step.CombineDict(kv.map(_._1)) :: todo.tail
        case Step.Todo(other) =>
          results = leaf(other) :: results
          todo = todo.tail
        case Step.Combine(n) =>
          val (items, rest) = results.splitAt(n)
          results = Json.JArr(items.reverse.toVector) :: rest
          todo = todo.tail
        case Step.CombineDict(keys) =>
          val (items, rest) = results.splitAt(keys.length)
          val pairs = keys.zip(items.reverse).map((k, v) => Json.JArr(Vector(Json.JStr(k), v)))
          results = Json.JObj(Vector("t" -> Json.JStr("dict"), "kv" -> Json.JArr(pairs))) :: rest
          todo = todo.tail
    results.head

  /** the largest magnitude a JSON number (a double) carries exactly */
  private val Exact = 9007199254740992.0

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
          // a Python int past 2^53: exact as a Long when it fits one,
          // and its digits when it does not (Python ints are unbounded)
          case Some(Json.JStr("int")) => m.get("v") match
            case Some(Json.JStr(d)) => d.toLongOption.fold(PyValue.Str(d))(PyValue.I64(_))
            case _ => PyValue.PyNone
          case Some(Json.JStr("ref")) => (m.get("id"), m.get("type")) match
            case (Some(Json.JNum(i)), Some(Json.JStr(t))) => PyValue.Ref(PyRef(i.toLong, t))
            case _ => PyValue.PyNone
          case _ => PyValue.PyNone   // an untagged object has no PyValue shape
      case Json.JErr(_) => PyValue.PyNone
      case Json.JArr(_) => throw IllegalStateException("unreachable: JArr is handled by the work-list")

    enum Step:
      case Todo(j: Json)
      case Combine(n: Int)
      case CombineDict(keys: Vector[String])

    def dictPairs(fs: Vector[(String, Json)]): Option[Vector[(String, Json)]] =
      if !fs.exists(_ == ("t" -> Json.JStr("dict"))) then None
      else fs.collectFirst { case ("kv", Json.JArr(ps)) => ps.collect {
        case Json.JArr(Vector(Json.JStr(k), v)) => (k, v)
      } }

    var todo = List[Step](Step.Todo(j0))
    var results = List.empty[PyValue]
    while todo.nonEmpty do
      todo.head match
        case Step.Todo(Json.JArr(xs)) =>
          todo = xs.toList.map(Step.Todo(_)) ::: Step.Combine(xs.length) :: todo.tail
        case Step.Todo(o @ Json.JObj(fs)) =>
          dictPairs(fs) match
            case Some(kv) =>
              todo = kv.toList.map(p => Step.Todo(p._2)) ::: Step.CombineDict(kv.map(_._1)) :: todo.tail
            case None =>
              results = leaf(o) :: results
              todo = todo.tail
        case Step.Todo(other) =>
          results = leaf(other) :: results
          todo = todo.tail
        case Step.Combine(n) =>
          val (items, rest) = results.splitAt(n)
          results = PyValue.Arr(items.reverse.toVector) :: rest
          todo = todo.tail
        case Step.CombineDict(keys) =>
          val (items, rest) = results.splitAt(keys.length)
          results = PyValue.Dict(keys.zip(items.reverse)) :: rest
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
