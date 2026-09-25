package okay.py


import okay.!
import okay.codec.Json

/**
 * Python as a handler (specs/py.md; the model is specs/r.md's):
 * calls are OPERATIONS — journalled by Durable (`ForeignEval`'s given), mockable by
 * handler swap, supervised by dead-process-throws. Named functions
 * only: there is deliberately NO operation that evals a string, so
 * untrusted input reaches Python only as data.
 */
enum PyValue:
  case PyNone                          // Python None — DISTINCT from NaN
  case Bool(v: Boolean)
  case I64(v: Long)
  /** an int past a Long: Python's ints are unbounded (schema-stubs found a
   * BigInt crossing as a `str`, which a TypedDict saying `int` refused) */
  case BigI(v: scala.math.BigInt)
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
  /** a missing value OF A TYPE — R's `NA_integer_`, `NA_real_`,
   * `NA_character_` and logical `NA` (foreign-one-value): four values, and
   * none of them `PyNone`, because `mean(c(1, NA))` is NA while
   * `mean(c(1, NULL))` is 1. `of` names the type: "logical", "integer",
   * "double", "character". A language without typed absences never sends
   * one; the shared tree carries it so R needs no tree of its own. */
  case NA(of: String)

object PyValue:
  /** every `Ref` id in `v`, preorder — on a worklist, since a value a
   * program passes can nest as deep as it built it (stack-safety-py-r) */
  def refs(v: PyValue): Vector[Long] =
    val out = Vector.newBuilder[Long]
    val todo = scala.collection.mutable.Stack[PyValue](v)
    while todo.nonEmpty do todo.pop() match
      case Ref(r) => out += r.id
      case Arr(xs) => xs.reverseIterator.foreach(todo.push)
      case Dict(kv) => kv.reverseIterator.foreach(p => todo.push(p._2))
      case _ => ()
    out.result()

  /** `v` rebuilt bottom-up: every leaf through `leaf`, every `Arr` and
   * `Dict` rebuilt around its rebuilt children — the one walk the
   * workers' `in`/`out`/`local` renamings are (stack-safety-py-r) */
  def rebuild(v: PyValue)(leaf: PyValue => PyValue): PyValue =
    Walk.up[PyValue, PyValue](v) {
      case Arr(xs) => Right((xs, Arr(_)))
      case Dict(kv) => Right((kv.map(_._2), vs => Dict(kv.map(_._1).zip(vs))))
      case l => Left(leaf(l))
    }

  /** `rebuild` where a leaf may refuse: the first refusal is the answer */
  def rebuildE[E](v: PyValue)(leaf: PyValue => Either[E, PyValue]): Either[E, PyValue] =
    Walk.up[PyValue, Either[E, PyValue]](v) {
      case Arr(xs) => Right((xs, vs => Walk.sequence(vs).map(Arr(_))))
      case Dict(kv) => Right((kv.map(_._2), vs => Walk.sequence(vs).map(ws => Dict(kv.map(_._1).zip(ws)))))
      case l => Left(leaf(l))
    }

/**
 * A bottom-up walk of any tree on an explicit stack (stack-safety-py-r):
 * `step` says of a node whether it is a leaf (`Left(its value)`) or a
 * node (`Right(children, assemble)`), and the tree is rebuilt from the
 * leaves up without a native frame per level. It is what the Json <->
 * PyValue conversions and the ref renamings share, so that a value as
 * deep as a worker made it costs heap, not stack.
 */
private[py] object Walk:
  def up[In, Out](root: In)(step: In => Either[Out, (Vector[In], Vector[Out] => Out)]): Out =
    final class Open(val kids: Vector[In], val assemble: Vector[Out] => Out):
      var i = 0
      val done = Vector.newBuilder[Out]
    val open = scala.collection.mutable.Stack[Open]()
    var todo: Option[In] = Some(root)
    var value: Option[Out] = None
    var result: Option[Out] = None
    while result.isEmpty do
      todo match
        case Some(node) =>
          todo = None
          step(node) match
            case Left(out) => value = Some(out)
            case Right((kids, assemble)) => open.push(Open(kids, assemble))
        case None =>
          value match
            case Some(out) =>
              value = None
              if open.isEmpty then result = Some(out)
              else { val o = open.top; o.done += out; o.i += 1 }
            case None =>
              val o = open.top
              if o.i < o.kids.length then todo = Some(o.kids(o.i))
              else { val _ = open.pop(); value = Some(o.assemble(o.done.result())) }
    result.get

  /** the first Left, or every Right */
  def sequence[E, A](xs: Vector[Either[E, A]]): Either[E, Vector[A]] =
    val out = Vector.newBuilder[A]
    var i = 0
    var bad: Option[E] = None
    while bad.isEmpty && i < xs.length do
      xs(i) match
        case Right(a) => out += a
        case Left(e) => bad = Some(e)
      i += 1
    bad.toLeft(out.result())

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
final case class PyRef(id: Long, pyType: String,
                       /** the shape its methods speak: the API that held it */
                       shape: Shape = Shape.python):
  /** a method of the held object, answering its value */
  def call[Out: okay.codec.Schema](method: String): PyRef.Method[Out] = PyRef.Method(this, method)
  /** a method of the held object whose result is HELD in turn */
  def hold(method: String): PyRef.HoldMethod = PyRef.HoldMethod(this, method)
  /** an attribute of the held object */
  def attr[Out: okay.codec.Schema](name: String): Either[Condition, Out] ! ForeignEval =
    okay.effect[ForeignEval, Either[Condition, PyValue]](ForeignEval.Attr(this, name))
      .map(_.flatMap(shape.decode[Out](_)))
  /** drop the object in the worker; idempotent */
  def release: Unit ! ForeignEval = okay.effect[ForeignEval, Unit](ForeignEval.Release(this))
  /** this object as a STATEFUL stage over chunks (foreign-streaming):
   * `method` per chunk, `finish` once at the end */
  def stage[I: ToPy, O: okay.codec.Schema](method: String, chunk: Int = 64,
                                          finish: Option[String] = None): Unit ! PyStream.Row[I, O] =
    PyStream.chunked[I, O](chunk,
      buf => ForeignEval.Method(this, method, Vector(PyValue.Arr(buf)), hold = false),
      finish.map(f => ForeignEval.Method(this, f, Vector.empty, hold = false)))

object PyRef:
  final class Method[Out: okay.codec.Schema](ref: PyRef, name: String):
    private given Shape = ref.shape
    def apply(): Either[Condition, Out] ! ForeignEval = go(Vector.empty)
    def apply[A: ToPy](a: A): Either[Condition, Out] ! ForeignEval = go(Vector(ToPy(a)))
    def apply[A: ToPy, B: ToPy](a: A, b: B): Either[Condition, Out] ! ForeignEval = go(Vector(ToPy(a), ToPy(b)))
    def apply[A: ToPy, B: ToPy, C: ToPy](a: A, b: B, c: C): Either[Condition, Out] ! ForeignEval =
      go(Vector(ToPy(a), ToPy(b), ToPy(c)))
    private def go(args: Vector[PyValue]): Either[Condition, Out] ! ForeignEval =
      okay.effect[ForeignEval, Either[Condition, PyValue]](ForeignEval.Method(ref, name, args, hold = false))
        .map(_.flatMap(ref.shape.decode[Out](_)))

  final class HoldMethod(ref: PyRef, name: String):
    private given Shape = ref.shape
    def apply(): Either[Condition, PyRef] ! ForeignEval = go(Vector.empty)
    def apply[A: ToPy](a: A): Either[Condition, PyRef] ! ForeignEval = go(Vector(ToPy(a)))
    def apply[A: ToPy, B: ToPy](a: A, b: B): Either[Condition, PyRef] ! ForeignEval = go(Vector(ToPy(a), ToPy(b)))
    def apply[A: ToPy, B: ToPy, C: ToPy](a: A, b: B, c: C): Either[Condition, PyRef] ! ForeignEval =
      go(Vector(ToPy(a), ToPy(b), ToPy(c)))
    private def go(args: Vector[PyValue]): Either[Condition, PyRef] ! ForeignEval =
      okay.effect[ForeignEval, Either[Condition, PyValue]](ForeignEval.Method(ref, name, args, hold = true))
        .map(_.flatMap(Wire.asRef).map(_.copy(shape = ref.shape)))

/** how an argument becomes a `PyValue`: through its `Schema`, or as the
 * handle it is */
trait ToPy[A]:
  def py(a: A)(using Shape): PyValue

object ToPy:
  def apply[A](a: A)(using t: ToPy[A], shape: Shape): PyValue = t.py(a)
  given ref: ToPy[PyRef] with
    def py(a: PyRef)(using Shape): PyValue = PyValue.Ref(a)
  given schema[A](using s: okay.codec.Schema[A]): ToPy[A] with
    def py(a: A)(using shape: Shape): PyValue = shape.encode(a)

/**
 * A columnar frame — dict-of-lists on the far side (a data.frame in R).
 * It CARRIES the value rules it is read by (`shape`, outside equality, as
 * `PyRef` carries its own): Python's by default, R's for a frame an R
 * worker answered or `RFrame.of` built (foreign-one-value) — so `rows`
 * never reads an R frame by Python's rules because of what happened to be
 * in scope.
 */
final case class PyFrame(cols: Vector[(String, Vector[PyValue])], shape: Shape = Shape.python):
  /** the frame as rows of a case class, a row being the dict of its
   * cells (foreign-typed-calls), by this frame's own value rules */
  def rows[A](using okay.codec.Schema[A]): Either[Condition, Vector[A]] =
    shape.rows[A](this)

  /** the same columns, read by `s`'s rules */
  def ruledBy(s: Shape): PyFrame = PyFrame(cols, s)

  // what a frame IS is its columns; its rules are how it is READ
  override def equals(other: Any): Boolean = other match
    case f: PyFrame => f.cols == cols
    case _ => false
  override def hashCode: Int = cols.hashCode
  override def toString: String = s"PyFrame($cols)"

object PyFrame:
  /** rows of a flat case class as a frame, a column per field, by the
   * value rules of the shape in scope (Python's unless one is given) */
  def of[A](rows: Seq[A])(using okay.codec.Schema[A], Shape): Either[Condition, PyFrame] =
    summon[Shape].frame(rows)

/** what a failing call answers: the exception's type name and text
 * — data, and the worker survives to take the next call */
final case class Condition(kind: String, message: String)

enum ForeignEval[+A] derives okay.Effect:
  case Call(fn: String, args: Vector[PyValue])
    extends ForeignEval[Either[Condition, PyValue]]
  case Frame(fn: String, in: PyFrame, args: Vector[PyValue])
    extends ForeignEval[Either[Condition, PyFrame]]
  /** a call that may CALL BACK (foreign-callbacks): Python may use
   * `okay.call(name, ...)` for the names offered here, and each use comes
   * back as a `PyStep.Ask` rather than as the call's answer */
  case Start(fn: String, args: Vector[PyValue], callbacks: Vector[String])
    extends ForeignEval[PyStep]
  /** the answer to an `Ask`, which resumes the Python frame waiting in
   * `okay.call`; the next step is another ask or the call's answer */
  case Resume(k: Long, answer: Either[Condition, PyValue])
    extends ForeignEval[PyStep]
  /** call `fn` and KEEP its result in the worker (foreign-object-handles) */
  case Hold(fn: String, args: Vector[PyValue]) extends ForeignEval[Either[Condition, PyRef]]
  /** a method of a held object: its value, or held in turn when `hold` */
  case Method(ref: PyRef, name: String, args: Vector[PyValue], hold: Boolean)
    extends ForeignEval[Either[Condition, PyValue]]
  /** an attribute of a held object */
  case Attr(ref: PyRef, name: String) extends ForeignEval[Either[Condition, PyValue]]
  /** drop a held object; idempotent */
  case Release(ref: PyRef) extends ForeignEval[Unit]
  /** start a program-as-data (remote-foreign): the function returns a
   * Python `okay.done`/`okay.perform(...).then(...)` tree, handed over one
   * node at a time under the run id the HOST chose */
  case Program(run: Long, fn: String, args: Vector[PyValue]) extends ForeignEval[Either[Condition, PyNode]]
  /** continue run `run` at continuation `k` with `answer`; the far side
   * keeps `k`, so the same one may be continued again (multi-shot) */
  case Continue(run: Long, k: Long, answer: PyValue) extends ForeignEval[Either[Condition, PyNode]]
  /** drop every continuation of a run; idempotent */
  case Forget(run: Long) extends ForeignEval[Unit]

/** one node of a program-as-data (remote-foreign) */
enum PyNode:
  case Done(value: PyValue)
  case Perform(name: String, args: Vector[PyValue], k: Long)

/** where a call with callbacks stands (foreign-callbacks) */
enum PyStep:
  /** the function returned (or raised) */
  case Done(answer: Either[Condition, PyValue])
  /** the function called `okay.call(callback, *args)`; `k` resumes it */
  case Ask(callback: String, args: Vector[PyValue], k: Long)

object ForeignEval:
  /**
   * `Durable` journals a Python call (foreign-journalled,
   * specs/foreign-highlevel.md stage 1). Found without an import: this
   * companion is in the implicit scope of `Journalled[ForeignEval]`.
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
  given okay.codec.Journalled[ForeignEval] with
    def name[A](op: ForeignEval[A]): String = op match
      case Call(fn, _) => fn
      case Frame(fn, _, _) => fn
      case Start(fn, _, _) => fn
      case Resume(_, _) => "resume"
      case Hold(fn, _) => s"hold:$fn"
      case Method(_, name, _, _) => s"method:$name"
      case Attr(_, name) => s"attr:$name"
      case Release(_) => "release"
      case Program(_, fn, _) => s"program:$fn"
      case Continue(_, _, _) => "continue"
      case Forget(_) => "forget"
    def fingerprint[A](op: ForeignEval[A]): String = op match
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
      case Program(run, fn, args) => s"program:$run:$fn#${Wire.digest(Json.JArr(args.map(Wire.enc)))}"
      case Continue(run, k, a) => s"continue:$run/$k#${Wire.digest(Wire.enc(a))}"
      case Forget(run) => s"forget:$run"
    def withKey[A](op: ForeignEval[A], key: String): ForeignEval[A] = op
    def perform[A](op: ForeignEval[A], inner: okay.Handler[ForeignEval]): (A, String) = op match
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
      case Program(run, fn, args) =>
        val answer = inner.handle(Program(run, fn, args))
        (answer, Wire.written(answer.map(Wire.encNode)))
      case Continue(run, k, a) =>
        val answer = inner.handle(Continue(run, k, a))
        (answer, Wire.written(answer.map(Wire.encNode)))
      case Forget(run) =>
        inner.handle(Forget(run))
        ((), "forgotten")
    def decode[A](op: ForeignEval[A], written: String): A = op match
      case Call(_, _) => Wire.read(written).map(Wire.dec)
      // an answer frame is read by the rules its REQUEST frame was made under
      case Frame(_, in, _) => Wire.read(written).flatMap(Wire.decFrame).map(_.ruledBy(in.shape))
      case Start(_, _, _) => Wire.readStep(written)
      case Resume(_, _) => Wire.readStep(written)
      case Hold(_, _) => Wire.read(written).flatMap(j => Wire.asRef(Wire.dec(j)))
      case Method(_, _, _, _) => Wire.read(written).map(Wire.dec)
      case Attr(_, _) => Wire.read(written).map(Wire.dec)
      case Release(_) => ()
      case Program(_, _, _) => Wire.read(written).flatMap(Wire.decNode)
      case Continue(_, _, _) => Wire.read(written).flatMap(Wire.decNode)
      case Forget(_) => ()


/** the wire halves shared by every engine: PyValue <-> the tagged
 * JSON the shim speaks (None = null; NaN and bytes ride tagged
 * objects, because JSON has neither) */
private[okay] object Wire {

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

  /** a program node on the wire: `{"done": v}` or `{"perform": n, "args": [...], "k": k}` */
  def encNode(n: PyNode): Json = n match
    case PyNode.Done(v) => Json.JObj(Vector("done" -> enc(v)))
    case PyNode.Perform(name, args, k) => Json.JObj(Vector(
      "perform" -> Json.JStr(name), "args" -> Json.JArr(args.map(enc)), "k" -> Json.JNum(k.toDouble)))

  def decNode(j: Json): Either[Condition, PyNode] = j match
    case Json.JObj(fs) =>
      val m = fs.toMap
      (m.get("done"), m.get("perform")) match
        case (Some(v), _) => Right(PyNode.Done(dec(v)))
        case (_, Some(Json.JStr(name))) =>
          val args = m.get("args").collect { case Json.JArr(xs) => xs.map(dec) }.getOrElse(Vector.empty)
          m.get("k") match
            case Some(Json.JNum(k)) => Right(PyNode.Perform(name, args, k.toLong))
            case _ => Left(Condition("WireError", s"a perform without a continuation: $j"))
        case _ => Left(Condition("WireError", s"not a program node: $j"))
    case other => Left(Condition("WireError", s"not a program node: $other"))

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
      case PyValue.BigI(n) =>
        Json.JObj(Vector("t" -> Json.JStr("int"), "v" -> Json.JStr(n.toString)))
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
      case PyValue.NA(of) => Json.JObj(Vector("t" -> Json.JStr("na"), "of" -> Json.JStr(of)))
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
            case Some(Json.JStr(d)) => d.toLongOption.fold(PyValue.BigI(scala.math.BigInt(d)))(PyValue.I64(_))
            case _ => PyValue.PyNone
          // jsonlite may box a scalar: the id and type are read either way
          case Some(Json.JStr("ref")) => (unboxed(m.get("id")), unboxed(m.get("type"))) match
            case (Some(Json.JNum(i)), Some(Json.JStr(t))) => PyValue.Ref(PyRef(i.toLong, t))
            case (Some(Json.JNum(i)), _) => PyValue.Ref(PyRef(i.toLong, "?"))
            case _ => PyValue.PyNone
          case Some(Json.JStr("na")) => PyValue.NA(unboxed(m.get("of")).collect { case Json.JStr(t) => t }.getOrElse("logical"))
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

  private def unboxed(j: Option[Json]): Option[Json] = j match
    case Some(Json.JArr(Vector(one))) => Some(one)
    case other => other

  /** the frame format's version inside the envelope: 2 is the columnar shape */
  val FrameFormat = 2

  /**
   * A frame, COLUMNAR (r-frame-columnar-wire, the shape every far side
   * that announces `"frames": ["columnar"]` reads since foreign-one-value):
   * the type belongs to the column, the values are a plain array — the
   * road a JSON library takes fastest — and the absences are index lists
   * beside them, so R's typed NAs stay four and NA stays apart from NaN
   * without a tag per cell. The placeholder at an absent position is the
   * type's ZERO, never null (a null in a numeric array is jsonlite's slow
   * path); `na`/`nan` are the authority. A column the four atomic types
   * cannot carry — bytes, nesting, a mix, an integer past 32 bits (R's
   * integer is 32-bit) — keeps the per-cell form under `cells`.
   */
  def encFrameColumnar(f: PyFrame): Json = Json.JObj(Vector(
    "t" -> Json.JStr("frame"),
    "v" -> Json.JNum(FrameFormat.toDouble),
    "cols" -> Json.JArr(f.cols.map((n, col) => encCol(n, col)))))

  private def encCol(name: String, col: Vector[PyValue]): Json =
    columnType(col) match
      case None =>
        Json.JObj(Vector("name" -> Json.JStr(name), "cells" -> Json.JArr(col.map(enc))))
      case Some(t) =>
        val na = Vector.newBuilder[Json]
        val nan = Vector.newBuilder[Json]
        val zero: Json = t match
          case "l" => Json.JBool(false)
          case "s" => Json.JStr("")
          case _ => Json.JNum(0)
        val values = col.zipWithIndex.map { (v, i) =>
          v match
            case PyValue.NA(_) | PyValue.PyNone => na += Json.JNum(i.toDouble); zero
            case PyValue.F64(d) if d.isNaN => nan += Json.JNum(i.toDouble); zero
            case PyValue.Bool(b) => Json.JBool(b)
            case PyValue.I64(x) => Json.JNum(x.toDouble)
            case PyValue.F64(x) => Json.JNum(x)
            case PyValue.Str(s) => Json.JStr(s)
            case _ => zero   // unreachable: columnType admitted the column
        }
        val fields = Vector("name" -> Json.JStr(name), "type" -> Json.JStr(t),
          "values" -> Json.JArr(values), "na" -> Json.JArr(na.result()))
        Json.JObj(if t == "d" then fields :+ ("nan" -> Json.JArr(nan.result())) else fields)

  /** the column's ONE type, when the four atomic ones carry it: "l", "i",
   * "d", "s" — an NA names its own, a value its own, and a mix (or bytes,
   * nesting, an integer past 32 bits) has none. An empty column is
   * logical, which is what R's own `c()` gives. */
  private def columnType(col: Vector[PyValue]): Option[String] =
    var seen: Option[String] = None
    var ok = true
    col.foreach { v =>
      val t = v match
        case PyValue.NA(of) => Some(of.take(1) match { case "c" => "s"; case c => c })
        case PyValue.Bool(_) => Some("l")
        case PyValue.I64(x) if x.isValidInt => Some("i")
        case PyValue.F64(_) => Some("d")
        case PyValue.Str(_) => Some("s")
        case PyValue.PyNone => None
        case _ => ok = false; None
      (seen, t) match
        case (_, None) => ()
        case (None, Some(x)) => seen = Some(x)
        case (Some(a), Some(b)) if a == b => ()
        case _ => ok = false
    }
    if !ok then None else Some(seen.getOrElse("l"))

  /** a frame off the wire: the columnar shape (v2) or the per-cell pairs
   * of v1 — every reader accepts both, an encoder writes one */
  def decFrame(j: Json): Either[Condition, PyFrame] = j match
    case Json.JObj(fs) if fs.toMap.get("t").contains(Json.JStr("frame")) =>
      val m = fs.toMap
      val v = unboxed(m.get("v")).collect { case Json.JNum(n) => n.toInt }.getOrElse(1)
      if v > FrameFormat then
        Left(Condition("WireError", s"frame format v$v: this host reads up to v$FrameFormat — refuse rather than guess"))
      else m.get("cols") match
        case Some(Json.JArr(cols)) =>
          val out = cols.map(decCol)
          out.collectFirst { case Left(c) => c }.toLeft(PyFrame(out.collect { case Right(p) => p }))
        case _ => Left(Condition("WireError", "a frame without cols"))
    case other => Left(Condition("WireError", s"expected a frame, got $other"))

  private def decCol(j: Json): Either[Condition, (String, Vector[PyValue])] = j match
    case Json.JArr(Vector(Json.JStr(n), Json.JArr(vals))) => Right(n -> vals.map(dec))
    case Json.JObj(fs) =>
      val m = fs.toMap
      val name = unboxed(m.get("name")).collect { case Json.JStr(s) => s }.getOrElse("")
      (m.get("cells"), m.get("values")) match
        case (Some(cells), _) => Right(name -> asArray(cells).map(dec))
        case (_, Some(raw)) =>
          val t = unboxed(m.get("type")).collect { case Json.JStr(s) => s }.getOrElse("l")
          def idx(k: String): Set[Int] = m.get(k).map(asArray).getOrElse(Vector.empty).collect { case Json.JNum(n) => n.toInt }.toSet
          val na = idx("na")
          val nan = idx("nan")
          val of = t match
            case "i" => "integer"
            case "d" => "double"
            case "s" => "character"
            case _ => "logical"
          Right(name -> asArray(raw).zipWithIndex.map { (x, i) =>
            if na(i) then PyValue.NA(of)
            else if nan(i) then PyValue.F64(Double.NaN)
            else (t, x) match
              case ("l", Json.JBool(b)) => PyValue.Bool(b)
              case ("i", Json.JNum(n)) => PyValue.I64(n.toLong)
              case ("d", Json.JNum(n)) => PyValue.F64(n)
              case ("s", Json.JStr(s)) => PyValue.Str(s)
              case (_, other) => dec(other)
          })
        case _ => Left(Condition("WireError", s"a column with neither values nor cells: $j"))
    case other => Left(Condition("WireError", s"not a column: $other"))

  /** jsonlite UNBOXES a length-1 vector: a one-row column arrives as a
   * scalar and a single absence as a bare number (r-frame-columnar-wire) */
  private def asArray(j: Json): Vector[Json] = j match
    case Json.JArr(xs) => xs
    case Json.JNull => Vector.empty
    case one => Vector(one)
}
