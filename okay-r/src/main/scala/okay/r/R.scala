package okay.r

import okay.!
import okay.codec.Json

/**
 * R as a handler (specs/r.md): calls are OPERATIONS — journalled by
 * Durable (`REval`'s given), mockable by handler swap, supervised by
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
  /** a NAMED LIST that is not a data.frame — a record, in order. Wire v3
   * (foreign-typed-calls): before it a named list answered by a call was
   * sent as a frame, which a value decode could not read */
  case Named(kv: Vector[(String, RValue)])
  /** an object HELD in the R process (foreign-object-handles) — a fitted
   * model, a formula — passed to any function as an argument */
  case Ref(ref: RRef)

/**
 * A handle to an R object kept in its process (foreign-object-handles).
 * R applies functions TO objects rather than calling methods on them, so
 * a ref is used as an argument: `R.fn[Vector[Double]]("stats::predict")(fit, newdata)`.
 * As okay-py's `PyRef`: it names state in one process, so a recovery onto
 * a fresh process meets a ref it never held and is refused by name.
 */
final case class RRef(id: Long, rClass: String):
  /** drop the object in the R process; idempotent */
  def release: Unit ! REval = okay.effect[REval, Unit](REval.Release(this))
  /** this held CLOSURE as a stateful stage over chunks (foreign-streaming);
   * `finish`, another held closure, is called with nothing at the end */
  def stage[I: ToR, O: okay.codec.Schema](chunk: Int = 64, finish: Option[RRef] = None): Unit ! RStream.Row[I, O] =
    RStream.chunked[I, O](chunk, buf => RStream.viaClosure(this, Vector(RValue.Vec(buf))),
      finish.map(f => RStream.viaClosure(f, Vector.empty)))

/** how an argument becomes an `RValue`: through its `Schema`, or as the
 * handle it is */
trait ToR[A]:
  def r(a: A): RValue

object ToR:
  def apply[A](a: A)(using t: ToR[A]): RValue = t.r(a)
  given ref: ToR[RRef] = RValue.Ref(_)
  given schema[A](using s: okay.codec.Schema[A]): ToR[A] = RCodec.encode(_)

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
final case class RFrame(cols: Vector[(String, Vector[RValue])]):
  /** the frame as a Seq of a flat case class (r-finish): the analyst
   * sends and reads ROWS, and the wire stays columns because a
   * data.frame is columns. Fields match columns BY NAME, and a
   * mismatch either way is a `Condition` naming what does not
   * line up — a frame whose columns drifted is the commonest way an
   * R pipeline goes quietly wrong. */
  def rows[A](using s: okay.codec.Schema[A]): Either[Condition, Vector[A]] =
    RFrame.rowsOf(this)

object RFrame:
  import okay.codec.Schema

  /** every row, decoded at `A` */
  def rowsOf[A](f: RFrame)(using s: Schema[A]): Either[Condition, Vector[A]] =
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
            decode(sc(), cols(name)(i)) match
              case Right(v) => v
              case Left(why) =>
                if bad.isEmpty then bad = Some(Condition("FrameSchema", s"column '$name', row $i: $why"))
                null
          }
          p.make(parts)
        }
        bad.toLeft(out)
    }

  /** the rows as a frame: the field order IS the column order */
  def of[A](rows: Seq[A])(using s: Schema[A]): Either[Condition, RFrame] =
    product(s).map { p =>
      val cells = rows.toVector.map(a => p.parts(a).toVector)
      RFrame(p.fields.zipWithIndex.map { case ((name, sc), i) =>
        name -> cells.map(row => encode(sc(), row(i)))
      })
    }

  private def product[A](s: Schema[A]): Either[Condition, Schema.SProduct[A]] = s match
    case p: Schema.SProduct[A] => Right(p)
    case other => Left(Condition("FrameSchema",
      s"a frame row is a flat case class; this Schema is $other"))

  /** an R cell at a field's type. R has no 64-bit integer and no
   * nesting inside a data.frame column, so the vocabulary is small
   * and stated: the four scalars, raw bytes, and Option for NA/NULL. */
  private def decode[X](s: Schema[X], v: RValue): Either[String, Any] = (s, v) match
    case (_: Schema.SOption[?], RValue.NA(_) | RValue.RNull) => Right(None)
    case (o: Schema.SOption[?], other) => decode(o.of(), other).map(Some(_))
    case (Schema.SInt, RValue.I32(x)) => Right(x)
    case (Schema.SLong, RValue.I32(x)) => Right(x.toLong)
    // R has no 64-bit integer: a Long crossed as an exact double or as its digits
    case (Schema.SLong, RValue.F64(x)) if x == math.floor(x) && math.abs(x) <= RCodec.Exact => Right(x.toLong)
    case (Schema.SLong, RValue.Str(x)) if x.toLongOption.isDefined => Right(x.toLong)
    case (Schema.SDouble, RValue.F64(x)) => Right(x)
    case (Schema.SDouble, RValue.I32(x)) => Right(x.toDouble)
    case (Schema.SBool, RValue.Bool(x)) => Right(x)
    case (Schema.SString, RValue.Str(x)) => Right(x)
    case (Schema.SBytes, RValue.Bytes(x)) => Right(x)
    // R has no integer past 32 bits: a BigInt travels as its digits (schema-bigint)
    case (Schema.SBigInt, RValue.Str(x)) =>
      scala.util.Try(BigInt(x)).toOption.toRight(s"not an integer: \"$x\"")
    case (Schema.SBigInt, RValue.I32(x)) => Right(BigInt(x))
    case (Schema.SIso(under, to, _), other) =>
      decode(under(), other).flatMap(u => to(u.asInstanceOf) match
        case Right(a) => Right(a)
        case Left(why) => Left(why))
    case (sc, other) => Left(s"$other does not fit $sc")

  private def encode[X](s: Schema[X], v: Any): RValue = (s, v) match
    case (o: Schema.SOption[?], None) => naOf(o.of())
    case (o: Schema.SOption[?], Some(x)) => encode(o.of(), x)
    case (Schema.SInt, x: Int) => RValue.I32(x)
    // was `I32(x.toInt)`, which truncated any Long past 32 bits SILENTLY
    case (Schema.SLong, x: Long) => RCodec.long(x)
    case (Schema.SDouble, x: Double) => RValue.F64(x)
    case (Schema.SBool, x: Boolean) => RValue.Bool(x)
    case (Schema.SString, x: String) => RValue.Str(x)
    case (Schema.SBytes, x: Array[Byte]) => RValue.Bytes(x)
    case (Schema.SBigInt, x: BigInt) => RValue.Str(x.toString)
    case (Schema.SIso(under, _, from), x) => encode(under(), from.asInstanceOf[Any => Any](x))
    case (_, other) => RValue.Str(String.valueOf(other))

  /** an absent cell keeps its COLUMN's type: R's four NAs are four
   * values, and a column of NA_character_ is not a logical column */
  private def naOf[X](s: Schema[X]): RValue = s match
    case Schema.SInt | Schema.SLong => RValue.NA(RType.Integer)
    case Schema.SDouble => RValue.NA(RType.Double)
    case Schema.SString => RValue.NA(RType.Character)
    case Schema.SBool => RValue.NA(RType.Logical)
    case _ => RValue.RNull

/** what a failing call answers: the R condition's class and its
 * message — data, and the process survives to take the next call */
final case class Condition(kind: String, message: String)

enum REval[+A] derives okay.Effect:
  case Call(fn: String, args: Vector[RValue])
    extends REval[Either[Condition, RValue]]
  case Frame(fn: String, in: RFrame, args: Vector[RValue])
    extends REval[Either[Condition, RFrame]]
  /** a call that may CALL BACK (foreign-callbacks): R may use
   * `okay_call(name, ...)` for the names offered here */
  case Start(fn: String, args: Vector[RValue], callbacks: Vector[String])
    extends REval[RStep]
  /** the answer to an `Ask`, resuming the R frame waiting in `okay_call` */
  case Resume(k: Long, answer: Either[Condition, RValue])
    extends REval[RStep]
  /** call `fn` and KEEP its result in the R process (foreign-object-handles) */
  case Hold(fn: String, args: Vector[RValue]) extends REval[Either[Condition, RRef]]
  /** drop a held object; idempotent */
  case Release(ref: RRef) extends REval[Unit]
  /** start an R program-as-data (remote-foreign) under the host's run id */
  case Program(run: Long, fn: String, args: Vector[RValue]) extends REval[Either[Condition, RNode]]
  /** continue run `run` at continuation `k`; R keeps it, so again is fine */
  case Continue(run: Long, k: Long, answer: RValue) extends REval[Either[Condition, RNode]]
  /** drop every continuation of a run; idempotent */
  case Forget(run: Long) extends REval[Unit]

/** one node of an R program-as-data (remote-foreign) */
enum RNode:
  case Done(value: RValue)
  case Perform(name: String, args: Vector[RValue], k: Long)

/** where an R call with callbacks stands (foreign-callbacks) */
enum RStep:
  case Done(answer: Either[Condition, RValue])
  case Ask(callback: String, args: Vector[RValue], k: Long)

object REval:
  /**
   * `Durable` journals an R call (foreign-journalled,
   * specs/foreign-highlevel.md stage 1) — okay-py's instance, with R's
   * values: the answer is written in this module's wire JSON, so NA and
   * NULL come back distinct. The fingerprint is the function plus a
   * SHA-256 of what it was ASKED, never the frame it answers.
   * `withKey` is the identity: an R call has nowhere to carry a key, so
   * it must not be declared `OnRepeat.WithKey`.
   */
  given okay.codec.Journalled[REval] with
    def name[A](op: REval[A]): String = op match
      case Call(fn, _) => fn
      case Frame(fn, _, _) => fn
      case Start(fn, _, _) => fn
      case Resume(_, _) => "resume"
      case Hold(fn, _) => s"hold:$fn"
      case Release(_) => "release"
      case Program(_, fn, _) => s"program:$fn"
      case Continue(_, _, _) => "continue"
      case Forget(_) => "forget"
    def fingerprint[A](op: REval[A]): String = op match
      case Call(fn, args) => s"$fn#${Wire.digest(Json.JArr(args.map(Wire.enc)))}"
      case Frame(fn, in, args) =>
        s"$fn#${Wire.digest(Json.JArr(Vector(Wire.encFrame(in), Json.JArr(args.map(Wire.enc)))))}"
      case Start(fn, args, cbs) =>
        s"$fn#${Wire.digest(Json.JArr(Vector(Json.JArr(args.map(Wire.enc)), Json.JArr(cbs.map(Json.JStr(_))))))}"
      case Resume(k, answer) => s"resume/$k#${Wire.digest(Json.parse(Wire.written(answer.map(Wire.enc))))}"
      case Hold(fn, args) => s"hold:$fn#${Wire.digest(Json.JArr(args.map(Wire.enc)))}"
      case Release(r) => s"release:${r.id}"
      case Program(run, fn, args) => s"program:$run:$fn#${Wire.digest(Json.JArr(args.map(Wire.enc)))}"
      case Continue(run, k, a) => s"continue:$run/$k#${Wire.digest(Wire.enc(a))}"
      case Forget(run) => s"forget:$run"
    def withKey[A](op: REval[A], key: String): REval[A] = op
    def perform[A](op: REval[A], inner: okay.Handler[REval]): (A, String) = op match
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
        (answer, Wire.written(answer.map(r => Wire.enc(RValue.Ref(r)))))
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
    def decode[A](op: REval[A], written: String): A = op match
      case Call(_, _) => Wire.read(written).map(Wire.dec)
      case Frame(_, _, _) => Wire.read(written).flatMap(Wire.decFrame)
      case Start(_, _, _) => Wire.readStep(written)
      case Resume(_, _) => Wire.readStep(written)
      case Hold(_, _) => Wire.read(written).flatMap(j => Wire.asRef(Wire.dec(j)))
      case Release(_) => ()
      case Program(_, _, _) => Wire.read(written).flatMap(Wire.decNode)
      case Continue(_, _, _) => Wire.read(written).flatMap(Wire.decNode)
      case Forget(_) => ()


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
    case other => throw IllegalStateException(s"okay.r: not a journalled answer: ${Json.print(other)}")

  /** a journalled step: `{"done": <written answer>}` or `{"ask": {...}}` */
  def writtenStep(step: RStep): String = Json.print(step match
    case RStep.Done(a) => Json.JObj(Vector("done" -> Json.JStr(written(a.map(enc)))))
    case RStep.Ask(cb, args, k) => Json.JObj(Vector("ask" -> Json.JObj(Vector(
      "cb" -> Json.JStr(cb), "args" -> Json.JArr(args.map(enc)), "k" -> Json.JStr(k.toString))))))

  def readStep(w: String): RStep = Json.parse(w) match
    case Json.JObj(Vector(("done", Json.JStr(a)))) => RStep.Done(read(a).map(dec))
    case j => step(j).getOrElse(throw IllegalStateException(s"okay.r: not a journalled step: $w"))

  /** an `ask` message from the shim (or the journal), if this is one.
   * jsonlite unboxes a length-1 vector, so `cb` and `k` may arrive as
   * scalars or as one-element arrays */
  def step(j: Json): Option[RStep.Ask] = j match
    case Json.JObj(fs) => fs.collectFirst { case ("ask", Json.JObj(a)) =>
      val m = a.toMap
      def unbox(x: Option[Json]) = x match
        case Some(Json.JArr(Vector(one))) => Some(one)
        case other => other
      RStep.Ask(
        unbox(m.get("cb")).collect { case Json.JStr(c) => c }.getOrElse(""),
        m.get("args").collect { case Json.JArr(xs) => xs.map(dec) }.getOrElse(Vector.empty),
        unbox(m.get("k")).collect {
          case Json.JNum(n) => n.toLong
          case Json.JStr(n) => n.toLong
        }.getOrElse(-1L))
    }
    case _ => None

  /** a program node on the wire; jsonlite may box a scalar, so the name
   * and the k are read either way */
  def encNode(n: RNode): Json = n match
    case RNode.Done(v) => Json.JObj(Vector("done" -> enc(v)))
    case RNode.Perform(name, args, k) => Json.JObj(Vector(
      "perform" -> Json.JStr(name), "args" -> Json.JArr(args.map(enc)), "k" -> Json.JNum(k.toDouble)))

  def decNode(j: Json): Either[Condition, RNode] = j match
    case Json.JObj(fs) =>
      val m = fs.toMap
      def unbox(x: Option[Json]) = x match
        case Some(Json.JArr(Vector(one))) => Some(one)
        case other => other
      (m.get("done"), unbox(m.get("perform"))) match
        case (Some(v), _) => Right(RNode.Done(dec(v)))
        case (_, Some(Json.JStr(name))) =>
          val args = m.get("args").collect { case Json.JArr(xs) => xs.map(dec) }.getOrElse(Vector.empty)
          unbox(m.get("k")) match
            case Some(Json.JNum(k)) => Right(RNode.Perform(name, args, k.toLong))
            case _ => Left(Condition("WireError", s"a perform without a continuation: $j"))
        case _ => Left(Condition("WireError", s"not a program node: $j"))
    case other => Left(Condition("WireError", s"not a program node: $other"))

  /** a held object's handle, or the refusal of an answer that is not one */
  def asRef(v: RValue): Either[Condition, RRef] = v match
    case RValue.Ref(r) => Right(r)
    case other => Left(Condition("WireError", s"expected a held object, got $other"))

  /** SHA-256 of a value's printed JSON, hex */
  def digest(j: Json): String =
    java.security.MessageDigest.getInstance("SHA-256").nn
      .digest(Json.print(j).getBytes(java.nio.charset.StandardCharsets.UTF_8)).nn
      .map(b => f"${b & 0xff}%02x").mkString

  /**
   * `RValue.Vec`/`Json.JArr` recurse on the VALUE's own nesting — an
   * R `list()` nests as deep as a script chooses, and this is the
   * boundary an arbitrarily-deep R return value crosses
   * (subprocess-wire-depth-safety, the same defect shape
   * `okay-mcp/Rpc.damaged`/`okay-demo/StateMcp.damaged` already had
   * fixed twice, and `okay-py/Py.scala`'s own `Wire.enc`/`dec` just
   * fixed alongside this one). An explicit work-list, not native
   * recursion: `todo` holds nodes still to visit and `Combine(n)`
   * markers saying "the last n results belong to one vector, in
   * order"; `results` accumulates finished values, most recent first,
   * so `Combine` reverses its slice before rebuilding the vector.
   */
  def enc(v0: RValue): Json =
    def leaf(v: RValue): Json = v match
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
      case RValue.Ref(r) => tagged("ref", "id" -> Json.JNum(r.id.toDouble), "type" -> Json.JStr(r.rClass))
      case RValue.Vec(_) | RValue.Named(_) =>
        throw IllegalStateException("unreachable: containers are handled by the work-list")

    enum Step:
      case Todo(v: RValue)
      case Combine(n: Int)
      case CombineNamed(keys: Vector[String])

    var todo = List[Step](Step.Todo(v0))
    var results = List.empty[Json]
    while todo.nonEmpty do
      todo.head match
        case Step.Todo(RValue.Vec(xs)) =>
          todo = xs.toList.map(Step.Todo(_)) ::: Step.Combine(xs.length) :: todo.tail
        case Step.Todo(RValue.Named(kv)) =>
          todo = kv.toList.map(p => Step.Todo(p._2)) ::: Step.CombineNamed(kv.map(_._1)) :: todo.tail
        case Step.Todo(other) =>
          results = leaf(other) :: results
          todo = todo.tail
        case Step.Combine(n) =>
          val (items, rest) = results.splitAt(n)
          results = Json.JArr(items.reverse.toVector) :: rest
          todo = todo.tail
        case Step.CombineNamed(keys) =>
          val (items, rest) = results.splitAt(keys.length)
          val pairs = keys.zip(items.reverse).map((k, v) => Json.JArr(Vector(Json.JStr(k), v)))
          results = tagged("named", "kv" -> Json.JArr(pairs)) :: rest
          todo = todo.tail
    results.head

  private def tagged(t: String, fields: (String, Json)*): Json =
    Json.JObj(("t" -> Json.JStr(t)) +: fields.toVector)

  /** the frame format's own version inside the envelope
   * (r-frame-columnar-wire): 2 is the columnar shape */
  val FrameFormat = 2

  /**
   * A frame, COLUMNAR (r-frame-columnar-wire): the type tag belongs to
   * the column, and the values are a plain array — the path jsonlite
   * takes fastest, which is the whole reason this shape exists.
   * Absences are index lists beside the values, so the four NAs stay
   * four and NA stays apart from NaN without a tag per cell.
   *
   * The placeholder at an absent position is the type's ZERO and not
   * JSON null on purpose: a null in a numeric array makes jsonlite
   * build a list, which is exactly the slow path being escaped. The
   * reader takes `na`/`nan` as the authority and never the value
   * standing at those positions.
   *
   * A column the four atomic types cannot carry — raw bytes, anything
   * mixed — keeps the per-cell form under `cells`, so generality is
   * not traded for the fast path; both readers accept either key.
   */
  def encFrame(f: RFrame): Json = Json.JObj(Vector(
    "t" -> Json.JStr("frame"),
    "v" -> Json.JNum(FrameFormat.toDouble),
    "cols" -> Json.JArr(f.cols.map((n, col) => encCol(n, col)))))

  private def encCol(name: String, col: Vector[RValue]): Json =
    columnType(col) match
      case None =>
        Json.JObj(Vector("name" -> Json.JStr(name), "cells" -> Json.JArr(col.map(enc))))
      case Some(t) =>
        val na = Vector.newBuilder[Json]
        val nan = Vector.newBuilder[Json]
        val values = col.zipWithIndex.map { (v, i) =>
          v match
            case RValue.NA(_) | RValue.RNull => na += Json.JNum(i.toDouble); zeroOf(t)
            case RValue.F64(d) if d.isNaN => nan += Json.JNum(i.toDouble); zeroOf(t)
            case RValue.Bool(b) => Json.JBool(b)
            case RValue.I32(x) => Json.JNum(x.toDouble)
            case RValue.F64(x) => Json.JNum(x)
            case RValue.Str(s) => Json.JStr(s)
            case _ => zeroOf(t)   // unreachable: columnType admitted the column
        }
        val fields = Vector(
          "name" -> Json.JStr(name),
          "type" -> Json.JStr(t.rName.take(1) match { case "c" => "s"; case c => c }),
          "values" -> Json.JArr(values),
          "na" -> Json.JArr(na.result()))
        Json.JObj(if t == RType.Double then fields :+ ("nan" -> Json.JArr(nan.result())) else fields)

  /** the column's ONE type, when the four atomic ones can carry it:
   * an NA names its own type, a value names its own, and a column
   * that mixes them (or holds bytes, or is empty) has none — an empty
   * column is `logical` on this wire, which is what R's own `c()`
   * gives, and the reason a type does not survive an empty column is
   * stated in specs/r.md */
  private def columnType(col: Vector[RValue]): Option[RType] =
    var seen: Option[RType] = None
    var ok = true
    col.foreach { v =>
      val t = v match
        case RValue.NA(t) => Some(t)
        case RValue.Bool(_) => Some(RType.Logical)
        case RValue.I32(_) => Some(RType.Integer)
        case RValue.F64(_) => Some(RType.Double)
        case RValue.Str(_) => Some(RType.Character)
        case RValue.RNull => None            // absent, takes the column's type
        case _ => ok = false; None
      (seen, t) match
        case (_, None) => ()
        case (None, Some(x)) => seen = Some(x)
        case (Some(a), Some(b)) if a == b => ()
        case _ => ok = false
    }
    if !ok then None else Some(seen.getOrElse(RType.Logical))

  private def zeroOf(t: RType): Json = t match
    case RType.Logical => Json.JBool(false)
    case RType.Integer | RType.Double => Json.JNum(0)
    case RType.Character => Json.JStr("")

  /** `enc`'s mirror — same work-list, same reasoning */
  def dec(j0: Json): RValue =
    def leaf(j: Json): RValue = j match
      case Json.JNull => RValue.RNull
      case Json.JBool(b) => RValue.Bool(b)
      case Json.JNum(n) => RValue.F64(n)
      case Json.JStr(s) => RValue.Str(s)
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
          // jsonlite may unbox or box a scalar: read the id and class either way
          case Some("ref") =>
            val id = m.get("id").collect { case Json.JNum(n) => n; case Json.JArr(Vector(Json.JNum(n))) => n }
            val cls = m.get("type").collect { case Json.JStr(t) => t; case Json.JArr(Vector(Json.JStr(t))) => t }
            id.map(i => RValue.Ref(RRef(i.toLong, cls.getOrElse("?")))).getOrElse(RValue.RNull)
          case _ => RValue.RNull      // an untagged object has no RValue shape
      case Json.JErr(_) => RValue.RNull
      case Json.JArr(_) => throw IllegalStateException("unreachable: JArr is handled by the work-list")

    enum Step:
      case Todo(j: Json)
      case Combine(n: Int)
      case CombineNamed(keys: Vector[String])

    def namedPairs(fs: Vector[(String, Json)]): Option[Vector[(String, Json)]] =
      if !fs.exists(_ == ("t" -> Json.JStr("named"))) then None
      else fs.collectFirst { case ("kv", Json.JArr(ps)) => ps.collect {
        case Json.JArr(Vector(Json.JStr(k), v)) => (k, v)
      } }

    var todo = List[Step](Step.Todo(j0))
    var results = List.empty[RValue]
    while todo.nonEmpty do
      todo.head match
        case Step.Todo(Json.JArr(xs)) =>
          todo = xs.toList.map(Step.Todo(_)) ::: Step.Combine(xs.length) :: todo.tail
        case Step.Todo(o @ Json.JObj(fs)) =>
          namedPairs(fs) match
            case Some(kv) =>
              todo = kv.toList.map(p => Step.Todo(p._2)) ::: Step.CombineNamed(kv.map(_._1)) :: todo.tail
            case None =>
              results = leaf(o) :: results
              todo = todo.tail
        case Step.Todo(other) =>
          results = leaf(other) :: results
          todo = todo.tail
        case Step.Combine(n) =>
          val (items, rest) = results.splitAt(n)
          results = RValue.Vec(items.reverse.toVector) :: rest
          todo = todo.tail
        case Step.CombineNamed(keys) =>
          val (items, rest) = results.splitAt(keys.length)
          results = RValue.Named(keys.zip(items.reverse)) :: rest
          todo = todo.tail
    results.head

  def decFrame(j: Json): Either[Condition, RFrame] = j match
    case Json.JObj(fs) if fs.toMap.get("t").contains(Json.JStr("frame")) =>
      val m = fs.toMap
      val v = m.get("v").collect { case Json.JNum(n) => n.toInt }.getOrElse(FrameFormat)
      if v > FrameFormat then
        Left(Condition("WireError",
          s"frame format v$v: this host reads up to v$FrameFormat — refuse rather than guess"))
      else m.get("cols") match
        case Some(Json.JArr(cols)) =>
          val out = cols.map(decCol)
          out.collectFirst { case Left(c) => c } match
            case Some(c) => Left(c)
            case None => Right(RFrame(out.collect { case Right(p) => p }))
        case _ => Left(Condition("WireError", "a frame without cols"))
    case other => Left(Condition("WireError", s"expected a frame, got $other"))

  private def decCol(j: Json): Either[Condition, (String, Vector[RValue])] = j match
    case Json.JObj(fs) =>
      val m = fs.toMap
      val name = m.get("name").collect { case Json.JStr(s) => s }.getOrElse("")
      (m.get("cells"), m.get("values")) match
        case (Some(cells), _) => Right(name -> asArray(cells).map(dec))
        case (_, Some(rawValues)) =>
          val values = asArray(rawValues)
          val t = m.get("type").collect { case Json.JStr(s) => s }.flatMap(shortType)
            .getOrElse(RType.Logical)
          def idx(k: String): Set[Int] =
            m.get(k).map(asArray).getOrElse(Vector.empty)
              .collect { case Json.JNum(n) => n.toInt }.toSet
          val na = idx("na")
          val nan = idx("nan")
          Right(name -> values.zipWithIndex.map { (x, i) =>
            if na(i) then RValue.NA(t)
            else if nan(i) then RValue.F64(Double.NaN)
            else atType(t, x)
          })
        case _ => Left(Condition("WireError", s"a column with neither values nor cells: $j"))
    case Json.JArr(Vector(Json.JStr(n), Json.JArr(vals))) =>
      // the v1 shape, still read: a host may meet its own older frame
      // in a journal or a test fixture, and reading is free
      Right(n -> vals.map(dec))
    case other => Left(Condition("WireError", s"not a column: $other"))

  /** jsonlite UNBOXES a length-1 vector: a one-row column arrives as a
   * scalar and a single absence as a bare number, not as arrays of one
   * (r-frame-columnar-wire — the round trip lost exactly one NA before
   * this was read rather than assumed) */
  private def asArray(j: Json): Vector[Json] = j match
    case Json.JArr(xs) => xs
    case Json.JNull => Vector.empty
    case one => Vector(one)

  private def shortType(s: String): Option[RType] = s match
    case "l" => Some(RType.Logical)
    case "i" => Some(RType.Integer)
    case "d" => Some(RType.Double)
    case "s" => Some(RType.Character)
    case _ => None

  /** a plain value at the column's type; jsonlite may hand a whole
   * numeric column back as doubles, so an integer column re-reads its
   * own type rather than trusting the JSON number's shape */
  private def atType(t: RType, j: Json): RValue = (t, j) match
    case (RType.Logical, Json.JBool(b)) => RValue.Bool(b)
    case (RType.Integer, Json.JNum(n)) => RValue.I32(n.toInt)
    case (RType.Double, Json.JNum(n)) => RValue.F64(n)
    case (RType.Character, Json.JStr(s)) => RValue.Str(s)
    case (_, other) => dec(other)
}
