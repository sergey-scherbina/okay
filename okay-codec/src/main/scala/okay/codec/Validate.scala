package okay.codec

/**
 * The accumulating decoder — the ninth algebra over `Schema`
 * (specs/schema-fold.md, stage 3). `Json.decode` is MONADIC: the
 * first refusal is the answer, one message, no path — which is what a
 * wire wants. A form, a config, a tool's argument check want the
 * APPLICATIVE reading of the same schema: EVERY refusal, each at its
 * dotted path, and the typed value when there is none. Same rules,
 * read off `Json.decodeNative` line by line — a default, then
 * None-if-optional, then "missing field"; a damaged optional is
 * absent; a damaged list element is skipped, the ones that arrived
 * survive; a sum is a one-entry object tagged by its case; a wrapper
 * decodes what it wraps and then refines — and one difference, which
 * is the point: a refusal does not stop the walk.
 *
 * A fold on `Schema.Step`, so depth is `Step`'s business. `Step`'s
 * answer type is ONE type for the whole walk, so a node answers
 * `Either[Errors, Any]` and the value is re-stated at its type in two
 * places only, both marked: the root (the `Any` a root schema folded
 * for IS its `A`) and a wrapper (`iso.to` wants the `B` its `under`
 * folded for). The same class as `fold`'s own casts — restoring what
 * a uniform type erased, never a runtime test on a value's shape.
 */
object Validate:
  type Path = String
  type Errors = Vector[(Path, String)]

  /** every error at its path, or the value */
  def decode[A](s: Schema[A])(j: Json): Either[Errors, A] =
    Schema.Step.walk(validator(s), Nil, j) match
      case Right(a) => Right(a.asInstanceOf[A])
      case Left(es) => Left(es)

  /** the paths and messages alone — what a form renders under its fields */
  def errors[A](s: Schema[A])(j: Json): Errors = decode(s)(j).left.getOrElse(Vector.empty)

  private type Out = Either[Errors, Any]
  private type Acc = Either[Errors, Vector[Any]]
  /** the path as it is walked: segments, innermost first — a cons per
    * level, shared with the parent, rendered to its dotted form only
    * when an error is recorded. A dotted STRING per level is O(depth²)
    * bytes (the same finding as Form.render's keys, TestFormDepth), and
    * a 100 000-level value exhausted the heap on exactly that. */
  private type At = List[String]
  private type Val[A] = Schema.Step[At, Json, Out]

  private def at(k: At, n: String): At = n :: k
  private def index(k: At, i: Int): At = k match
    case h :: t => s"$h[$i]" :: t
    case Nil => s"[$i]" :: Nil
  private def dotted(k: At): Path = k.reverse.mkString(".")
  private def one(k: At, m: String): Out = Left(Vector(dotted(k) -> m))
  /** the applicative step: both sides' errors survive */
  private def gather(acc: Acc, r: Out): Acc = (acc, r) match
    case (Right(xs), Right(a)) => Right(xs :+ a)
    case (Left(e1), Left(e2)) => Left(e1 ++ e2)
    case (Left(e), _) => Left(e)
    case (_, Left(e)) => Left(e)

  private val validator = Schema.Folded[Val](new Schema.Algebra[Val]:
    import Schema.Step
    import Json.*

    private def leaf(want: String)(f: PartialFunction[Json, Out]) =
      Step.leaf[At, Json, Out]((k, j) => j match
        case JErr(m) => one(k, m)
        case _ if f.isDefinedAt(j) => f(j)
        case got => one(k, s"expected $want, got $got"))
    def int = leaf("SInt") { case JNum(n) => Right(n.toInt) }
    def long = leaf("SLong") { case JNum(n) => Right(n.toLong) }
    def double = leaf("SDouble") { case JNum(n) => Right(n) }
    def bool = leaf("SBool") { case JBool(b) => Right(b) }
    def string = leaf("SString") { case JStr(x) => Right(x) }
    def char = Step.leaf[At, Json, Out]((k, j) => j match
      case JStr(x) if x.length == 1 => Right(x.head)
      case JStr(x) => one(k, s"expected one character, got ${x.length}")
      case JErr(m) => one(k, m)
      case got => one(k, s"expected SChar, got $got"))
    def bytes = Step.leaf[At, Json, Out]((k, j) => j match
      case JStr(x) => Base64.decode(x).left.map(m => Vector(dotted(k) -> m))
      case JErr(m) => one(k, m)
      case got => one(k, s"expected SBytes, got $got"))

    def option[A](o: Schema.SOption[A], of: () => Val[A]) =
      Step.node[At, Json, Out, Out](
        (_, _) => Right(None),
        (k, j) => j match
          case JNull => Vector.empty
          case v => Vector(Step.Kid(of(), k, v)),
        (_, r) => r.map(Some(_)),
        (_, _, s) => s)

    private def elements(want: String, each: () => Val[?], finish: Vector[Any] => Any) =
      Step.node[At, Json, Out, Acc](
        (_, _) => Right(Vector.empty),
        (k, j) => j match
          // a damaged element is skipped, the ones that arrived
          // survive — the SList rule, exactly as decode has it
          case JArr(vs) => vs.zipWithIndex.collect { case (v, i) if !v.isInstanceOf[JErr] => Step.Kid(each(), index(k, i), v) }
          case _ => Vector.empty,
        gather,
        (k, j, s) => j match
          case JArr(_) => s.map(finish)
          case JErr(m) => one(k, m)
          case got => one(k, s"expected $want, got $got"))
    def list[A](l: Schema.SList[A], of: () => Val[A]) = elements("SList", of, _.toList)
    def vector[A](v: Schema.SVector[A], of: () => Val[A]) = elements("SVector", of, identity)

    def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[Val, Any])]) =
      // an absent (or damaged-optional) field takes, in order: its
      // DECLARED default, None-if-optional, the missing refusal —
      // as a constant kid, so the field keeps its position for `make`
      def absent(k: At, i: Int): Step.Kid[At, Json, Out] =
        val (n, _) = fields(i)
        val r: Out = p.defaults.lift(i).flatten match
          case Some(d) => Right(d())
          case None => p.fields(i)._2() match
            case _: Schema.SOption[?] => Right(None)
            case _ => one(at(k, n), s"missing field '$n' in ${p.name}")
        Step.Kid(Step.leaf[At, Json, Out]((_, _) => r), k, JNull)
      Step.node[At, Json, Out, Acc](
        (_, _) => Right(Vector.empty),
        (k, j) => j match
          case JObj(fs) =>
            val m = fs.toMap
            fields.zipWithIndex.map { case ((n, edge), i) =>
              (m.get(n), p.fields(i)._2()) match
                case (None, _) => absent(k, i)
                case (Some(JErr(_)), _: Schema.SOption[?]) => absent(k, i)
                case (Some(v), _) => Step.Kid(edge(), at(k, n), v)
            }
          case _ => Vector.empty,
        gather,
        (k, j, s) => j match
          case JObj(_) => s.map(xs => p.make(xs))
          case JErr(m) => one(k, m)
          case got => one(k, s"expected ${p.name}, got $got"))

    def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[Val, A])]) =
      Step.node[At, Json, Out, Out](
        (k, j) => j match
          case JObj(Vector((name, _))) =>
            if su.cases.exists(_._1 == name) then Right(null)   // replaced by the case's answer
            else one(k, s"unknown case '$name' of ${su.name}")
          case JErr(m) => one(k, m)
          case got => one(k, s"expected ${su.name}, got $got"),
        (k, j) => j match
          case JObj(Vector((name, v))) =>
            val i = su.cases.indexWhere(_._1 == name)
            if i < 0 then Vector.empty else Vector(Step.Kid(cases(i)._2(), k, v))
          case _ => Vector.empty,
        (_, r) => r,
        (_, _, s) => s)

    /** the wrapper decodes what it wraps, then refines — `to`'s Left
      * is a refusal at this path. The `B` is the one `under` folded
      * for, re-stated: `Step`'s uniform answer erased it. */
    def iso[A, B](iso: Schema.SIso[A, B], under: () => Val[B]) =
      Step.node[At, Json, Out, Out](
        (_, _) => Right(null),
        (k, j) => Vector(Step.Kid(under(), k, j)),
        (_, r) => r,
        (k, _, s) => s.flatMap(b => iso.to(b.asInstanceOf[B]).left.map(m => Vector(dotted(k) -> m))))

    def ref[A](name: String) =
      throw IllegalStateException(s"a lazy carrier never meets a back edge, got one at $name")
  )
