package okay.codec

/**
 * Does the other side still read our messages?
 *
 * Two services share a Schema-encoded message; one of them changes
 * its case class. Nothing here answered whether the other can still
 * decode, and the usual industry answer is a broker (a schema
 * registry) or a contract-testing tool. Neither is needed: a
 * `Schema` IS a value, so the question is a fold over two of them —
 * and the answer is not invented, it is READ OFF the decoders in
 * this same module (Json.decode, Cbor.get):
 *
 *  - an absent field takes its declared default, then
 *    None-if-optional, then a refusal by name;
 *  - an unknown case is a refusal, on both wires;
 *  - an unknown FIELD is SKIPPED, on both wires.
 *
 * That last line used to read differently, and the history is worth
 * a sentence. When this was first written, Json skipped an unknown
 * field and Cbor refused it, so a verdict had to name its WIRE. The
 * check reported the difference faithfully — and reporting it is what
 * made someone look at it. Nothing had chosen it: no test pinned the
 * refusal and no spec stated it. It was fixed (cbor-unknown-fields),
 * the two wires now answer alike, and the `Wire` parameter went with
 * the defect it existed to describe.
 *
 * Two directions, and they are not the same question:
 *
 *  - BACKWARD: the NEW reader over OLD bytes — "can we deploy this
 *    version and still read the log / the other service's traffic?"
 *  - FORWARD: the OLD reader over NEW bytes — "can we deploy this
 *    version before every consumer is upgraded?"
 *
 * A rolling deploy needs both; a log needs backward for ever.
 */
object Compat:

  /** where in the message the change is: field and case names from
    * the root, `""` for the root itself */
  type Path = String

  enum Change:
    case FieldAdded(path: Path, field: String, optional: Boolean, defaulted: Boolean)
    case FieldRemoved(path: Path, field: String, optional: Boolean, defaulted: Boolean)
    case CaseAdded(path: Path, name: String)
    case CaseRemoved(path: Path, name: String)
    /** the same field or case, a different shape underneath */
    case TypeChanged(path: Path, from: String, to: String)
    /** a product where a sum was, an Int where a String was */
    case ShapeChanged(path: Path, from: String, to: String)

    def path0: Path = this match
      case FieldAdded(p, _, _, _) => p
      case FieldRemoved(p, _, _, _) => p
      case CaseAdded(p, _) => p
      case CaseRemoved(p, _) => p
      case TypeChanged(p, _, _) => p
      case ShapeChanged(p, _, _) => p

    /** why a direction breaks, or None when this change is safe there.
      * `newReader` = backward (new schema reading old bytes). */
    def breaks(newReader: Boolean): Option[String] = this match
      case FieldAdded(p, f, optional, defaulted) =>
        if newReader then
          // old bytes lack the field: the decoder needs a fallback
          if defaulted || optional then None
          else Some(s"$p$f is new and required: old bytes have no value for it")
        else None                       // an old reader skips what it does not declare
      case FieldRemoved(p, f, optional, defaulted) =>
        if newReader then None          // the new reader skips it
        else if defaulted || optional then None
        else Some(s"$p$f is gone and was required: the old reader has no value for it")
      case CaseAdded(p, n) =>
        if newReader then None
        else Some(s"$p$n is a new case: the old reader refuses a case it does not know")
      case CaseRemoved(p, n) =>
        if newReader then Some(s"$p$n is gone: this reader refuses a case it does not know")
        else None
      case TypeChanged(p, from, to) => Some(s"$p changed from $from to $to")
      case ShapeChanged(p, from, to) => Some(s"$p changed from $from to $to")

  /** what one direction on one wire answers */
  final case class Verdict(compatible: Boolean, reasons: Vector[String]):
    def and(o: Verdict): Verdict = Verdict(compatible && o.compatible, reasons ++ o.reasons)
  object Verdict:
    val ok: Verdict = Verdict(true, Vector.empty)

  /** the whole answer: what changed, and what each direction says on
    * each wire */
  final case class Report(changes: Vector[Change]):
    /** the NEW reader over OLD bytes */
    def backward: Verdict = verdict(newReader = true)
    /** the OLD reader over NEW bytes */
    def forward: Verdict = verdict(newReader = false)
    /** both directions — what a rolling deploy needs */
    def rolling: Verdict = backward.and(forward)

    private def verdict(newReader: Boolean): Verdict =
      val why = changes.flatMap(_.breaks(newReader))
      Verdict(why.isEmpty, why)

    def isEmpty: Boolean = changes.isEmpty

    /** the report an operator reads: the changes, then each verdict */
    def render: String =
      val sb = new StringBuilder
      if changes.isEmpty then sb ++= "no change\n"
      else
        sb ++= s"${changes.size} change(s):\n"
        changes.foreach(c => sb ++= s"  $c\n")
      def line(what: String, v: Verdict): Unit =
        sb ++= s"$what: ${if v.compatible then "compatible" else "INCOMPATIBLE"}\n"
        v.reasons.foreach(r => sb ++= s"    $r\n")
      line("backward (new reader, old bytes)", backward)
      line("forward (old reader, new bytes)", forward)
      sb.result()

  /** every change from `old` to `next`, deepest paths included */
  def compare[A, B](old: Schema[A], next: Schema[B]): Report =
    Report(walk(old, next, "", Set.empty))

  /** the shape word a report names a schema by */
  def shape(s: Schema[?]): String = s match
    case Schema.SInt => "Int"
    case Schema.SLong => "Long"
    case Schema.SDouble => "Double"
    case Schema.SBool => "Boolean"
    case Schema.SString => "String"
    case Schema.SChar => "Char"
    case Schema.SBytes => "Bytes"
    case Schema.SOption(of) => s"Option[${shape(of())}]"
    case Schema.SList(of) => s"List[${shape(of())}]"
    case Schema.SVector(of) => s"Vector[${shape(of())}]"
    case p: Schema.SProduct[?] => p.name
    case su: Schema.SSum[?] => su.name
    case Schema.SIso(u, _, _) => shape(u())

  private def under(s: Schema[?]): Schema[?] = s match
    case Schema.SIso(u, _, _) => under(u())   // a wrapper does not exist to the wire
    case other => other

  private def at(path: String, name: String): String = if path.isEmpty then s"$name" else s"$path.$name"

  /** the recursion guard: a schema's thunks are re-entered, so a
    * self-referential type (a tree) would walk for ever. A pair of
    * names seen once is not walked twice — the fields under it were
    * compared the first time. */
  private def walk(a: Schema[?], b: Schema[?], path: String, seen: Set[(String, String)]): Vector[Change] =
    (under(a), under(b)) match
      case (x, y) if x == y => Vector.empty
      case (Schema.SOption(x), Schema.SOption(y)) => walk(x(), y(), path, seen)
      case (Schema.SList(x), Schema.SList(y)) => walk(x(), y(), path, seen)
      case (Schema.SVector(x), Schema.SVector(y)) => walk(x(), y(), path, seen)
      case (Schema.SList(x), Schema.SVector(y)) => walk(x(), y(), path, seen)   // both are arrays on both wires
      case (Schema.SVector(x), Schema.SList(y)) => walk(x(), y(), path, seen)

      case (p: Schema.SProduct[?], q: Schema.SProduct[?]) =>
        val key = (p.name, q.name)
        if seen(key) then Vector.empty
        else
          val seen2 = seen + key
          val prefix = if path.isEmpty then "" else s"$path."
          val oldF = p.fields.map(_._1)
          val newF = q.fields.map(_._1)
          val added = q.fields.zipWithIndex.collect {
            case ((n, sc), i) if !oldF.contains(n) =>
              Change.FieldAdded(prefix, n, sc().isInstanceOf[Schema.SOption[?]], q.defaults.lift(i).flatten.isDefined)
          }
          val removed = p.fields.zipWithIndex.collect {
            case ((n, sc), i) if !newF.contains(n) =>
              Change.FieldRemoved(prefix, n, sc().isInstanceOf[Schema.SOption[?]], p.defaults.lift(i).flatten.isDefined)
          }
          val common = p.fields.flatMap { (n, sc) =>
            q.fields.find(_._1 == n).toVector.flatMap((_, sc2) => compareField(sc(), sc2(), at(path, n), seen2))
          }
          added ++ removed ++ common

      case (s1: Schema.SSum[?], s2: Schema.SSum[?]) =>
        val key = (s1.name, s2.name)
        if seen(key) then Vector.empty
        else
          val seen2 = seen + key
          val prefix = if path.isEmpty then "" else s"$path."
          val oldC = s1.cases.map(_._1)
          val newC = s2.cases.map(_._1)
          val added = s2.cases.collect { case (n, _) if !oldC.contains(n) => Change.CaseAdded(prefix, n) }
          val removed = s1.cases.collect { case (n, _) if !newC.contains(n) => Change.CaseRemoved(prefix, n) }
          val common = s1.cases.flatMap { (n, sc) =>
            s2.cases.find(_._1 == n).toVector.flatMap((_, sc2) => walk(sc(), sc2(), at(path, n), seen2))
          }
          added ++ removed ++ common

      case (x, y) =>
        Vector(Change.ShapeChanged(if path.isEmpty then "the root" else path, shape(x), shape(y)))

  /** a field's own change: a differing PRIMITIVE is a type change (the
    * wire value stops decoding), anything structured recurses */
  private def compareField(a: Schema[?], b: Schema[?], path: String, seen: Set[(String, String)]): Vector[Change] =
    (under(a), under(b)) match
      case (x, y) if x == y => Vector.empty
      case (x, y) if primitive(x) && primitive(y) => Vector(Change.TypeChanged(path, shape(x), shape(y)))
      // Option[X] to X and back: the wire value is the same when
      // present, and the difference is exactly the absent case
      case (Schema.SOption(x), y) if !y.isInstanceOf[Schema.SOption[?]] =>
        Vector(Change.TypeChanged(path, s"Option[${shape(x())}]", shape(y))) ++ walk(x(), y, path, seen)
      case (x, Schema.SOption(y)) if !x.isInstanceOf[Schema.SOption[?]] =>
        Vector(Change.TypeChanged(path, shape(x), s"Option[${shape(y())}]")) ++ walk(x, y(), path, seen)
      case (x, y) => walk(x, y, path, seen)

  private def primitive(s: Schema[?]): Boolean = s match
    case Schema.SInt | Schema.SLong | Schema.SDouble | Schema.SBool
       | Schema.SString | Schema.SChar | Schema.SBytes => true
    case _ => false
