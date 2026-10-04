package okay2.codec

/**
 * Does the other side still read our messages? (okay-codec's
 * Compat.scala.) A `Schema` IS a value, so the question is a fold over
 * two of them, and the answer is READ OFF the decoders in this module:
 *
 *  - an absent field takes its declared default, then
 *    None-if-optional, then a refusal by name;
 *  - an unknown case is a refusal, on both wires;
 *  - an unknown FIELD is SKIPPED, on both wires.
 *
 * Two directions: BACKWARD, the new reader over old bytes, and
 * FORWARD, the old reader over new bytes. A rolling deploy needs both.
 */
object Compat {

  /** where in the message the change is: field and case names from the
   * root, `""` for the root itself */
  type Path = String

  sealed trait Change {
    import Change._

    def path0: Path = this match {
      case FieldAdded(p, _, _, _) => p
      case FieldRemoved(p, _, _, _) => p
      case CaseAdded(p, _) => p
      case CaseRemoved(p, _) => p
      case TypeChanged(p, _, _) => p
      case ShapeChanged(p, _, _) => p
    }

    /** why a direction breaks, or None when this change is safe there.
     * `newReader` = backward (new schema reading old bytes). */
    def breaks(newReader: Boolean): Option[String] = this match {
      case FieldAdded(p, f, optional, defaulted) =>
        if (newReader) {
          // old bytes lack the field: the decoder needs a fallback
          if (defaulted || optional) None
          else Some(s"$p$f is new and required: old bytes have no value for it")
        } else None // an old reader skips what it does not declare
      case FieldRemoved(p, f, optional, defaulted) =>
        if (newReader) None // the new reader skips it
        else if (defaulted || optional) None
        else Some(s"$p$f is gone and was required: the old reader has no value for it")
      case CaseAdded(p, n) =>
        if (newReader) None
        else Some(s"$p$n is a new case: the old reader refuses a case it does not know")
      case CaseRemoved(p, n) =>
        if (newReader) Some(s"$p$n is gone: this reader refuses a case it does not know")
        else None
      case TypeChanged(p, from, to) => Some(s"$p changed from $from to $to")
      case ShapeChanged(p, from, to) => Some(s"$p changed from $from to $to")
    }
  }

  object Change {
    final case class FieldAdded(path: Path, field: String, optional: Boolean, defaulted: Boolean) extends Change
    final case class FieldRemoved(path: Path, field: String, optional: Boolean, defaulted: Boolean) extends Change
    final case class CaseAdded(path: Path, name: String) extends Change
    final case class CaseRemoved(path: Path, name: String) extends Change
    /** the same field or case, a different shape underneath */
    final case class TypeChanged(path: Path, from: String, to: String) extends Change
    /** a product where a sum was, an Int where a String was */
    final case class ShapeChanged(path: Path, from: String, to: String) extends Change
  }

  /** what one direction answers */
  final case class Verdict(compatible: Boolean, reasons: Vector[String]) {
    def and(o: Verdict): Verdict = Verdict(compatible && o.compatible, reasons ++ o.reasons)
  }
  object Verdict {
    val ok: Verdict = Verdict(true, Vector.empty)
  }

  /** the whole answer: what changed, and what each direction says */
  final case class Report(changes: Vector[Change]) {
    /** the NEW reader over OLD bytes */
    def backward: Verdict = verdict(newReader = true)
    /** the OLD reader over NEW bytes */
    def forward: Verdict = verdict(newReader = false)
    /** both directions — what a rolling deploy needs */
    def rolling: Verdict = backward.and(forward)

    private def verdict(newReader: Boolean): Verdict = {
      val why = changes.flatMap(_.breaks(newReader))
      Verdict(why.isEmpty, why)
    }

    def isEmpty: Boolean = changes.isEmpty

    /** the report an operator reads: the changes, then each verdict */
    def render: String = {
      val sb = new StringBuilder
      if (changes.isEmpty) sb ++= "no change\n"
      else {
        sb ++= s"${changes.size} change(s):\n"
        changes.foreach(c => sb ++= s"  $c\n")
      }
      def line(what: String, v: Verdict): Unit = {
        sb ++= s"$what: ${if (v.compatible) "compatible" else "INCOMPATIBLE"}\n"
        v.reasons.foreach(r => sb ++= s"    $r\n")
      }
      line("backward (new reader, old bytes)", backward)
      line("forward (old reader, new bytes)", forward)
      sb.result()
    }
  }

  /** every change from `old` to `next`, deepest paths included */
  def compare[A, B](old: Schema[A], next: Schema[B]): Report =
    Report(walk(old, next, "", Set.empty))

  /** the shape word a report names a schema by; recursion bounded by
   * the schema's nesting to its first product or sum, where it stops */
  def shape(s: Schema[_]): String = s match {
    case Schema.SInt => "Int"
    case Schema.SLong => "Long"
    case Schema.SDouble => "Double"
    case Schema.SBool => "Boolean"
    case Schema.SString => "String"
    case Schema.SChar => "Char"
    case Schema.SBytes => "Bytes"
    case Schema.SBigInt => "BigInt"
    case Schema.SOption(of) => s"Option[${shape(of())}]"
    case Schema.SList(of) => s"List[${shape(of())}]"
    case Schema.SVector(of) => s"Vector[${shape(of())}]"
    case p: Schema.SProduct[_] => p.name
    case su: Schema.SSum[_] => su.name
    case i: Schema.SIso[_, _] => shape(i.under())
  }

  /** a wrapper does not exist to the wire */
  @scala.annotation.tailrec
  private def under(s: Schema[_]): Schema[_] = s match {
    case i: Schema.SIso[_, _] => under(i.under())
    case other => other
  }

  private def at(path: String, name: String): String = if (path.isEmpty) s"$name" else s"$path.$name"

  /** the recursion guard: a pair of names seen once is not walked twice
   * — the fields under it were compared the first time — so a
   * self-referential type terminates, and the depth is bounded by the
   * number of distinct product/sum names in the two schemas */
  private def walk(a: Schema[_], b: Schema[_], path: String, seen: Set[(String, String)]): Vector[Change] =
    (under(a), under(b)) match {
      case (x, y) if x == y => Vector.empty
      case (Schema.SOption(x), Schema.SOption(y)) => walk(x(), y(), path, seen)
      case (Schema.SList(x), Schema.SList(y)) => walk(x(), y(), path, seen)
      case (Schema.SVector(x), Schema.SVector(y)) => walk(x(), y(), path, seen)
      case (Schema.SList(x), Schema.SVector(y)) => walk(x(), y(), path, seen) // both are arrays on both wires
      case (Schema.SVector(x), Schema.SList(y)) => walk(x(), y(), path, seen)

      case (p: Schema.SProduct[_], q: Schema.SProduct[_]) =>
        val key = (p.name, q.name)
        if (seen(key)) Vector.empty
        else {
          val seen2 = seen + key
          val prefix = if (path.isEmpty) "" else s"$path."
          val oldF = p.fields.map(_._1)
          val newF = q.fields.map(_._1)
          val added = q.fields.zipWithIndex.collect {
            case ((n, sc), i) if !oldF.contains(n) =>
              Change.FieldAdded(prefix, n, sc().isInstanceOf[Schema.SOption[_]], q.defaults.lift(i).flatten.isDefined)
          }
          val removed = p.fields.zipWithIndex.collect {
            case ((n, sc), i) if !newF.contains(n) =>
              Change.FieldRemoved(prefix, n, sc().isInstanceOf[Schema.SOption[_]], p.defaults.lift(i).flatten.isDefined)
          }
          val common = p.fields.flatMap { case (n, sc) =>
            q.fields.find(_._1 == n).toVector.flatMap { case (_, sc2) => compareField(sc(), sc2(), at(path, n), seen2) }
          }
          added ++ removed ++ common
        }

      case (s1: Schema.SSum[_], s2: Schema.SSum[_]) =>
        val key = (s1.name, s2.name)
        if (seen(key)) Vector.empty
        else {
          val seen2 = seen + key
          val prefix = if (path.isEmpty) "" else s"$path."
          val oldC = s1.cases.map(_._1)
          val newC = s2.cases.map(_._1)
          val added = s2.cases.collect { case (n, _) if !oldC.contains(n) => Change.CaseAdded(prefix, n) }
          val removed = s1.cases.collect { case (n, _) if !newC.contains(n) => Change.CaseRemoved(prefix, n) }
          val common = s1.cases.flatMap { case (n, sc) =>
            s2.cases.find(_._1 == n).toVector.flatMap { case (_, sc2) => walk(sc(), sc2(), at(path, n), seen2) }
          }
          added ++ removed ++ common
        }

      case (x, y) =>
        Vector(Change.ShapeChanged(if (path.isEmpty) "the root" else path, shape(x), shape(y)))
    }

  /** a field's own change: a differing PRIMITIVE is a type change (the
   * wire value stops decoding), anything structured recurses */
  private def compareField(a: Schema[_], b: Schema[_], path: String, seen: Set[(String, String)]): Vector[Change] =
    (under(a), under(b)) match {
      case (x, y) if x == y => Vector.empty
      case (x, y) if primitive(x) && primitive(y) => Vector(Change.TypeChanged(path, shape(x), shape(y)))
      // Option[X] to X and back: the wire value is the same when
      // present, and the difference is exactly the absent case
      case (Schema.SOption(x), y) if !y.isInstanceOf[Schema.SOption[_]] =>
        Vector(Change.TypeChanged(path, s"Option[${shape(x())}]", shape(y))) ++ walk(x(), y, path, seen)
      case (x, Schema.SOption(y)) if !x.isInstanceOf[Schema.SOption[_]] =>
        Vector(Change.TypeChanged(path, shape(x), s"Option[${shape(y())}]")) ++ walk(x, y(), path, seen)
      case (x, y) => walk(x, y, path, seen)
    }

  private def primitive(s: Schema[_]): Boolean = s match {
    case Schema.SInt | Schema.SLong | Schema.SDouble | Schema.SBool
       | Schema.SString | Schema.SChar | Schema.SBytes | Schema.SBigInt => true
    case _ => false
  }
}
