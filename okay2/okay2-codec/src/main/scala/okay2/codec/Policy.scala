package okay2.codec

import scala.annotation.tailrec
import okay2.Applicative
import okay2.optics.Optic._

/**
 * A PROJECTION POLICY (okay-codec's Policy, specs/optics-outside.md
 * stage 7): which fields of a record may NOT be seen, embedded or
 * logged — declared once, as dotted keys checked against the `Schema`
 * that writes the record, and read by TWO interpreters:
 *
 *   - DESCRIBE — `touches`: the fields this policy changes, with no
 *     document in hand. The audit, which a function `A => A` can never
 *     answer.
 *   - RUN — `project` (the fields removed), `redact` (kept, their values
 *     replaced), `optic(key)` (a hidden field as an optic), `text` (a
 *     value's allowed fields as one string).
 *
 * The law that couples them (TestPolicy): the keys `project` removes
 * from a document are exactly `touches`, restricted to the keys that
 * document has. A key names a product field, through any Option/iso; a
 * segment through a list applies to EVERY element; a segment through a
 * sum reaches the case that has it.
 */
final class Policy[A] private (val hidden: Vector[String], parents: Vector[(String, Policy.Tr, String)],
                               schema: Schema[A]) {

  /** DESCRIBE: the fields this policy touches — no document needed */
  def touches: Set[String] = hidden.toSet

  /** RUN: the document with the hidden fields removed, wherever they are */
  def project(j: Json): Json =
    parents.foldLeft(j) { case (doc, (_, to, last)) =>
      to.modify {
        case Json.JObj(fs) => Json.JObj(fs.filterNot(_._1 == last))
        case other => other
      }(doc)
    }

  /** RUN: the hidden fields kept, their values replaced by `marker` */
  def redact(j: Json, marker: Json = Json.JStr("[redacted]")): Json =
    parents.foldLeft(j) { case (doc, (_, to, last)) =>
      to.modify {
        case Json.JObj(fs) => Json.JObj(fs.map { case (k, v) => if (k == last) (k, marker) else (k, v) })
        case other => other
      }(doc)
    }

  /** each hidden field as an optic over the document, in policy order —
   * one per key, because two independent traversals compose only
   * through a bind, which an optic has not */
  def optics: Vector[(String, Policy.Tr)] =
    parents.map { case (key, to, last) => key -> Policy.into(to, JsonOptic.field(last)) }

  /** the optic of one hidden key, if this policy hides it */
  def optic(key: String): Option[Policy.Tr] =
    optics.collectFirst { case (k, t) if k == key => t }

  /** the value's allowed fields as text: what may be embedded or logged */
  def text(a: A): String = Json.print(project(Json.parse(Json.write(a)(schema))))
}

object Policy {
  type Tr = Traversal[Json, Json, Json, Json]

  /** composed WITHOUT the expected type in view: Scala 2 would infer
   * `andThen`'s constraint from it (JsonOptic.field says the same) */
  private def via(a: Tr, b: Tr): Tr = { val o = a.andThen(b); o }
  private[codec] def into(a: Tr, b: Affine[Json, Json, Json, Json]): Tr = { val o = a.andThen(b); o }

  /** the traversal that is the document itself */
  private val here: Tr = Traversal(new Walk[Json, Json, Json, Json] {
    def apply[F[_]](f: Json => F[Json])(implicit F: Applicative[F]): Json => F[Json] = f
  })

  /**
   * A policy hiding `keys`, each checked against the schema: a key naming
   * nothing the schema writes is refused BY NAME, at construction — a
   * typo must not be discovered by the field it failed to hide.
   */
  def hide[A](keys: String*)(implicit s: Schema[A]): Either[String, Policy[A]] = {
    val ks = keys.toVector.distinct
    // a key past JsonOptic.MaxSegments names nothing: the descent below
    // is one frame per segment
    val resolved = ks.map { k =>
      val segs = k.split('.').toList
      k -> (if (segs.length > JsonOptic.MaxSegments) None else to(s, segs, here))
    }
    resolved.collectFirst { case (k, None) => k } match {
      case Some(bad) => Left(s"policy: `$bad` names no field the schema of ${nameOf(s)} writes")
      case None => Right(new Policy[A](ks, resolved.collect { case (k, Some((t, last))) => (k, t, last) }, s))
    }
  }

  @tailrec private def nameOf(s: Schema[_]): String = s match {
    case p: Schema.SProduct[_] => p.name
    case su: Schema.SSum[_] => su.name
    case i: Schema.SIso[_, _] => nameOf(i.under())
    case other => other.toString
  }

  /** the traversal to the OBJECTS that hold the key's last segment, and
   * that segment; None when a segment names nothing */
  private def to(s: Schema[_], segs: List[String], acc: Tr): Option[(Tr, String)] = s match {
    case i: Schema.SIso[_, _] => to(i.under(), segs, acc)
    case o: Schema.SOption[_] => to(o.of(), segs, acc)
    case l: Schema.SList[_] => to(l.of(), segs, via(acc, JsonOptic.values))
    case v: Schema.SVector[_] => to(v.of(), segs, via(acc, JsonOptic.values))
    case su: Schema.SSum[_] =>
      // the case wrapper is a level the key does not spell: through its
      // one entry, into whichever case knows the segment
      su.cases.iterator.flatMap { case (_, cs) => to(cs(), segs, via(acc, JsonOptic.entries)) }.nextOption()
    case p: Schema.SProduct[_] => segs match {
      case last :: Nil => p.fields.find(_._1 == last).map(_ => (acc, last))
      case seg :: rest => p.fields.find(_._1 == seg).flatMap { case (_, fs) => to(fs(), rest, into(acc, JsonOptic.field(seg))) }
      case Nil => None
    }
    case _ => None
  }
}
