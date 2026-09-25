package okay.codec

import okay.*
import okay.given
import scala.annotation.tailrec

/**
 * A PROJECTION POLICY (specs/optics-outside.md, stage 7): which fields
 * of a record may NOT be seen, embedded or logged — declared once,
 * as dotted keys checked against the `Schema` that writes the record,
 * and read by TWO interpreters:
 *
 *   - DESCRIBE — `touches`: the fields this policy changes, with no
 *     document in hand. The audit; what a function `A => A` can never
 *     answer, and the reason the policy is a value and not a function.
 *   - RUN — `project` (the fields removed), `redact` (kept, their
 *     values replaced), `optic(key)` (a hidden field as an optic, for
 *     whatever else a caller wants to do at it), `text` (a value's
 *     allowed fields as one string — what an embedding or a log line
 *     may see).
 *
 * The law that couples them, pinned by `TestPolicy`: the keys
 * `project` removes from a document are exactly `touches`, restricted
 * to the keys that document has. A key names a product field, through
 * any `Option`/iso; a segment through a list or vector applies to EVERY
 * element (`lines.qty` is every line's `qty`); a segment through a sum
 * reaches the case that has it (`{"Case": {...}}` is one level the key
 * does not spell, as `JsonOptic.path` also knows).
 *
 * Why a reified list of keys and optics DERIVED from it, rather
 * than a traversal alone: a profunctor optic can be run, but not
 * asked what it looks at — the audit needs the names, so the names
 * are the source and the optic is built from them. This is the
 * criterion the arc stated for an optic in a public API: one
 * declaration, several interpreters, at least one of which describes.
 */
final class Policy[A] private (val hidden: Vector[String], parents: Vector[(String, Policy.Tr, String)],
                               schema: Schema[A]):

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
        case Json.JObj(fs) => Json.JObj(fs.map((k, v) => if k == last then (k, marker) else (k, v)))
        case other => other
      }(doc)
    }

  /** each hidden field as an optic over the document, in policy order
   * — one per key rather than one for all, because two independent
   * traversals compose only through a bind, which an optic has not */
  def optics: Vector[(String, Traversal[Json, Json, Json, Json])] =
    parents.map((key, to, last) => key -> to.andThen(JsonOptic.field(last)))

  /** the optic of one hidden key, if this policy hides it */
  def optic(key: String): Option[Traversal[Json, Json, Json, Json]] =
    optics.collectFirst { case (k, t) if k == key => t }

  /** the value's allowed fields as text: what may be embedded or logged */
  def text(a: A): String = Json.print(project(Json.parse(Json.write(a)(using schema))))

object Policy:
  private[codec] type Tr = Traversal[Json, Json, Json, Json]

  /** the traversal that is the document itself */
  private val here: Tr = Traversal([F[_]] => (F: Applicative[F]) ?=> (f: Json => F[Json]) => (j: Json) => f(j))

  /**
   * A policy hiding `keys`, each checked against the schema: a key
   * naming nothing the schema writes is refused BY NAME, at
   * construction — a policy is declared once and a typo in it must
   * not be discovered by the field it failed to hide.
   */
  def hide[A](keys: String*)(using s: Schema[A]): Either[String, Policy[A]] =
    val ks = keys.toVector.distinct
    val resolved = ks.map(k => k -> to(s, k.split('.').toList, here))
    resolved.collectFirst { case (k, None) => k } match
      case Some(bad) => Left(s"policy: `$bad` names no field the schema of ${nameOf(s)} writes")
      case None => Right(new Policy[A](ks, resolved.collect { case (k, Some((to, last))) => (k, to, last) }, s))

  @tailrec private def nameOf(s: Schema[?]): String = s match
    case p: Schema.SProduct[?] => p.name
    case su: Schema.SSum[?] => su.name
    case Schema.SIso(u, _, _) => nameOf(u())
    case other => other.toString

  /** the traversal to the OBJECTS that hold the key's last segment,
   * and that segment; None when a segment names nothing */
  private def to(s: Schema[?], segs: List[String], acc: Tr): Option[(Tr, String)] = s match
    case Schema.SIso(u, _, _) => to(u(), segs, acc)
    case Schema.SOption(of) => to(of(), segs, acc)
    case Schema.SList(of) => to(of(), segs, acc.andThen(JsonOptic.values))
    case Schema.SVector(of) => to(of(), segs, acc.andThen(JsonOptic.values))
    case su: Schema.SSum[?] =>
      // the case wrapper is a level the key does not spell: through
      // its one entry, into whichever case knows the segment
      su.cases.iterator.flatMap((_, cs) => to(cs(), segs, acc.andThen(JsonOptic.entries))).nextOption()
    case p: Schema.SProduct[?] => segs match
      case last :: Nil => p.fields.find(_._1 == last).map(_ => (acc, last))
      case seg :: rest => p.fields.find(_._1 == seg).flatMap((_, fs) => to(fs(), rest, acc.andThen(JsonOptic.field(seg))))
      case Nil => None
    case _ => None
