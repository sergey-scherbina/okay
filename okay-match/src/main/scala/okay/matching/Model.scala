package okay.matching

import okay.rag.Embedding

/**
 * The data model of okay-match (specs/match.md), stage 0.
 *
 * Log-first is the founding decision: everything here is a
 * PROJECTION — rebuildable from the chat log — which is why facts are
 * append-only, corrections supersede instead of overwrite, and every
 * fact carries provenance back to the chat span it came from. The
 * store may be wrong; the log never is.
 */

/** the owner-secret identity: email binds it, the UUID is the key.
 * Recovery after a lost email is stage 2 (the hijack question). */
final case class ProfileId(uuid: String)

final case class FactId(n: Long)
final case class AttrId(n: Long)

/** which side of the market a fact serves: what a profile OFFERS is
 * matched against what another NEEDS, and symmetrically — a need is
 * stored by the same machinery as a skill */
enum Side:
  case Offer, Need

/** where a fact came from: the chat and the place in it. The system
 * can always answer "why do you believe I can do X" with a quote —
 * and the (profile, attribute, provenance) triple is the idempotence
 * key that makes replaying extraction over the same log a no-op. */
final case class Provenance(chat: String, offset: Long, span: String)

/** the owner's intent for a fact. The platform's own gate is the
 * handler's policy, not data the owner writes — disclosure is the
 * MINIMUM of the two (the second gate is the business). */
enum Vis:
  case Public   // matchable and disclosable (platform willing)
  case Matched  // matchable; disclosed only after a confirmed match
  case Private  // owner and LLM only; excluded from matching

/** the small value core — grown through the registry, never designed
 * up front. VTime stays textual at stage 0; a real schedule type
 * earns its place when an attribute needs querying by it. */
enum Value:
  case VText(s: String)
  case VNum(d: Double)
  case VRange(lo: Double, hi: Double)
  case VGeo(lat: Double, lon: Double)
  case VTime(desc: String)
  case VRef(profile: ProfileId)

object Value:
  /** the text a value contributes to the profile's embedding */
  def text(v: Value): String = v match
    case Value.VText(s) => s
    case Value.VNum(d) => d.toString
    case Value.VRange(lo, hi) => s"$lo..$hi"
    case Value.VGeo(lat, lon) => s"$lat,$lon"
    case Value.VTime(d) => d
    case Value.VRef(p) => p.uuid

/** what a value slot holds, registry-side */
enum Kind:
  case Text, Num, Range, Geo, Time, Ref

/** a registered attribute. Born provisional, promoted by use; a
 * synonym merge marks the loser MergedInto and the projection is
 * rebuilt — the log never changes. */
enum Status:
  case Provisional, Established
  case MergedInto(slug: String)

final case class AttrDef(id: AttrId, slug: String, kind: Kind,
                         description: String,
                         synonyms: Vector[String] = Vector.empty,
                         status: Status = Status.Provisional,
                         volatile: Boolean = false)

/** what the LLM proposes; the registry decides whether it already
 * exists (search-before-create is enforced on THIS side too) */
final case class AttrDraft(slug: String, kind: Kind, description: String,
                           synonyms: Vector[String] = Vector.empty,
                           volatile: Boolean = false)

final case class Fact(id: FactId, profile: ProfileId, attr: String,
                      side: Side, value: Value, prov: Provenance,
                      confidence: Double, ts: Long, vis: Vis,
                      supersededBy: Option[FactId] = None,
                      reason: Option[String] = None)

/** current state and history — both are needed: the in-chat merge
 * dialogue reads the profile to notice conflicts, and search reads
 * it to rank */
final case class Profile(id: ProfileId, email: String,
                         current: Vector[Fact], history: Vector[Fact])

/** a hard constraint over one attribute's typed values */
enum Pred:
  case Is(v: Value)
  case AtLeast(x: Double)
  case AtMost(x: Double)
  case Within(lat: Double, lon: Double, km: Double)
  case HasText(sub: String)

object Pred:
  private def dKm(aLat: Double, aLon: Double, bLat: Double, bLon: Double): Double =
    // equirectangular — fine at city scale, and stage 0 is city scale
    val r = 6371.0
    val x = math.toRadians(bLon - aLon) * math.cos(math.toRadians((aLat + bLat) / 2))
    val y = math.toRadians(bLat - aLat)
    math.sqrt(x * x + y * y) * r

  def holds(p: Pred, v: Value): Boolean = (p, v) match
    case (Is(w), _) => w == v
    case (AtLeast(x), Value.VNum(d)) => d >= x
    case (AtLeast(x), Value.VRange(_, hi)) => hi >= x
    case (AtMost(x), Value.VNum(d)) => d <= x
    case (AtMost(x), Value.VRange(lo, _)) => lo <= x
    case (Within(lat, lon, km), Value.VGeo(bLat, bLon)) => dKm(lat, lon, bLat, bLon) <= km
    case (HasText(sub), Value.VText(s)) => s.toLowerCase.contains(sub.toLowerCase)
    case _ => false

/** a structured need: hard filters plus free text, against one side */
final case class Query(side: Side,
                       filters: Vector[(String, Pred)] = Vector.empty,
                       text: String = "", k: Int = 10)

/** a hit: who, how well, and only what the two gates disclose */
final case class Ranked(profile: ProfileId, score: Float,
                        disclosed: Vector[Fact])
