package okay.matching

import okay.*
import okay.rag.{Embedding, Vectors}

/**
 * The reference implementation: everything in memory, embeddings by
 * a plain function (the deterministic hashing embedder by default —
 * no network, reproducible tests), platform policy as a predicate.
 *
 * Honest scope, like rag's MemoryStore: linear scans, fine to 10^4
 * profiles; real databases are stage-1 handlers behind the same
 * three effects.
 */
final class MemoryMatch(embed: String => Embedding = Vectors.hashing(),
                        policy: PlatformPolicy = PlatformPolicy.open,
                        proposeThreshold: Float = 0.85f,
                        halfLifeMs: Long = 7L * 24 * 3600 * 1000,
                        hash: String => String = identity,
                        verifyHash: (String, String) => Boolean = _ == _,
                        now: () => Long = () => System.currentTimeMillis()) {

  private var attrs: Vector[AttrDef] = Vector.empty
  private var facts: Vector[Fact] = Vector.empty
  private var profiles: Map[ProfileId, String] = Map.empty  // id -> email
  private var byEmail: Map[String, ProfileId] = Map.empty
  private var nextAttr = 1L
  private var nextFact = 1L
  private var recovery: Map[ProfileId, String] = Map.empty  // hashed secrets

  // ---- registry -----------------------------------------------------

  private def attrText(a: AttrDef): String =
    (a.slug +: a.synonyms).mkString(" ") + " " + a.description

  private def live(a: AttrDef): Boolean = a.status match
    case Status.MergedInto(_) => false
    case _ => true

  def registrySearch(text: String): Vector[AttrDef] =
    val q = embed(text)
    attrs.filter(live)
      .map(a => a -> Vectors.cosine(q, embed(attrText(a))))
      .sortBy(-_._2).take(8).map(_._1)

  /** search-before-create, enforced on the propose side too: an
   * exact slug/synonym hit or a near-duplicate description RETURNS
   * the existing attribute instead of minting a twin */
  def propose(d: AttrDraft): AttrDef =
    val names = (d.slug +: d.synonyms).map(_.toLowerCase).toSet
    val existing = attrs.filter(live).find { a =>
      val theirs = (a.slug +: a.synonyms).map(_.toLowerCase).toSet
      names.intersect(theirs).nonEmpty ||
        Vectors.cosine(embed(d.description), embed(a.description)) >= proposeThreshold
    }
    existing.getOrElse {
      val a = AttrDef(AttrId(nextAttr), d.slug, d.kind, d.description,
        d.synonyms, Status.Provisional, d.volatile)
      nextAttr += 1
      attrs :+= a
      a
    }

  def get(slug: String): Option[AttrDef] =
    attrs.find(a => a.slug == slug && live(a))

  // ---- facts --------------------------------------------------------

  private val rnd = new scala.util.Random
  private def freshId(): String =
    val bs = new Array[Byte](16)
    rnd.nextBytes(bs)
    bs.iterator.map(b => f"$b%02x").mkString

  def register(email: String): ProfileId =
    byEmail.getOrElse(email, {
      // NOT UUID.randomUUID: its csprng is java.security.SecureRandom,
      // which the Scala.js LINKER rejects even from a test leg (the
      // Crypto.Handle lesson). A profile id needs uniqueness, not
      // cryptography — util.Random hex serves every platform.
      val id = ProfileId(freshId())
      profiles += id -> email
      byEmail += email -> id
      id
    })

  /** idempotent by (profile, attr, provenance): replaying the same
   * chat asserts the same facts and the store does not grow */
  def assert(p: ProfileId, attr: String, side: Side, v: Value,
             prov: Provenance, conf: Double, vis: Vis): FactId =
    facts.find(f => f.profile == p && f.attr == attr && f.prov == prov)
      .map(_.id).getOrElse {
        val f = Fact(FactId(nextFact), p, attr, side, v, prov, conf,
          now(), vis)
        nextFact += 1
        facts :+= f
        summaries -= (p, side)
        f.id
      }

  def supersede(id: FactId, v: Value, reason: String, prov: Provenance): FactId =
    facts.find(_.id == id) match
      case None => id
      case Some(old) if old.supersededBy.isDefined => old.supersededBy.get
      case Some(old) =>
        val nf = old.copy(id = FactId(nextFact), value = v, prov = prov,
          ts = now(), supersededBy = None,
          reason = Some(reason))
        nextFact += 1
        facts = facts.map(f =>
          if f.id == id then f.copy(supersededBy = Some(nf.id)) else f) :+ nf
        summaries -= ((old.profile, old.side))
        nf.id

  def profileOf(id: ProfileId): Option[Profile] =
    profiles.get(id).map { email =>
      val mine = facts.filter(_.profile == id)
      Profile(id, email, mine.filter(_.supersededBy.isEmpty), mine)
    }

  // ---- search -------------------------------------------------------

  /** a profile's matchable text, one side: Private facts are excluded
   * from matching entirely — matching on them would leak them */
  private var summaries: Map[(ProfileId, Side), Embedding] = Map.empty

  private def matchable(p: ProfileId, side: Side): Vector[Fact] =
    facts.filter(f => f.profile == p && f.side == side &&
      f.supersededBy.isEmpty && f.vis != Vis.Private)

  private def summary(p: ProfileId, side: Side): Embedding =
    summaries.getOrElse((p, side), {
      val text = matchable(p, side)
        .map(f => f.attr + " " + Value.text(f.value)).mkString(" ")
      val e = embed(text)
      summaries += (p, side) -> e
      e
    })

  /** the two gates, at disclosure time: owner intent AND the
   * platform's engine. AfterMatch and Withhold facts still MATCHED —
   * that gate is the business — they just do not come back; the
   * AfterMatch ones are NAMED in `withheld`, which is the hook. */
  private def disclose(fs: Vector[Fact]): (Vector[Fact], Vector[String]) =
    val owned = fs.filter(_.vis == Vis.Public)
    (owned.filter(f => policy.gate(f.attr) == Gate.Allow),
      owned.filter(f => policy.gate(f.attr) == Gate.AfterMatch).map(_.attr).distinct)

  /** a volatile fact ages: exp2(-age/halfLife); stable facts do not */
  private def freshness(fs: Vector[Fact]): Float =
    val vol = fs.filter(f => attrs.exists(a => a.slug == f.attr && a.volatile))
    if vol.isEmpty then 1.0f
    else (vol.map(f => math.pow(2, -(now() - f.ts).toDouble / halfLifeMs))
      .sum / vol.length).toFloat

  def candidates(q: Query): Vector[Ranked] =
    val holders = facts.filter(f => f.side == q.side && f.supersededBy.isEmpty &&
      f.vis != Vis.Private).map(_.profile).distinct
    val passing = holders.filter { p =>
      q.filters.forall { (slug, pred) =>
        matchable(p, q.side).exists(f => f.attr == slug && Pred.holds(pred, f.value))
      }
    }
    val qe = if q.text.nonEmpty then embed(q.text) else null
    passing.map { p =>
      val fs = matchable(p, q.side)
      val base = if qe == null then 1.0f else Vectors.cosine(qe, summary(p, q.side))
      val (open, gated) = disclose(fs)
      Ranked(p, base * freshness(fs), open, gated)
    }.sortBy(-_.score).take(q.k)

  // ---- identity recovery (stage 2) ----------------------------------
  // The seam, not the dependency: `hash`/`verifyHash` are constructor
  // parameters; okay-security's Password plugs in at the integration
  // site. Without the secret there is NO path from a new email to an
  // existing profile — a stranger gets a fresh profile, not a hijack.

  def setRecovery(p: ProfileId, secret: String): Unit =
    recovery += p -> hash(secret)

  /** rebind the profile to a new email, authorized by the recovery
   * secret; refusal is an answer, not a throw */
  def rebind(oldEmail: String, newEmail: String, secret: String): Option[ProfileId] =
    byEmail.get(oldEmail).filter(p =>
      recovery.get(p).exists(verifyHash(secret, _))).map { p =>
      byEmail -= oldEmail
      byEmail += newEmail -> p
      profiles += p -> newEmail
      p
    }

  // ---- the handlers -------------------------------------------------

  given registry: Handler[Registry] = new:
    def handle[A](e: Registry[A]): A = e match
      case Registry.Search(t) => registrySearch(t)
      case Registry.Propose(d) => propose(d)
      case Registry.Get(s) => get(s)

  given factsH: Handler[Facts] = new:
    def handle[A](e: Facts[A]): A = e match
      case Facts.Register(email) => register(email)
      case Facts.Assert(p, a, s, v, prov, c, vis) => assert(p, a, s, v, prov, c, vis)
      case Facts.Supersede(id, v, r, prov) => supersede(id, v, r, prov)
      case Facts.ProfileOf(id) => profileOf(id)

  given find: Handler[Find] = new:
    def handle[A](e: Find[A]): A = e match
      case Find.Candidates(q) => candidates(q)
}
