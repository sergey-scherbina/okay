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
                        now: () => Long = () => System.currentTimeMillis(),
                        fresh: () => String = Entropy.weak) extends MatchStore {

  private var attrs: Vector[AttrDef] = Vector.empty
  private var facts: Vector[Fact] = Vector.empty
  private var profiles: Map[ProfileId, String] = Map.empty  // id -> email
  private var byEmail: Map[String, ProfileId] = Map.empty
  private var nextAttr = 1L
  private var nextFact = 1L
  private var recovery: Map[ProfileId, String] = Map.empty  // hashed secrets
  private var links: Vector[(ProfileId, ProfileId)] = Vector.empty
  private var deals: Vector[Deal] = Vector.empty
  private var nextDeal = 1L
  private var tokens: Map[String, LinkToken] = Map.empty
  private val tokenTtlMs = 15L * 60 * 1000

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
        d.synonyms, Status.Provisional, d.volatile, d.identifying)
      nextAttr += 1
      attrs :+= a
      a
    }

  def get(slug: String): Option[AttrDef] =
    attrs.find(a => a.slug == slug && live(a))

  // ---- facts --------------------------------------------------------

  def register(email: String): ProfileId =
    byEmail.getOrElse(email, {
      // ids and tokens draw from the `fresh` seam — see Entropy: the
      // cross default links everywhere, the JVM stores default secure
      val id = ProfileId(fresh())
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
      val ids = identityOf(id).toSet
      val mine = facts.filter(f => ids.contains(f.profile))
      Profile(id, email, mine.filter(_.supersededBy.isEmpty), mine)
    }

  // ---- cross-channel identity (match-identity-x) --------------------

  /** the equivalence class of confirmed links (reflexive closure) */
  def identityOf(p: ProfileId): Vector[ProfileId] =
    var cls = Set(p)
    var grew = true
    while grew do
      val next = cls ++ links.collect {
        case (a, b) if cls(a) => b
        case (a, b) if cls(b) => a
      }
      grew = next.size != cls.size
      cls = next
    cls.toVector.sortBy(_.uuid)

  /** who shares an identifying fact value — the attribute and a
   * masked hint, NOTHING else (leaking less is the design) */
  def linkCandidates(p: ProfileId): Vector[LinkHint] =
    val identifying = attrs.filter(a => live(a) && a.identifying).map(_.slug).toSet
    val mine = facts.filter(f => f.profile == p && f.supersededBy.isEmpty
      && identifying.contains(f.attr)).map(f => (f.attr, Value.text(f.value)))
    val cls = identityOf(p).toSet
    mine.flatMap { (attr, v) =>
      facts.filter(f => !cls.contains(f.profile) && f.attr == attr
        && f.supersededBy.isEmpty && Value.text(f.value) == v)
        .map(_.profile).distinct.flatMap(o => profiles.get(o).map(e =>
          LinkHint(attr, LinkHint.mask(e))))
    }.distinct

  /** mint the single-use token addressed to the OLD profile; the
   * integration site delivers it through the old channel */
  def requestLink(from: ProfileId, to: ProfileId): Option[LinkToken] =
    if !profiles.contains(from) || !profiles.contains(to) then None
    else
      val t = LinkToken(fresh(), from, to,
        now() + tokenTtlMs)
      tokens += t.token -> t
      Some(t)

  /** the person in the NEW chat produced the token: both ends held */
  def confirmLink(token: String, by: ProfileId, prov: Provenance): Option[ProfileId] =
    tokens.get(token) match
      case Some(t) if t.from == by && now() <= t.expiresAt =>
        tokens -= token                                    // single use
        links :+= (t.from, t.to)
        summaries = Map.empty                              // the class changed shape
        Some(t.to)
      case _ => None

  /** the fallback for a dead old channel: the recovery secret */
  def linkByRecovery(from: ProfileId, oldEmail: String, secret: String): Option[ProfileId] =
    byEmail.get(oldEmail).filter(p =>
      recovery.get(p).exists(verifyHash(secret, _))).map { old =>
      links :+= (from, old)
      summaries = Map.empty
      old
    }

  // ---- search -------------------------------------------------------

  /** a profile's matchable text, one side: Private facts are excluded
   * from matching entirely — matching on them would leak them */
  private var summaries: Map[(ProfileId, Side), Embedding] = Map.empty

  private def matchable(p: ProfileId, side: Side): Vector[Fact] =
    val cls = identityOf(p).toSet
    facts.filter(f => cls.contains(f.profile) && f.side == side &&
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
      .map(p => identityOf(p).head).distinct              // one person, one candidate
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

  // ---- deals: the negotiation, and the Matched unlock ---------------

  def inquire(seeker: ProfileId, provider: ProfileId, what: String): DealId =
    val id = DealId(nextDeal); nextDeal += 1
    deals :+= Deal(id, seeker, provider, what, DealState.Asked, now())
    id

  def respond(deal: DealId, by: ProfileId, accept: Boolean): Option[Deal] =
    deals.find(d => d.id == deal && d.state == DealState.Asked)
      .filter(_.provider == by)                 // the asked one's answer alone
      .map { d =>
        val d2 = d.copy(state = if accept then DealState.Accepted else DealState.Declined,
          ts = now())
        deals = deals.map(x => if x.id == deal then d2 else x)
        d2
      }

  def dealsFor(p: ProfileId): Vector[Deal] =
    deals.filter(d => d.seeker == p || d.provider == p)

  /** the withdraw: an Asked deal the seeker takes back (the round's
   * cleanup once somebody accepted) */
  def withdraw(deal: DealId, by: ProfileId): Option[Deal] =
    deals.find(d => d.id == deal && d.state == DealState.Asked)
      .filter(_.seeker == by)
      .map { d =>
        val d2 = d.copy(state = DealState.Withdrawn, ts = now())
        deals = deals.map(x => if x.id == deal then d2 else x)
        d2
      }

  private def bound(a: ProfileId, b: ProfileId): Boolean =
    deals.exists(d => d.state == DealState.Accepted &&
      ((d.seeker == a && d.provider == b) || (d.seeker == b && d.provider == a)))

  def contacts(viewer: ProfileId, other: ProfileId): Vector[Fact] =
    if !bound(viewer, other) then Vector.empty
    else facts.filter(f => f.profile == other && f.supersededBy.isEmpty &&
      (f.vis == Vis.Matched ||
        (f.vis == Vis.Public && policy.gate(f.attr) == Gate.AfterMatch)))

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

  given ident: Handler[Ident] = new:
    def handle[A](e: Ident[A]): A = e match
      case Ident.Candidates(p) => linkCandidates(p)
      case Ident.Request(f, t) => requestLink(f, t)
      case Ident.Confirm(t, by, prov) => confirmLink(t, by, prov)
      case Ident.IdentityOf(p) => identityOf(p)

  given find: Handler[Find] = new:
    def handle[A](e: Find[A]): A = e match
      case Find.Candidates(q) => candidates(q)
}
