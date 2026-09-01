package okay.matching

import okay.*

/**
 * The three effects (specs/match.md, Interface): the facade the
 * LLM's structuring programs are written against. Handlers own the
 * store — memory + rag embeddings at stage 0, sqlite/Postgres behind
 * the Sql seam at stage 1 — and the same typed programs run over all
 * of them.
 */

/** the attribute registry; search-before-create is its law */
enum Registry[+A]:
  case Search(text: String) extends Registry[Vector[AttrDef]]
  case Propose(draft: AttrDraft) extends Registry[AttrDef]
  case Get(slug: String) extends Registry[Option[AttrDef]]

object Registry:
  def search(text: String): Vector[AttrDef] ! Registry = effect(Search(text))
  def propose(d: AttrDraft): AttrDef ! Registry = effect(Propose(d))
  def get(slug: String): Option[AttrDef] ! Registry = effect(Get(slug))

/** the fact store; append-only, supersede with a reason */
enum Facts[+A]:
  case Register(email: String) extends Facts[ProfileId]
  case Assert(profile: ProfileId, attr: String, side: Side, value: Value,
              prov: Provenance, confidence: Double, vis: Vis) extends Facts[FactId]
  case Supersede(fact: FactId, value: Value, reason: String,
                 prov: Provenance) extends Facts[FactId]
  case ProfileOf(profile: ProfileId) extends Facts[Option[Profile]]

object Facts:
  def register(email: String): ProfileId ! Facts = effect(Register(email))
  def assert(profile: ProfileId, attr: String, side: Side, value: Value,
             prov: Provenance, confidence: Double = 1.0,
             vis: Vis = Vis.Public): FactId ! Facts =
    effect(Assert(profile, attr, side, value, prov, confidence, vis))
  def supersede(fact: FactId, value: Value, reason: String,
                prov: Provenance): FactId ! Facts =
    effect(Supersede(fact, value, reason, prov))
  def profile(id: ProfileId): Option[Profile] ! Facts = effect(ProfileOf(id))

/** hybrid search: hard filters over typed facts exclude, semantic
 * similarity ranks the rest. (Named Find, not Match — `match` is a
 * keyword, and this package already paid that toll once.) */
enum Find[+A]:
  case Candidates(q: Query) extends Find[Vector[Ranked]]

object Find:
  def candidates(q: Query): Vector[Ranked] ! Find = effect(Candidates(q))
