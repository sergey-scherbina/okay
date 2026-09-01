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

/** cross-channel identity (match-identity-x): candidates from
 * identifying facts, the token challenge, the equivalence class.
 * The token TRAVELS outside this module — through the old channel —
 * which is exactly why producing it in the new chat proves the link. */
enum Ident[+A]:
  case Candidates(p: ProfileId) extends Ident[Vector[LinkHint]]
  case Request(from: ProfileId, to: ProfileId) extends Ident[Option[LinkToken]]
  case Confirm(token: String, by: ProfileId, prov: Provenance) extends Ident[Option[ProfileId]]
  case IdentityOf(p: ProfileId) extends Ident[Vector[ProfileId]]

object Ident:
  def candidates(p: ProfileId): Vector[LinkHint] ! Ident = effect(Candidates(p))
  def request(from: ProfileId, to: ProfileId): Option[LinkToken] ! Ident =
    effect(Request(from, to))
  def confirm(token: String, by: ProfileId, prov: Provenance): Option[ProfileId] ! Ident =
    effect(Confirm(token, by, prov))
  def identityOf(p: ProfileId): Vector[ProfileId] ! Ident = effect(IdentityOf(p))

/** the reranker (stage 2): an LLM reads the need and the top
 * candidates' summaries and orders them — an EFFECT, so tests use a
 * deterministic handler and no network lives in this module (the
 * rag/Embed precedent). The production handler is the integration
 * site's five lines over okay-llm. */
enum Rerank[+A]:
  case Order(need: String, candidates: Vector[(ProfileId, String)])
    extends Rerank[Vector[ProfileId]]

object Rerank:
  def order(need: String, cs: Vector[(ProfileId, String)])
  : Vector[ProfileId] ! Rerank = effect(Order(need, cs))

  /** overlap-scored deterministic reranker for tests and offline use */
  def lexical: Handler[Rerank] = new:
    def handle[A](e: Rerank[A]): A = e match
      case Order(need, cs) =>
        val ws = need.toLowerCase.split("\\W+").toSet
        cs.sortBy((_, text) =>
          -text.toLowerCase.split("\\W+").count(ws.contains)).map(_._1)

/** candidates, then the reranker over the top slice — the composed
 * program a seeker-side agent actually runs */
def top(q: Query, rerankOver: Int = 10): Vector[Ranked] ! (Find + Rerank) =
  !.widen[Vector[Ranked], Find, Rerank](Find.candidates(q.copy(k = rerankOver)))
    .flatMap { cs =>
      val summaries = cs.map(r => r.profile ->
        r.disclosed.map(f => f.attr + " " + Value.text(f.value)).mkString(" "))
      !.widen[Vector[ProfileId], Rerank, Find](Rerank.order(q.text, summaries))
        .map(order => order.flatMap(p => cs.find(_.profile == p)).take(q.k))
    }
