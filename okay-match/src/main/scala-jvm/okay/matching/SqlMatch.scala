package okay.matching

import okay.*
import okay.given
import okay.rag.{Embedding, Vectors}
import okay.sql.{Sql, SqlValue}
import okay.sql.SqlValue.*

/**
 * The durable store, stage 1 (specs/match.md): the same three
 * handlers over the `Sql` seam — H2 in the tests, sqlite or Postgres
 * in production, ANY driver that serves the trait. The backend list
 * is open by construction; this file is one entry in it.
 *
 * Values are flattened into typed columns (vkind discriminates), so
 * numeric predicates could push into WHERE when it matters; stage 1
 * filters candidates in Scala like the memory reference — honest
 * scope, the seam is the point here, not the query planner.
 * Embeddings are recomputed from fact text by the same function the
 * memory handler uses; pgvector ANN is okay-pg's business later.
 *
 * JVM: each operation runs its little Async program to completion on
 * the calling fiber (CanBlock — parking is free on Loom).
 */
final class SqlMatch(sql: Sql,
                     embed: String => Embedding = Vectors.hashing(),
                     policy: PlatformPolicy = PlatformPolicy.open,
                     proposeThreshold: Float = 0.85f,
                     halfLifeMs: Long = 7L * 24 * 3600 * 1000,
                     hash: String => String = identity,
                     verifyHash: (String, String) => Boolean = _ == _,
                     now: () => Long = () => System.currentTimeMillis())(using CanBlock) {

  private def run[A](p: A ! Async): A = Async.run[A, Pure](p).runWith

  private def rows(q: String, ps: Vector[SqlValue] = Vector.empty): Vector[Vector[SqlValue]] =
    val S = summon[Stream[[X] =>> X ! (Produce + Async), Async]]
    def go(p: Chunk[Vector[SqlValue]] ! (Produce + Async),
           acc: Vector[Vector[SqlValue]]): Vector[Vector[SqlValue]] ! Async =
      S.uncons(p).flatMap {
        case None => pure(acc)
        case Some((c, rest)) => go(rest, acc ++ c)
      }
    run(go(sql.query(q, ps), Vector.empty))

  private def exec(q: String, ps: Vector[SqlValue] = Vector.empty): Long =
    run(sql.update(q, ps))

  // ---- schema -------------------------------------------------------

  exec("""CREATE TABLE IF NOT EXISTS match_attrs(
    id BIGINT PRIMARY KEY, slug VARCHAR(255), kind VARCHAR(16),
    description VARCHAR(4000), synonyms VARCHAR(4000),
    status VARCHAR(300), volatile BOOLEAN)""")
  exec("""CREATE TABLE IF NOT EXISTS match_profiles(
    uuid VARCHAR(64) PRIMARY KEY, email VARCHAR(255))""")
  exec("""CREATE TABLE IF NOT EXISTS match_facts(
    id BIGINT PRIMARY KEY, profile VARCHAR(64), attr VARCHAR(255),
    side VARCHAR(8), vkind VARCHAR(8), vtext VARCHAR(4000),
    vnum DOUBLE, vlo DOUBLE, vhi DOUBLE, vlat DOUBLE, vlon DOUBLE,
    chat VARCHAR(255), off BIGINT, span VARCHAR(4000),
    confidence DOUBLE, ts BIGINT, vis VARCHAR(8),
    superseded_by BIGINT, reason VARCHAR(4000))""")
  exec("""CREATE TABLE IF NOT EXISTS match_recovery(
    profile VARCHAR(64) PRIMARY KEY, secret VARCHAR(1000))""")
  exec("CREATE INDEX IF NOT EXISTS match_facts_attr ON match_facts(attr)")
  exec("CREATE INDEX IF NOT EXISTS match_facts_profile ON match_facts(profile)")

  private def maxId(table: String): Long = rows(s"SELECT MAX(id) FROM $table") match
    case Vector(Vector(I64(n))) => n
    case Vector(Vector(I32(n))) => n.toLong
    case _ => 0L
  private var nextAttr = maxId("match_attrs") + 1
  private var nextFact = maxId("match_facts") + 1

  // ---- codecs -------------------------------------------------------

  private def s(v: SqlValue): String = v match { case Text(x) => x; case _ => "" }
  private def sOpt(v: SqlValue): Option[String] = v match { case Text(x) => Some(x); case _ => None }
  private def dbl(v: SqlValue): Double = v match
    case F64(x) => x; case I64(x) => x.toDouble; case I32(x) => x.toDouble; case _ => 0.0
  private def lng(v: SqlValue): Long = v match
    case I64(x) => x; case I32(x) => x.toLong; case F64(x) => x.toLong; case _ => 0L
  private def lOpt(v: SqlValue): Option[Long] = v match
    case I64(x) => Some(x); case I32(x) => Some(x.toLong); case _ => None

  private def attrOf(r: Vector[SqlValue]): AttrDef = AttrDef(
    AttrId(lng(r(0))), s(r(1)),
    Kind.values.find(_.toString == s(r(2))).getOrElse(Kind.Text),
    s(r(3)),
    if s(r(4)).isEmpty then Vector.empty else s(r(4)).split('\u0001').toVector,
    s(r(5)) match
      case "Provisional" => Status.Provisional
      case "Established" => Status.Established
      case x => Status.MergedInto(x.stripPrefix("MergedInto:")),
    r(6) match { case Bool(b) => b; case _ => false })

  private def valueOf(r: Vector[SqlValue]): Value = s(r(4)) match
    case "num" => Value.VNum(dbl(r(6)))
    case "range" => Value.VRange(dbl(r(7)), dbl(r(8)))
    case "geo" => Value.VGeo(dbl(r(9)), dbl(r(10)))
    case "time" => Value.VTime(s(r(5)))
    case "ref" => Value.VRef(ProfileId(s(r(5))))
    case _ => Value.VText(s(r(5)))

  private def factOf(r: Vector[SqlValue]): Fact = Fact(
    FactId(lng(r(0))), ProfileId(s(r(1))), s(r(2)),
    if s(r(3)) == "Need" then Side.Need else Side.Offer,
    valueOf(r),
    Provenance(s(r(11)), lng(r(12)), s(r(13))),
    dbl(r(14)), lng(r(15)),
    Vis.valueOf(s(r(16))),
    lOpt(r(17)).map(FactId(_)), sOpt(r(18)))

  private val factCols = "id, profile, attr, side, vkind, vtext, vnum, vlo, vhi, " +
    "vlat, vlon, chat, off, span, confidence, ts, vis, superseded_by, reason"

  // ---- registry -----------------------------------------------------

  private def liveAttrs: Vector[AttrDef] =
    rows("SELECT id, slug, kind, description, synonyms, status, volatile FROM match_attrs")
      .map(attrOf).filter(a => a.status match
        case Status.MergedInto(_) => false
        case _ => true)

  private def attrText(a: AttrDef): String =
    (a.slug +: a.synonyms).mkString(" ") + " " + a.description

  def registrySearch(text: String): Vector[AttrDef] =
    val q = embed(text)
    liveAttrs.map(a => a -> Vectors.cosine(q, embed(attrText(a))))
      .sortBy(-_._2).take(8).map(_._1)

  def propose(d: AttrDraft): AttrDef =
    val names = (d.slug +: d.synonyms).map(_.toLowerCase).toSet
    liveAttrs.find { a =>
      val theirs = (a.slug +: a.synonyms).map(_.toLowerCase).toSet
      names.intersect(theirs).nonEmpty ||
        Vectors.cosine(embed(d.description), embed(a.description)) >= proposeThreshold
    }.getOrElse {
      val a = AttrDef(AttrId(nextAttr), d.slug, d.kind, d.description,
        d.synonyms, Status.Provisional, d.volatile)
      nextAttr += 1
      exec("INSERT INTO match_attrs VALUES(?,?,?,?,?,?,?)", Vector(
        I64(a.id.n), Text(a.slug), Text(a.kind.toString), Text(a.description),
        Text(a.synonyms.mkString("\u0001")), Text("Provisional"), Bool(a.volatile)))
      a
    }

  def get(slug: String): Option[AttrDef] = liveAttrs.find(_.slug == slug)

  /** the registry migration (stage 1): the loser's facts move to the
   * winner — a projection rebuild in place; the log never changed */
  def mergeAttr(loser: String, winner: String): Unit =
    exec("UPDATE match_facts SET attr = ? WHERE attr = ?",
      Vector(Text(winner), Text(loser)))
    exec("UPDATE match_attrs SET status = ? WHERE slug = ?",
      Vector(Text(s"MergedInto:$winner"), Text(loser)))
    ()

  // ---- facts --------------------------------------------------------

  def register(email: String): ProfileId =
    rows("SELECT uuid FROM match_profiles WHERE email = ?", Vector(Text(email))) match
      case Vector(Vector(Text(u))) => ProfileId(u)
      case _ =>
        val id = ProfileId(java.util.UUID.randomUUID().toString)
        exec("INSERT INTO match_profiles VALUES(?,?)", Vector(Text(id.uuid), Text(email)))
        id

  def assert(p: ProfileId, attr: String, side: Side, v: Value,
             prov: Provenance, conf: Double, vis: Vis): FactId =
    rows("SELECT id FROM match_facts WHERE profile = ? AND attr = ? " +
      "AND chat = ? AND off = ? AND span = ?",
      Vector(Text(p.uuid), Text(attr), Text(prov.chat), I64(prov.offset),
        Text(prov.span))) match
      case Vector(Vector(idv)) => FactId(lng(idv))
      case _ =>
        val id = FactId(nextFact); nextFact += 1
        insertFact(Fact(id, p, attr, side, v, prov, conf,
          now(), vis))
        id

  private def insertFact(f: Fact): Unit =
    val (vk, vt, vn, lo, hi, la, lo2) = f.value match
      case Value.VText(x) => ("text", x, 0.0, 0.0, 0.0, 0.0, 0.0)
      case Value.VNum(n) => ("num", "", n, 0.0, 0.0, 0.0, 0.0)
      case Value.VRange(a, b) => ("range", "", 0.0, a, b, 0.0, 0.0)
      case Value.VGeo(a, b) => ("geo", "", 0.0, 0.0, 0.0, a, b)
      case Value.VTime(x) => ("time", x, 0.0, 0.0, 0.0, 0.0, 0.0)
      case Value.VRef(r) => ("ref", r.uuid, 0.0, 0.0, 0.0, 0.0, 0.0)
    exec(s"INSERT INTO match_facts($factCols) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
      Vector(I64(f.id.n), Text(f.profile.uuid), Text(f.attr), Text(f.side.toString),
        Text(vk), Text(vt), F64(vn), F64(lo), F64(hi), F64(la), F64(lo2),
        Text(f.prov.chat), I64(f.prov.offset), Text(f.prov.span),
        F64(f.confidence), I64(f.ts), Text(f.vis.toString),
        f.supersededBy.map(x => I64(x.n)).getOrElse(Null),
        f.reason.map(Text(_)).getOrElse(Null)))
    ()

  def supersede(id: FactId, v: Value, reason: String, prov: Provenance): FactId =
    rows(s"SELECT $factCols FROM match_facts WHERE id = ?", Vector(I64(id.n)))
      .map(factOf).headOption match
      case None => id
      case Some(old) if old.supersededBy.isDefined => old.supersededBy.get
      case Some(old) =>
        val nf = old.copy(id = FactId(nextFact), value = v, prov = prov,
          ts = now(), supersededBy = None, reason = Some(reason))
        nextFact += 1
        insertFact(nf)
        exec("UPDATE match_facts SET superseded_by = ? WHERE id = ?",
          Vector(I64(nf.id.n), I64(id.n)))
        nf.id

  def profileOf(id: ProfileId): Option[Profile] =
    rows("SELECT email FROM match_profiles WHERE uuid = ?", Vector(Text(id.uuid))) match
      case Vector(Vector(Text(email))) =>
        val mine = rows(s"SELECT $factCols FROM match_facts WHERE profile = ? ORDER BY id",
          Vector(Text(id.uuid))).map(factOf)
        Some(Profile(id, email, mine.filter(_.supersededBy.isEmpty), mine))
      case _ => None

  // ---- search -------------------------------------------------------

  private def matchable(side: Side): Vector[Fact] =
    rows(s"SELECT $factCols FROM match_facts WHERE side = ? " +
      "AND superseded_by IS NULL AND vis <> 'Private'",
      Vector(Text(side.toString))).map(factOf)

  private def disclose(fs: Vector[Fact]): (Vector[Fact], Vector[String]) =
    val owned = fs.filter(_.vis == Vis.Public)
    (owned.filter(f => policy.gate(f.attr) == Gate.Allow),
      owned.filter(f => policy.gate(f.attr) == Gate.AfterMatch).map(_.attr).distinct)

  private def freshness(fs: Vector[Fact]): Float =
    val volatile = liveAttrs.filter(_.volatile).map(_.slug).toSet
    val vol = fs.filter(f => volatile.contains(f.attr))
    if vol.isEmpty then 1.0f
    else (vol.map(f => math.pow(2, -(now() - f.ts).toDouble / halfLifeMs))
      .sum / vol.length).toFloat

  def candidates(q: Query): Vector[Ranked] =
    val pool = matchable(q.side).groupBy(_.profile)
    val passing = pool.filter { (_, fs) =>
      q.filters.forall { (slug, pred) =>
        fs.exists(f => f.attr == slug && Pred.holds(pred, f.value))
      }
    }
    val qe = if q.text.nonEmpty then embed(q.text) else null
    passing.toVector.map { (p, fs) =>
      val base = if qe == null then 1.0f else Vectors.cosine(qe,
        embed(fs.map(f => f.attr + " " + Value.text(f.value)).mkString(" ")))
      val (open, gated) = disclose(fs)
      Ranked(p, base * freshness(fs), open, gated)
    }.sortBy(-_.score).take(q.k)

  // ---- identity recovery (stage 2): the hash seam, no dependency ----

  def setRecovery(p: ProfileId, secret: String): Unit =
    exec("DELETE FROM match_recovery WHERE profile = ?", Vector(Text(p.uuid)))
    exec("INSERT INTO match_recovery VALUES(?,?)",
      Vector(Text(p.uuid), Text(hash(secret))))
    ()

  def rebind(oldEmail: String, newEmail: String, secret: String): Option[ProfileId] =
    rows("SELECT uuid FROM match_profiles WHERE email = ?",
      Vector(Text(oldEmail))) match
      case Vector(Vector(Text(u))) =>
        val ok = rows("SELECT secret FROM match_recovery WHERE profile = ?",
          Vector(Text(u))) match
          case Vector(Vector(Text(stored))) => verifyHash(secret, stored)
          case _ => false
        if ok then
          exec("UPDATE match_profiles SET email = ? WHERE uuid = ?",
            Vector(Text(newEmail), Text(u)))
          Some(ProfileId(u))
        else None
      case _ => None

  // ---- the handlers, same shape as the memory reference -------------

  given registry: Handler[Registry] = new:
    def handle[A](e: Registry[A]): A = e match
      case Registry.Search(t) => registrySearch(t)
      case Registry.Propose(d) => propose(d)
      case Registry.Get(x) => get(x)

  given factsH: Handler[Facts] = new:
    def handle[A](e: Facts[A]): A = e match
      case Facts.Register(email) => register(email)
      case Facts.Assert(p, a, sd, v, prov, c, vis) => assert(p, a, sd, v, prov, c, vis)
      case Facts.Supersede(id, v, r, prov) => supersede(id, v, r, prov)
      case Facts.ProfileOf(id) => profileOf(id)

  given find: Handler[Find] = new:
    def handle[A](e: Find[A]): A = e match
      case Find.Candidates(q) => candidates(q)
}
