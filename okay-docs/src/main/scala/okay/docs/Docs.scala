package okay.docs

import okay.{!, +, Async, Chunk, Produce}

/**
 * The document seam (specs/data.md, "the one new seam"): the access
 * shape SQL does not cover — get/put/delete by key, bounded queries
 * over DECLARED secondary indexes, per-item atomicity. Everything
 * else in the data landscape lands on an existing seam; this trait
 * is the one addition, and it is small on purpose.
 *
 * `Cond` is the load-bearing part: compare-and-set is how WithKey
 * and optimistic concurrency are spelled in this class, and every
 * serious store offers it (Dynamo condition expressions, Mongo
 * find-and-modify, Cassandra LWT). `PutResult` answers
 * applied-or-not as DATA — a stale CAS is an answer, not an error.
 *
 * Multi-document transactions are deliberately NOT in the seam,
 * even where an engine advertises them: a multi-item change is a
 * journaled sequence of conditional writes (the Durable policies
 * over CAS — the saga pattern with machinery this stack already
 * has, and no new concepts).
 */
trait Docs[A]:

  def get(id: String): Option[Docs.Versioned[A]] ! Async

  def put(id: String, a: A, cond: Cond = Cond.Always): PutResult ! Async

  def delete(id: String, cond: Cond = Cond.Always): PutResult ! Async

  /** equality over one DECLARED index; an undeclared field refuses
   * loudly — a scan wearing a query's hat is the lie this seam
   * refuses to tell */
  def query(field: String, equals: String, max: Int = 256)
  : Chunk[(String, A)] ! (Produce + Async)

  /** the engine's honest consistency mapping (the granted-isolation
   * move): ask once at startup what a request will actually mean */
  def grants(requested: Consistency): Consistency

object Docs:
  /** a document and the version a CAS can be aimed at */
  final case class Versioned[A](version: Long, value: A)

/** the write condition — the decision, not a promise */
enum Cond:
  case Always
  case IfAbsent
  case IfVersion(v: Long)

/** applied-or-not as data; `Stale` carries what the engine holds
 * NOW, so the caller can reconcile without a second read */
enum PutResult:
  case Applied(version: Long)
  case Stale(current: Option[Long])

/** requested per operation in spirit; engines map it honestly via
 * `grants` (a single-node engine grants Strong for everything; a
 * cluster grants what its read/write concerns actually give) */
enum Consistency:
  case One, Quorum, Strong
