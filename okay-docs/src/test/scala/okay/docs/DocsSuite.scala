package okay.docs

import okay.{!, +, Async, Chunk, Produce, Stream}
import okay.given
import okay.codec.Schema
import munit.FunSuite

/** the shared fixture: a document with one declared index */
final case class Person(name: String, city: String)
object Person:
  given Schema[Person] = Schema.derived
  val indexes: Map[String, Person => String] = Map("city" -> (_.city))

/**
 * The contract every Docs engine must honor (specs/data.md — the
 * StoreSuite pattern at the document seam): CAS semantics as data,
 * declared-index queries, the lost-ack retry landing once. Run
 * over the own-posture TopicDocs and every foreign adapter.
 */
abstract class DocsSuite extends FunSuite:

  /** a fresh store per test */
  def mkDocs(): Docs[Person]

  /** cross-platform runner: these programs are Run-only, so the
   * drive completes inline — no CanBlock, so the suite runs on JS
   * and Native (the TestCache precedent) */
  def run[A](prog: A ! Async): A =
    Async.runAsync(prog).value match
      case Some(t) => t.get
      case None => fail("the test program did not complete synchronously")

  def collect[A](s: Chunk[A] ! (Produce + Async)): List[A] =
    val S = summon[Stream[[X] =>> X ! (Produce + Async), Async]]
    def go(rest: Chunk[A] ! (Produce + Async)): Vector[A] ! Async =
      S.uncons(rest).flatMap {
        case None => okay.pure(Vector.empty)
        case Some((c, more)) => go(more).map(c.toVector ++ _)
      }
    run(go(s)).toList

  test("put then get round-trips; versions are monotone per document") {
    val d = mkDocs()
    val PutResult.Applied(v1) = run(d.put("ann", Person("Ann", "Kyiv"))): @unchecked
    val got = run(d.get("ann")).getOrElse(fail("absent after put"))
    assertEquals(got.value, Person("Ann", "Kyiv"))
    assertEquals(got.version, v1)
    val PutResult.Applied(v2) = run(d.put("ann", Person("Ann", "Lviv"))): @unchecked
    assert(v2 > v1, s"version did not advance: $v1 -> $v2")
    assertEquals(run(d.get("ann")).map(_.value), Some(Person("Ann", "Lviv")))
    assertEquals(run(d.get("nobody")), None)
  }

  test("IfAbsent: the first write applies, the second answers Stale with the current version") {
    val d = mkDocs()
    val PutResult.Applied(v1) = run(d.put("a", Person("A", "X"), Cond.IfAbsent)): @unchecked
    run(d.put("a", Person("A2", "Y"), Cond.IfAbsent)) match
      case PutResult.Stale(cur) => assertEquals(cur, Some(v1))
      case other => fail(s"expected Stale, got $other")
    assertEquals(run(d.get("a")).map(_.value), Some(Person("A", "X")))
  }

  test("IfVersion: the right token applies, the wrong one answers Stale carrying what holds NOW") {
    val d = mkDocs()
    val PutResult.Applied(v1) = run(d.put("doc", Person("P", "X"))): @unchecked
    val PutResult.Applied(v2) = run(d.put("doc", Person("P", "Y"), Cond.IfVersion(v1))): @unchecked
    run(d.put("doc", Person("P", "Z"), Cond.IfVersion(v1))) match
      case PutResult.Stale(cur) => assertEquals(cur, Some(v2))
      case other => fail(s"a stale CAS applied: $other")
    assertEquals(run(d.get("doc")).map(_.value), Some(Person("P", "Y")))
  }

  test("delete removes; a conditional delete respects the version") {
    val d = mkDocs()
    val PutResult.Applied(v1) = run(d.put("gone", Person("G", "X"))): @unchecked
    run(d.delete("gone", Cond.IfVersion(v1 + 999))) match
      case PutResult.Stale(_) => ()
      case other => fail(s"a stale conditional delete applied: $other")
    assert(run(d.get("gone")).isDefined)
    run(d.delete("gone", Cond.IfVersion(v1))) match
      case PutResult.Applied(_) => ()
      case other => fail(s"the right version refused: $other")
    assertEquals(run(d.get("gone")), None)
  }

  test("query walks a DECLARED index, bounded; an undeclared field refuses loudly") {
    val d = mkDocs()
    run(d.put("a", Person("A", "Kyiv"))): Unit
    run(d.put("b", Person("B", "Lviv"))): Unit
    run(d.put("c", Person("C", "Kyiv"))): Unit
    run(d.put("d", Person("D", "Kyiv"))): Unit
    val kyiv = collect(d.query("city", "Kyiv", max = 2))
    assertEquals(kyiv.length, 2)
    assert(kyiv.forall(_._2.city == "Kyiv"))
    val all = collect(d.query("city", "Kyiv", max = 10))
    assertEquals(all.map(_._1).sorted, List("a", "c", "d"))
    intercept[IllegalArgumentException](collect(d.query("name", "A", 10))): Unit
  }

  test("the lost-ack retry lands once: CAS is WithKey at this seam") {
    val d = mkDocs()
    // the write applied but the ack was lost; the retry carries the
    // SAME condition — and the far end answers "already happened"
    val PutResult.Applied(v1) = run(d.put("order-1", Person("O", "X"), Cond.IfAbsent)): @unchecked
    run(d.put("order-1", Person("O", "X"), Cond.IfAbsent)) match
      case PutResult.Stale(Some(v)) => assertEquals(v, v1)
      case other => fail(s"the retry did not deduplicate: $other")
    // exactly one document, exactly the first version
    assertEquals(run(d.get("order-1")).map(_.version), Some(v1))
  }

  test("grants answers the engine's honest mapping") {
    val d = mkDocs()
    for c <- Consistency.values do
      val g = d.grants(c)
      assert(g.ordinal >= c.ordinal || g == d.grants(Consistency.Strong),
        s"granted $g for requested $c — an engine may strengthen or state its truth, not lie upward")
  }
