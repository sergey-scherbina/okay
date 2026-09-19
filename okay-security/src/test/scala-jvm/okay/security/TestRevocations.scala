package okay.security

import okay.*
import okay.given

/**
 * The list is somebody else's, which changes nothing about the check
 * and everything about what can go wrong. So the tests that matter
 * are the failures: a registry that breaks must not un-revoke anyone,
 * and a snapshot nobody has refreshed must not quietly pass for
 * evidence.
 */
class TestRevocations extends munit.FunSuite {

  private val Fresh = 60_000L
  private def now(t: Long): () => Long = () => t

  private def source(answer: Either[String, Set[String]]): Revocations.Source =
    () => pure(answer)

  private def run[A](p: A ! Async): A = Async.run[A, Pure](p).runWith

  test("a refreshed snapshot revokes what it holds, and nothing else") {
    val r = Revocations(Fresh, Revocations.Stale.Allow)
    run(r.refresh(source(Right(Set("a1", "a2"))))(now(1000)))
    val off = r.revoked(now(1000))
    assert(off("a1") && off("a2"))
    assert(!off("a3"))
    assertEquals(r.age(1500), Some(500L))
    assertEquals(r.failure, None)
  }

  test("A FAILED REFRESH KEEPS THE PREVIOUS LIST — a broken registry un-revokes nobody") {
    val r = Revocations(Fresh, Revocations.Stale.Allow)
    run(r.refresh(source(Right(Set("a1"))))(now(1000)))
    run(r.refresh(source(Left("503 from the registry")))(now(2000)))

    assert(r.revoked(now(2000))("a1"), "the list a failure could not replace still holds")
    assertEquals(r.failure, Some("503 from the registry"))
    // and the age is the last GOOD answer's, not the last attempt's
    assertEquals(r.age(2000), Some(1000L))
  }

  test("a source that THROWS is a failure like any other: named, and the list kept") {
    val r = Revocations(Fresh, Revocations.Stale.Allow)
    run(r.refresh(source(Right(Set("a1"))))(now(1000)))
    val exploding: Revocations.Source = () => sys.error("connection reset")
    run(r.refresh(exploding)(now(2000)))

    assert(r.revoked(now(2000))("a1"))
    assert(r.failure.exists(_.contains("connection reset")), r.failure.toString)
  }

  test("past freshFor: Deny refuses everything, Allow keeps serving the last answer") {
    val lenient = Revocations(Fresh, Revocations.Stale.Allow)
    val strict = Revocations(Fresh, Revocations.Stale.Deny)
    for r <- Seq(lenient, strict) do
      run(r.refresh(source(Right(Set("a1"))))(now(1000)))

    val stale = 1000 + Fresh + 1
    assert(lenient.revoked(now(stale))("a1"))
    assert(!lenient.revoked(now(stale))("a3"), "advisory: the last answer still serves")

    assert(strict.revoked(now(stale))("a1"))
    assert(strict.revoked(now(stale))("a3"),
      "load-bearing: no evidence is not evidence of none")

    // and both say how old the answer is, so an operator can see it
    assertEquals(lenient.age(stale), Some(Fresh + 1))
  }

  test("before the FIRST answer, fail-closed means what it says") {
    val strict = Revocations(Fresh, Revocations.Stale.Deny)
    val lenient = Revocations(Fresh, Revocations.Stale.Allow)
    assert(strict.revoked(now(1000))("anyone"))
    assert(!lenient.revoked(now(1000))("anyone"))
    assertEquals(strict.age(1000), None)
  }

  test("an emptied list is only ever an ANSWER, never a failure in disguise") {
    val r = Revocations(Fresh, Revocations.Stale.Allow)
    run(r.refresh(source(Right(Set("a1"))))(now(1000)))
    // the registry really did say "nobody is revoked any more"
    run(r.refresh(source(Right(Set.empty)))(now(2000)))
    assert(!r.revoked(now(2000))("a1"))
    assertEquals(r.listed, Set.empty[String])
  }

  test("the MCP door takes it unchanged — it was always just a predicate") {
    val r = Revocations(Fresh, Revocations.Stale.Allow)
    run(r.refresh(source(Right(Set("a1"))))(now(1000)))
    val root: Array[Byte] = Array.tabulate(32)(i => (i * 7 + 3).toByte)
    val leaf = Capability.issue(root, "alice").attenuate(Caveat.Agent("a1"))
    val ok = Capability.issue(root, "alice").attenuate(Caveat.Agent("a2"))

    val off = r.revoked(now(1000))
    assert(!leaf.verify(root, Capability.checking(1000, Set.empty, off)))
    assert(ok.verify(root, Capability.checking(1000, Set.empty, off)))
  }
}
