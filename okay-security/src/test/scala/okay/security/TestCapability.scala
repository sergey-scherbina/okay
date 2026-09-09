package okay.security


/**
 * The asymmetry is the whole product: narrowing is free, widening is
 * impossible. A suite that only shows a valid token verifying proves
 * none of it, so the tests that MATTER here are the ones that must
 * fail — and each of the three from the claim has its own.
 */
class TestCapability extends munit.FunSuite {

  private val root: Array[Byte] = Array.tabulate(32)(i => (i * 7 + 3).toByte)
  private val other: Array[Byte] = Array.tabulate(32)(i => (i * 11 + 5).toByte)
  private val Now = 1_700_000_000_000L

  private def anything: String => Boolean = _ => true

  test("a fresh capability verifies against its root key") {
    val c = Capability.issue(root, "alice")
    assert(c.verify(root, anything))
    assertEquals(c.caveats, Vector.empty)
  }

  test("a capability does NOT verify against a different key") {
    val c = Capability.issue(root, "alice")
    assert(!c.verify(other, anything), "any key must not do")
  }

  // ── the first half of the claim: narrowing is free ──────────────

  test("ANYONE CAN NARROW: attenuate needs no key, and the result still verifies") {
    // the root key is not in scope for this holder at all — it only
    // has the token. That is the property; if `attenuate` needed the
    // key this would be a re-issue and the type would be pointless.
    val issued = Capability.issue(root, "alice")
    val narrowed = holderNarrows(issued)
    assert(narrowed.verify(root, anything), "a narrowed token must still verify")
    assertEquals(narrowed.caveats, Vector("scope=read", s"until=${Now + 1000}"))
  }

  /** a holder with NO access to `root`: everything it can do, it does
   * from the token alone */
  private def holderNarrows(c: Capability)(using Crypto): Capability =
    c.attenuate(Caveat.Scope("read")).attenuate(Caveat.Until(Now + 1000))

  test("narrowing twice, by two different holders in turn, still verifies") {
    val a = Capability.issue(root, "alice").attenuate(Caveat.Scope("read"))
    val b = a.attenuate(Caveat.Scope("chat-7"))
    assert(b.verify(root, Capability.checking(Now, Set("read", "chat-7"))))
  }

  // ── the second half: widening is impossible ─────────────────────

  test("NOBODY CAN WIDEN: dropping a caveat breaks the chain") {
    val full = Capability.issue(root, "alice")
      .attenuate(Caveat.Scope("read"))
      .attenuate(Caveat.Until(Now + 1000))
    // the attacker keeps the tag and drops the restriction
    val widened = full.copy(caveats = full.caveats.dropRight(1))
    assert(!widened.verify(root, anything),
      "a token with a caveat removed must not verify")
  }

  test("NOBODY CAN WIDEN: editing a caveat breaks the chain") {
    val c = Capability.issue(root, "alice").attenuate(Caveat.Until(Now + 1000))
    val later = c.copy(caveats = Vector(s"until=${Now + 999_999_999}"))
    assert(!later.verify(root, anything), "an edited caveat must not verify")
  }

  test("NOBODY CAN WIDEN: the subject and the id are signed too") {
    val c = Capability.issue(root, "alice").attenuate(Caveat.Scope("read"))
    assert(!c.copy(subject = "bob").verify(root, anything), "subject is signed")
    assert(!c.copy(id = "00000000000000000000000000").verify(root, anything), "id is signed")
  }

  test("a caveat cannot be smuggled past the chain by containing a separator") {
    // the id/subject pair is CHAINED, not concatenated, so no pair of
    // fields can collide however they are spelled
    val a = Capability.issueAs(root, "a", "b.c")
    val b = Capability.issueAs(root, "a.b", "c")
    assert(a.tag != b.tag, "two different (id, subject) pairs must not sign alike")
  }

  // ── the third: a valid signature is not authority ───────────────

  test("A PERFECT SIGNATURE IS NOT ENOUGH: an expired token is refused") {
    val c = Capability.issue(root, "alice").attenuate(Caveat.Until(Now))
    // the chain is intact — prove that first, so the refusal below is
    // known to come from the caveat and not from a broken signature
    assert(c.verify(root, anything), "the signature itself is valid")
    assert(!c.verify(root, Capability.checking(Now, Set.empty)),
      "until=Now must refuse at Now: the bound is strict")
    assert(c.verify(root, Capability.checking(Now - 1, Set.empty)),
      "and must allow a moment earlier")
  }

  test("A PERFECT SIGNATURE IS NOT ENOUGH: a missing scope is refused") {
    val c = Capability.issue(root, "alice").attenuate(Caveat.Scope("write"))
    assert(c.verify(root, anything), "the signature itself is valid")
    assert(!c.verify(root, Capability.checking(Now, Set("read"))))
    assert(c.verify(root, Capability.checking(Now, Set("read", "write"))))
  }

  test("an UNKNOWN caveat is refused, never ignored") {
    // ignoring what it cannot enforce would grant more than the token
    // says, which is the one failure mode worth being paranoid about
    val c = Capability.issue(root, "alice").attenuate("region=eu-only")
    assert(c.verify(root, anything), "the signature itself is valid")
    assert(!c.verify(root, Capability.checking(Now, Set("read"))),
      "a verifier that does not understand a restriction must refuse")
  }

  test("scopes intersect, because every caveat must hold") {
    val c = Capability.issue(root, "alice")
      .attenuate(Caveat.Scope("read"))
      .attenuate(Caveat.Scope("write"))
    assert(!c.verify(root, Capability.checking(Now, Set("read"))))
    assert(c.verify(root, Capability.checking(Now, Set("read", "write"))))
  }

  // ── the wire ────────────────────────────────────────────────────

  test("round trip through the wire form") {
    val c = Capability.issue(root, "alice")
      .attenuate(Caveat.Scope("read"))
      .attenuate(Caveat.Until(Now + 60_000))
    val back = Capability.decode(c.encoded)
    assertEquals(back, Some(c))
    assert(back.get.verify(root, Capability.checking(Now, Set("read"))))
  }

  test("the wire form survives a caveat full of separators and unicode") {
    // the prediction in this lane's claim was that the wire round trip
    // would break first, on a caveat carrying the separator
    val awkward = "note=a.b.c d/e+f=g привет"
    val c = Capability.issue(root, "alice").attenuate(awkward)
    val back = Capability.decode(c.encoded)
    assertEquals(back, Some(c))
    assertEquals(back.get.caveats, Vector(awkward))
    assert(back.get.verify(root, _ == awkward))
  }

  test("garbage on the wire answers None instead of throwing") {
    assertEquals(Capability.decode(""), None)
    assertEquals(Capability.decode("okc1"), None)
    assertEquals(Capability.decode("okc1.a.b"), None)
    assertEquals(Capability.decode("nope.YQ.Yg.00ff"), None, "the version is checked")
    assertEquals(Capability.decode("okc1.!!!.Yg.00ff"), None, "bad base64")
    assertEquals(Capability.decode("okc1.YQ.Yg.zz"), None, "the tag must be hex")
  }

  test("a decoded token whose tag was edited does not verify") {
    val c = Capability.issue(root, "alice")
    val flipped = c.tag.updated(0, if c.tag.charAt(0) == 'a' then 'b' else 'a')
    val forged = Capability.decode(c.copy(tag = flipped).encoded)
    assert(forged.isDefined, "it still parses")
    assert(!forged.get.verify(root, anything), "and it does not verify")
  }

  test("Caveat.parse reads back what Caveat writes, and refuses nonsense") {
    assertEquals(Caveat.parse("until=17"), Some(Caveat.Until(17L)))
    assertEquals(Caveat.parse("scope=read"), Some(Caveat.Scope("read")))
    assertEquals(Caveat.parse("until=notanumber"), None)
    assertEquals(Caveat.parse("scope="), None)
    assertEquals(Caveat.parse("whatever"), None)
    assertEquals(Caveat.Until(17L).text, "until=17")
    assertEquals(Caveat.Scope("read").text, "scope=read")
  }
}
