package okay.security

import okay.persist.MemoryStore

/** specs/identity-roster.md — an address bound to a principal, roles as
 * grants, a policy over them, and a roster that is a fold of its topic */
class TestRoster extends munit.FunSuite:
  private var clock = 1_000L
  private def tick(): Long = { clock += 1; clock }
  private def fresh() = MemoryStore().topic("roster")
  private def p(id: String) = Principal(id, id, Claims())

  test("owned: the configured address is the owner, the console is the owner, anyone else is nobody") {
    val t = fresh()
    val r = Roster.owned(t, Channel.Telegram, "123", tick)
    val owner = r.whoIs(Channel.Telegram, "123").getOrElse(fail("no owner"))
    assertEquals(owner.id, "owner")
    assertEquals(Policy.role("owner")(owner, "run", "/x"), Decision.Permit)
    assertEquals(r.whoIs(Channel.Console, "").map(_.id), Some("owner"))
    assertEquals(r.whoIs(Channel.Telegram, "124"), None)
    // idempotent on a roster that exists: loading, not re-writing
    val again = Roster.owned(t, Channel.Telegram, "123", tick)
    assertEquals(again.bindings.size, 2)
    assertEquals(again.grants.size, 1)
  }

  test("bind round-trips; a second bind of the same address replaces the first") {
    val r = Roster(fresh(), tick)
    r.bind(Channel.Telegram, "555", "ada"): Unit
    assertEquals(r.whoIs(Channel.Telegram, "555").map(_.id), Some("ada"))
    assertEquals(r.whoIs(Channel.Telegram, "555").map(_.name), Some("555"))
    r.bind(Channel.Telegram, "555", "bob"): Unit
    assertEquals(r.whoIs(Channel.Telegram, "555").map(_.id), Some("bob"))
    assertEquals(r.addressesOf("ada"), Vector.empty)
    assertEquals(r.addressesOf("bob"), Vector(Channel.Telegram -> "555"))
    assert(r.unbind(Channel.Telegram, "555"))
    assert(!r.unbind(Channel.Telegram, "555"))
    assertEquals(r.whoIs(Channel.Telegram, "555"), None)
  }

  test("a scoped grant permits its prefix and denies the rest; an unscoped one permits everything") {
    val r = Roster(fresh(), tick)
    r.grant("ada", "operator", Some("/work/x")): Unit
    val op = Roster.role(r, "operator")
    assertEquals(op(p("ada"), "run", "/work/x"), Decision.Permit)
    assertEquals(op(p("ada"), "run", "/work/x/sub"), Decision.Permit)
    assert(op(p("ada"), "run", "/work/y").isInstanceOf[Decision.Deny])
    assert(op(p("bob"), "run", "/work/x").isInstanceOf[Decision.Deny])
    r.grant("bob", "operator"): Unit
    assertEquals(op(p("bob"), "run", "/anything"), Decision.Permit)
    // the roles ride in the principal's claims, where Policy.role reads them
    r.bind(Channel.Web, "s1", "bob"): Unit
    val bob = r.whoIs(Channel.Web, "s1").get
    assertEquals(Policy.role("operator")(bob, "run", "/x"), Decision.Permit)
  }

  test("revoke of a role not held is false and appends nothing") {
    val t = fresh(); val r = Roster(t, tick)
    val before = t.end(0)
    assert(!r.revoke("ada", "operator"))
    assertEquals(t.end(0), before)
    r.grant("ada", "operator"): Unit
    assert(r.revoke("ada", "operator"))
    assertEquals(r.rolesOf("ada"), Vector.empty)
    assert(!r.revoke("ada", "operator", Some("/x")), "a different scope is a different grant")
  }

  test("Roster.role composes with anyOf/allOf") {
    val r = Roster(fresh(), tick)
    r.grant("ada", "operator"): Unit
    val either = Policy.anyOf(Roster.role(r, "owner"), Roster.role(r, "operator"))
    assertEquals(either(p("ada"), "run", "/x"), Decision.Permit)
    assert(Policy.allOf(Roster.role(r, "owner"), Roster.role(r, "operator"))(p("ada"), "run", "/x").isInstanceOf[Decision.Deny])
  }

  test("a roster reloaded from its topic equals the one that wrote it — over random operations") {
    val rnd = new scala.util.Random(7)
    val chans = Vector(Channel.Telegram, Channel.Web, Channel.Mcp, Channel.Other("irc"))
    for round <- 1 to 20 do
      val t = fresh(); val r = Roster(t, tick)
      for _ <- 1 to 40 do rnd.nextInt(4) match
        case 0 => r.bind(chans(rnd.nextInt(chans.size)), s"a${rnd.nextInt(5)}", s"p${rnd.nextInt(4)}"): Unit
        case 1 => r.unbind(chans(rnd.nextInt(chans.size)), s"a${rnd.nextInt(5)}"): Unit
        case 2 => r.grant(s"p${rnd.nextInt(4)}", s"r${rnd.nextInt(3)}", if rnd.nextBoolean() then None else Some(s"/s${rnd.nextInt(2)}")): Unit
        case _ => r.revoke(s"p${rnd.nextInt(4)}", s"r${rnd.nextInt(3)}", if rnd.nextBoolean() then None else Some(s"/s${rnd.nextInt(2)}")): Unit
      val again = Roster(t, tick)
      assertEquals(again.bindings, r.bindings, s"round $round bindings")
      assertEquals(again.grants, r.grants, s"round $round grants")
      assertEquals(again.principals, r.principals, s"round $round principals")
  }

  test("no record carries anything but what was written: channel, address, principal, role, scope, time") {
    val t = fresh(); val r = Roster(t, tick)
    r.bind(Channel.Telegram, "1", "ada"): Unit
    r.grant("ada", "owner"): Unit
    val Topic = okay.persist.Topic
    val texts = t.read(0, 0, 10) match
      case Topic.Read.Records(rs) => rs.map(x => new String(x.value, "UTF-8"))
      case _ => fail("readable")
    assertEquals(texts.size, 2)
    assert(texts.forall(s => s.contains("\"kind\"") && s.contains("\"at\"")))
    assert(!texts.mkString.contains("token"))
  }

