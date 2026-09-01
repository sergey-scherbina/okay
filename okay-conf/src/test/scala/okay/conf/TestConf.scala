package okay.conf

import okay.codec.Schema

/**
 * The invariants argued where they can fail (specs/conf.md): errors
 * name references and never values, toString is the reference, the
 * chain answers once, an unrecognized scheme refuses rather than
 * guesses. SHARED suite — the environment exists on JVM, Node and
 * Native alike, so `env:` is proven on all three.
 */
class TestConf extends munit.FunSuite {

  final case class Db(url: String, user: String, password: Secret,
                      poolSize: Option[Int] = None)
  given Schema[Db] = Schema.derived

  val json = """{"url":"jdbc:h2:mem:t","user":"app","password":"env:PG_PASSWORD"}"""

  test("a config loads via its derived Schema; absent optional fields decode absent") {
    assertEquals(Conf.read[Db](json),
      Right(Db("jdbc:h2:mem:t", "app", Secret("env:PG_PASSWORD"), None)))
  }

  test("a Secret round-trips: the reference intact, no other representation") {
    val Right(db) = Conf.read[Db](json): @unchecked
    val out = okay.codec.Json.write(db)
    assert(out.contains("\"env:PG_PASSWORD\""), out)
    assertEquals(Conf.read[Db](out), Right(db))
  }

  test("Secret.toString is the reference — logs are where discipline fails") {
    assertEquals(Secret("env:PG_PASSWORD").toString, "env:PG_PASSWORD")
    assertEquals(s"config: ${Secret("file:/run/secrets/pg")}", "config: file:/run/secrets/pg")
  }

  test("env: resolves from the real environment on THIS platform") {
    // PATH exists on every platform this suite runs on
    assert(Secrets.env.get(Secret("env:PATH")).isRight)
  }

  test("a missing variable names the reference and nothing resembling a value") {
    val miss = Secrets.env.get(Secret("env:OKAY_SURELY_NOT_SET_42"))
    assertEquals(miss, Left("'env:OKAY_SURELY_NOT_SET_42' is not set"))
  }

  test("an unrecognized scheme refuses by name; the reference is never the value") {
    val out = Secrets.env.get(Secret("vault:pg/password"))
    assertEquals(out, Left("unrecognized scheme 'vault' in 'vault:pg/password'"))
  }

  test("chain: the first resolver that answers wins; all-miss errors once, specifically") {
    val a = Secrets.memory(Map("env:X" -> "from-a"))
    val b = Secrets.memory(Map("env:X" -> "from-b", "env:Y" -> "y"))
    val c = Secrets.chain(a, b)
    assertEquals(c.get(Secret("env:X")), Right("from-a"))
    assertEquals(c.get(Secret("env:Y")), Right("y"))
    // env's own miss (a matched scheme) beats memory's generic one
    val specific = Secrets.chain(Secrets.env, a).get(Secret("env:OKAY_SURELY_NOT_SET_42"))
    assertEquals(specific, Left("'env:OKAY_SURELY_NOT_SET_42' is not set"))
  }

  test("memory serves tests — and there is deliberately no plain: scheme") {
    val m = Secrets.memory(Map("k" -> "v"))
    assertEquals(m.get(Secret("k")), Right("v"))
    assertEquals(Secrets.env.get(Secret("plain:hunter2")),
      Left("unrecognized scheme 'plain' in 'plain:hunter2'"))
  }
}
