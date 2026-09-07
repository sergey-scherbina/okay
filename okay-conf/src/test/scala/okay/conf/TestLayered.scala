package okay.conf

import okay.codec.Schema

/**
 * specs/conf.md, "The layering, and why the order is that one":
 * defaults, then a file over them, then the environment over that.
 */
class TestLayered extends munit.FunSuite:

  final case class Conf(
    pages: String = "pages",
    port: Int = 8080,
    ops: Boolean = false,
    ratio: Double = 0.5,
    tlsKey: Secret = Secret(""),
    langs: Vector[String] = Vector("en"))

  given Schema[Conf] = Schema.derived

  private def env(pairs: (String, String)*): String => Option[String] = pairs.toMap.get

  test("the order is defaults, file, environment — pinned by making all three disagree") {
    val got = okay.conf.Conf.layered(
      Conf(),
      Some("""{"pages":"from-file","port":9000}"""),
      env("OKAY_PORT" -> "9999"),
      "okay")
    // the field only the defaults set stays; the file wins over the
    // defaults; the environment wins over the file
    assertEquals(got, Right(Conf(pages = "from-file", port = 9999)))
  }

  test("a partial file changes one field, because a file that must be complete is a file nobody edits") {
    assertEquals(
      okay.conf.Conf.layered(Conf(), Some("""{"ops":true}"""), env(), "okay"),
      Right(Conf(ops = true)))
  }

  test("a variable whose text is not a value of that field's type is a refusal NAMING it") {
    val bad = okay.conf.Conf.layered(Conf(), None, env("OKAY_PORT" -> "eighty"), "okay")
    assert(bad.left.exists(_.contains("OKAY_PORT is not a whole number: 'eighty'")), bad.toString)
    assert(bad.left.exists(_.contains("`port`")), bad.toString)

    val notBool = okay.conf.Conf.layered(Conf(), None, env("OKAY_OPS" -> "maybe"), "okay")
    assert(notBool.left.exists(_.contains("OKAY_OPS is not a yes or a no")), notBool.toString)
  }

  test("yes and no are spelled several ways, because a unit file and a compose file spell them differently") {
    for yes <- Vector("1", "true", "TRUE", "yes", "on") do
      assertEquals(okay.conf.Conf.layered(Conf(), None, env("OKAY_OPS" -> yes), "okay").map(_.ops), Right(true), yes)
    for no <- Vector("0", "false", "no", "off") do
      assertEquals(okay.conf.Conf.layered(Conf(), None, env("OKAY_OPS" -> no), "okay").map(_.ops), Right(false), no)
  }

  test("the environment cannot invent a name: only a field's own derived name is read") {
    // OKAY_PROT is one letter from OKAY_PORT and sets nothing
    assertEquals(
      okay.conf.Conf.layered(Conf(), None, env("OKAY_PROT" -> "9999", "PORT" -> "7777"), "okay"),
      Right(Conf()))
  }

  test("a Secret arrives as its REFERENCE — which is exactly what belongs in a unit file") {
    assertEquals(
      okay.conf.Conf.layered(Conf(), None, env("OKAY_TLS_KEY" -> "file:/run/secrets/key.pem"), "okay").map(_.tlsKey),
      Right(Secret("file:/run/secrets/key.pem")))
  }

  test("a field the environment cannot carry refuses BY NAME, and only when a variable is actually set for it") {
    // untouched: a config may hold a list nobody configures from a container
    assertEquals(okay.conf.Conf.layered(Conf(), None, env(), "okay").map(_.langs), Right(Vector("en")))
    val set = okay.conf.Conf.layered(Conf(), None, env("OKAY_LANGS" -> "en,uk"), "okay")
    assert(set.left.exists(_.contains("OKAY_LANGS cannot come from the environment")), set.toString)
    assert(set.left.exists(_.contains("a list")), set.toString)
  }

  test("an empty variable is not a value: it is the same as not setting it") {
    assertEquals(okay.conf.Conf.layered(Conf(), None, env("OKAY_PAGES" -> ""), "okay").map(_.pages), Right("pages"))
  }

  test("a file that is not a JSON object is named, not swallowed") {
    val bad = okay.conf.Conf.layered(Conf(), Some("[1,2,3]"), env(), "okay")
    assert(bad.left.exists(_.contains("must be a JSON object")), bad.toString)
  }

  test("envName is the one derivation: camelCase to PREFIX_SNAKE_CASE") {
    assertEquals(okay.conf.Conf.envName("okay", "tlsReload"), "OKAY_TLS_RELOAD")
    assertEquals(okay.conf.Conf.envName("okay", "port"), "OKAY_PORT")
    assertEquals(okay.conf.Conf.envName("okay", "acmeDomains"), "OKAY_ACME_DOMAINS")
    assertEquals(okay.conf.Conf.envName("", "port"), "PORT")
  }

  test("fromEnv is a PATCH: only the fields a variable is set for") {
    assertEquals(
      okay.conf.Conf.fromEnv[Conf]("okay", env("OKAY_OPS" -> "1")),
      Right(okay.codec.Json.JObj(Vector("ops" -> okay.codec.Json.JBool(true)))))
  }
