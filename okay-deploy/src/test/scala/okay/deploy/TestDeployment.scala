package okay.deploy

import okay.conf.Secret

/** specs/deployment.md, stage 0: the model, the settings derived from
 * a Schema, and the two targets that need no account.
 */
class TestDeployment extends munit.FunSuite:

  private val web = Service(
    name = "web",
    run = Run.Module("okayScript", "okay-script", "okay.script.Serve"),
    settings = Settings.of("okay")("pages" -> "/app/pages", "port" -> "8080"),
    secrets = Vector(Secret("env:OKAY_ADMIN_TOKEN"), Secret("file:/run/secrets/other")),
    needs = Vector(
      Need.Port(8080),
      Need.Volume("/app/data"),
      Need.Database(Engine.Postgres, "16", "shop"),
      Need.Dns("shop.example.com"),
      Need.Tls(TlsMode.Acme)))

  private val one = Deployment("shop", Vector(web))

  test("a Deployment round-trips through its Schema -- the CLI reads the value as JSON, so it must") {
    val text = Deployment.json(one)
    assertEquals(Deployment.read(text), Right(one))
    // and a secret travels as its REFERENCE, which is what makes the
    // JSON committable
    assert(text.contains("env:OKAY_ADMIN_TOKEN"), text)
  }

  test("Settings.of[A] derives environment names from a schema's fields, camelCase to SNAKE_CASE") {
    final case class Conf(pages: String, port: Int, tlsReload: Int, ops: Boolean)
    given okay.codec.Schema[Conf] = okay.codec.Schema.derived
    val s = Settings.of(Conf("/app/pages", 8080, 3600, true), "okay")
    assertEquals(s.env, Vector(
      "OKAY_PAGES" -> "/app/pages",
      "OKAY_PORT" -> "8080",
      "OKAY_TLS_RELOAD" -> "3600",
      "OKAY_OPS" -> "1"))
    assertEquals(Settings.envName("okay", "httpPort"), "OKAY_HTTP_PORT")
    assertEquals(Settings.envName("", "port"), "PORT")
  }

  test("ordered: what is needed comes first, and a cycle is a refusal naming it") {
    val a = Service("a", Run.Image("x"), needs = Vector(Need.Neighbour("b")))
    val b = Service("b", Run.Image("x"))
    assertEquals(Deployment("d", Vector(a, b)).ordered.map(_.map(_.name)), Right(Vector("b", "a")))
    val loop = Deployment("d", Vector(
      Service("a", Run.Image("x"), needs = Vector(Need.Neighbour("b"))),
      Service("b", Run.Image("x"), needs = Vector(Need.Neighbour("a")))))
    assert(loop.ordered.left.exists(m => m.contains("wait on each other") && m.contains("a") && m.contains("b")),
      loop.ordered.toString)
  }

  test("laptop: one compose file where the port, the image and the database appear once each") {
    val files = Deployment.files(one, Targets.Laptop).fold(m => fail(m), identity).toMap
    val compose = files("compose.yaml")
    assert(compose.contains("  web:\n"), compose)
    assert(compose.contains("""      - "8080:8080"""), compose)
    assert(compose.contains("      OKAY_PAGES: \"/app/pages\""), compose)
    // the database is a container beside it, with its own volume, and
    // the service is told ONE url rather than four settings
    assert(compose.contains("  web-db:\n    image: postgres:16"), compose)
    assert(compose.contains("DB_URL: \"jdbc:postgresql://web-db:5432/shop\""), compose)
    assert(compose.contains("    depends_on:\n      - web-db"), compose)
    assert(compose.contains("web-db-data:/var/lib/postgresql/data"), compose)
    assert(compose.contains("volumes:\n  web-data:\n  web-db-data:\n"), compose)
    // the healthcheck comes from the value's own probe
    assert(compose.contains("http://127.0.0.1:8080/healthz"), compose)
    // the value rides along, which is what makes the directory
    // self-contained for the CLI
    assertEquals(Deployment.read(files("deployment.json")), Right(one))
  }

  test("no rendered file carries a secret VALUE -- an env: reference becomes a pass-through and a .env.example") {
    val files = Deployment.files(one, Targets.Laptop).fold(m => fail(m), identity).toMap
    val compose = files("compose.yaml")
    assert(compose.contains("OKAY_ADMIN_TOKEN: ${OKAY_ADMIN_TOKEN:?"), compose)
    assertEquals(files(".env.example").linesIterator.filterNot(_.startsWith("#")).toVector, Vector("OKAY_ADMIN_TOKEN="))
    // a file: reference needs no plumbing: the process resolves it
    assert(!compose.contains("/run/secrets/other"), compose)
    for (_, content) <- files do assert(!content.contains("sekrit"), "a value reached a rendered file")
  }

  test("host: a unit per service, settings in an EnvironmentFile, secrets NOT installed") {
    val simple = Deployment("shop", Vector(web.copy(needs = web.needs.filterNot(_.isInstanceOf[Need.Database]))))
    val files = Deployment.files(simple, Targets.Host).fold(m => fail(m), identity).toMap
    val unit = files("web.service")
    assert(unit.contains("ExecStart=/usr/bin/env java -cp /opt/shop/web/app.jar okay.script.Serve"), unit)
    assert(unit.contains("EnvironmentFile=/etc/shop/web.env"), unit)
    assert(unit.contains("ProtectSystem=strict") && unit.contains("NoNewPrivileges=true"), unit)
    assert(unit.contains("ReadWritePaths=/app/data /var/lib/shop/web"), unit)
    val env = files("web.env")
    assert(env.contains("OKAY_PAGES=/app/pages"), env)
    // the secret is named as required and left EMPTY: an installed
    // file with a value in it is a value in the repository
    assert(env.contains("# OKAY_ADMIN_TOKEN="), env)
    assert(files("install.sh").contains("systemctl enable --now web.service"), files("install.sh"))
    // with no container in the way, a Need.Volume's path is a
    // directory on this host, and the install makes it
    assert(files("install.sh").contains("install -d -o shop -g shop /app/data"), files("install.sh"))
    assert(files("uninstall.sh").contains("were kept"), files("uninstall.sh"))
  }

  test("host REFUSES a database by name -- installing someone's Postgres is not a renderer's business") {
    val out = Deployment.files(one, Targets.Host)
    assert(out.left.exists(m => m.contains("Postgres") && m.contains("does not install one")), out.toString)
  }

  test("write and drift: the committed rendering IS the value, per file") {
    val root = java.nio.file.Files.createTempDirectory("okay-deploy-")
    try
      assertEquals(Deployment.drift(one, Targets.Laptop, root).map(_.nonEmpty), Right(true))
      Deployment.write(one, Targets.Laptop, root).fold(m => fail(m), identity): Unit
      assertEquals(Deployment.drift(one, Targets.Laptop, root), Right(Vector.empty))
      val compose = root.resolve("okay-script/deploy/laptop/compose.yaml")
      java.nio.file.Files.writeString(compose, "hand edited\n"): Unit
      assertEquals(Deployment.drift(one, Targets.Laptop, root), Right(Vector("compose.yaml")))
    finally
      java.nio.file.Files.walk(root).sorted(java.util.Comparator.reverseOrder[java.nio.file.Path]())
        .forEach(p => java.nio.file.Files.deleteIfExists(p): Unit)
  }

  test("targets name what they require, so a check can run before anything is applied") {
    assertEquals(Targets.byName("laptop").map(_.requires(one)), Some(Vector("docker")))
    assertEquals(Targets.byName("host").map(_.requires(one)), Some(Vector("systemctl", "java")))
    assertEquals(Targets.byName("nowhere"), None)
    assert(Targets.Laptop.up(java.nio.file.Path.of("/x")).startsWith(Vector("docker", "compose")))
  }
