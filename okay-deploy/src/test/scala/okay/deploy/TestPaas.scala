package okay.deploy

import okay.codec.Json
import okay.conf.Secret

/**
 * specs/deployment.md, stage 2: the fly, render and railway targets,
 * as arithmetic over the value.
 *
 * The `railway.json` parse lives here rather than in the Live suite
 * because okay-codec's own `Json` needs nothing outside the JVM.
 * `fly.toml` and `render.yaml` need a real parser that is a
 * subprocess, so those are `TestPaasParsers`.
 */
class TestPaas extends munit.FunSuite:

  private val web = Service(
    name = "web",
    run = Run.Image("ghcr.io/okay/web", "1.4"),
    settings = Settings.of("okay")("pages" -> "/app/pages"),
    secrets = Vector(Secret("env:ADMIN_TOKEN"), Secret("file:/run/secrets/other")),
    needs = Vector(
      Need.Port(8080),
      Need.Volume("/app/data", size = "3Gi"),
      Need.Database(Engine.Postgres, "16", "shop"),
      Need.Dns("shop.example.com"),
      Need.Tls(TlsMode.Proxy),
      Need.Region("iad")),
    scale = Scale(2))

  private val worker = Service(
    name = "worker",
    run = Run.Module("okayScript", "okay-script", "okay.script.Serve"),
    needs = Vector(Need.Neighbour("web"), Need.Region("iad")))

  private val shop = Deployment("shop", Vector(web, worker))

  private def files(t: Target, d: Deployment = shop): Map[String, String] =
    t.render(d).getOrElse(fail(s"${t.name} refused: ${t.render(d)}")).toMap

  // ---- fly ----------------------------------------------------------

  test("fly is one app per service, and the file says which app it is") {
    val f = files(Paas.Fly)
    assert(f.contains("web/fly.toml"), f.keys.toString)
    assert(f.contains("worker/fly.toml"), f.keys.toString)
    assert(f("web/fly.toml").contains("""app = "shop-web""""), f("web/fly.toml"))
    assert(f("web/fly.toml").contains("""primary_region = "iad""""), f("web/fly.toml"))
    // one service is the deployment itself, not `shop-only`
    val one = Deployment("solo", Vector(web))
    assert(files(Paas.Fly, one)("web/fly.toml").contains("""app = "solo""""), files(Paas.Fly, one)("web/fly.toml"))
  }

  test("fly REFUSES without a region, because no target can invent where to run") {
    val d = shop.copy(services = Vector(web.copy(needs = web.needs.filterNot(_.isInstanceOf[Need.Region]))))
    Paas.Fly.render(d) match
      case Right(_) => fail("a region was invented")
      case Left(m) =>
        assert(m.contains("web"), m)
        assert(m.contains("Need.Region"), m)
        assert(m.contains("flyctl platform regions"), m)
  }

  test("a service with no port is a worker, and fly gives it no public address") {
    val w = files(Paas.Fly)("worker/fly.toml")
    assert(!w.contains("[http_service]"), w)
    assert(w.contains("this app is a worker"), w)
    val web1 = files(Paas.Fly)("web/fly.toml")
    assert(web1.contains("internal_port = 8080"), web1)
    assert(web1.contains("min_machines_running = 2"), web1)
  }

  test("the database is DELEGATED: the commands are exact, and neither is in the toml") {
    val f = files(Paas.Fly)
    assert(!f("web/fly.toml").contains("postgres"), f("web/fly.toml"))
    val setup = f("setup.sh")
    assert(setup.contains("flyctl postgres create --name 'shop-web-db'"), setup)
    assert(setup.contains("flyctl postgres attach 'shop-web-db' --app 'shop-web'"), setup)
    assert(setup.contains("not expressible in fly.toml") || setup.contains("neither is expressible"), setup)
  }

  test("fly secrets are set by reference, and no value is rendered anywhere") {
    val f = files(Paas.Fly)
    assert(f("setup.sh").contains("flyctl secrets set --app 'shop-web'"), f("setup.sh"))
    assert(f("setup.sh").contains("ADMIN_TOKEN=\"${ADMIN_TOKEN:?"), f("setup.sh"))
    // a file: reference resolves inside the process and needs nothing here
    assert(!f.values.exists(_.contains("/run/secrets/other")), "a file: reference was wired as a platform secret")
  }

  test("fly volumes and certificates are in setup, once each, with their sizes") {
    val setup = files(Paas.Fly)("setup.sh")
    assert(setup.contains("flyctl volumes create 'web_data' --app 'shop-web' --region 'iad' --size 3"), setup)
    assert(setup.contains("flyctl certs create 'shop.example.com' --app 'shop-web'"), setup)
    assert(files(Paas.Fly)("web/fly.toml").contains("""destination = "/app/data""""), files(Paas.Fly)("web/fly.toml"))
  }

  test("deploy.sh names every app, because one command does not deploy three") {
    val sh = files(Paas.Fly)("deploy.sh")
    assert(sh.contains("--app \"shop-web\""), sh)
    assert(sh.contains("--app \"shop-worker\""), sh)
    assertEquals(sh.linesIterator.count(_.startsWith("flyctl deploy")), 2, sh)
  }

  // ---- render -------------------------------------------------------

  test("render takes the whole system in one Blueprint, databases included") {
    val y = files(Paas.Render)("render.yaml")
    assert(y.contains("  - type: web\n    name: web\n"), y)
    assert(y.contains("  - type: worker\n    name: worker\n"), y)
    assert(y.contains("databases:"), y)
    assert(y.contains("- name: web-db"), y)
    assert(y.contains("""databaseName: "shop""""), y)
  }

  test("render wires the database URL as a REFERENCE to the database it declared") {
    val y = files(Paas.Render)("render.yaml")
    assert(y.contains("- key: DB_URL"), y)
    assert(y.contains("fromDatabase:"), y)
    assert(y.contains("property: connectionString"), y)
    // never a literal URL, because Render owns the credentials
    assert(!y.contains("postgresql://"), y)
  }

  test("a Render secret is `sync: false` — the platform's own way of saying a human types it") {
    val y = files(Paas.Render)("render.yaml")
    assert(y.contains("- key: ADMIN_TOKEN\n        sync: false"), y)
  }

  test("what a Blueprint cannot say is written down rather than dropped") {
    val d = shop.copy(services = Vector(web.copy(needs = web.needs :+ Need.Cache(Engine.Redis, "7"))))
    val y = files(Paas.Render, d)("render.yaml")
    assert(y.contains("NOT in this file"), y)
    assert(y.contains("Redis"), y)
    assert(y.contains("Key Value"), y)
  }

  test("render has no `up`: a Blueprint is applied from the repository, and we say so") {
    assert(Paas.Render.up(java.nio.file.Path.of("/x")).mkString(" ").contains("git repository"),
      Paas.Render.up(java.nio.file.Path.of("/x")).toString)
    assertEquals(Paas.Render.requires(shop), Vector.empty)
  }

  // ---- railway ------------------------------------------------------

  test("railway.json is JSON this build can read back, with the schema named") {
    val text = files(Paas.Railway)("web/railway.json")
    Json.parse(text) match
      case Json.JObj(fs) =>
        assertEquals(fs.collectFirst { case ("$schema", Json.JStr(s)) => s },
          Some("https://railway.com/railway.schema.json"))
        val deploy = fs.collectFirst { case ("deploy", Json.JObj(d)) => d }.getOrElse(fail(s"no deploy: $text"))
        assertEquals(deploy.collectFirst { case ("numReplicas", Json.JNum(n)) => n.toInt }, Some(2))
        assertEquals(deploy.collectFirst { case ("healthcheckPath", Json.JStr(p)) => p }, Some("/readyz"))
        assertEquals(deploy.collectFirst { case ("region", Json.JStr(r)) => r }, Some("iad"))
      case other => fail(s"not an object: $other")
  }

  test("a railway worker has no health check, because there is nothing to check") {
    Json.parse(files(Paas.Railway)("worker/railway.json")) match
      case Json.JObj(fs) =>
        val deploy = fs.collectFirst { case ("deploy", Json.JObj(d)) => d }.getOrElse(fail("no deploy"))
        assert(!deploy.exists(_._1 == "healthcheckPath"), deploy.toString)
      case other => fail(s"not an object: $other")
  }

  test("railway's setup carries the project, the engines and the variables — never a value") {
    val setup = files(Paas.Railway)("setup.sh")
    assert(setup.contains("railway init --name 'shop'"), setup)
    assert(setup.contains("railway add --database 'postgres'"), setup)
    assert(setup.contains("--set 'OKAY_PAGES=/app/pages'"), setup)
    assert(setup.contains("ADMIN_TOKEN=${ADMIN_TOKEN:?"), setup)
    assert(setup.contains("Volumes"), setup)
  }

  // ---- all three ----------------------------------------------------

  test("no PaaS rendering carries a secret VALUE, on any target") {
    val resolved = Vector("hunter2", "s3cr3t", "local-only")
    for t <- Vector(Paas.Fly, Paas.Render, Paas.Railway); (path, content) <- files(t) do
      for bad <- resolved do assert(!content.contains(bad), s"${t.name}/$path")
  }

  test("every PaaS target is in Targets.all and answers to its own name") {
    for n <- Vector("fly", "render", "railway") do
      assertEquals(Targets.byName(n).map(_.name), Some(n))
    assertEquals(Targets.all.length, 6)
  }

  test("Need.Region round-trips through the Schema, like every other need") {
    assertEquals(Deployment.read(Deployment.json(shop)), Right(shop))
  }
