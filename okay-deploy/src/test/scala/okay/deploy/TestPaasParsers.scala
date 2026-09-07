package okay.deploy

import okay.conf.Secret

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/**
 * A REAL parser for every rendered format (specs/deployment.md, stage
 * 2, "The gate").
 *
 * Stage 1 is the reason this suite exists: `helm lint` rejected a
 * chart whose bytes were exactly what the renderer meant to write, and
 * a golden-file test would have passed on it. There is no local
 * validator for any of these three platforms — `flyctl config
 * validate` wants an account — but a syntax error is the failure mode
 * a string-building renderer actually has, and every one of these
 * formats has a real parser on an ordinary machine.
 *
 * Live-tagged because it shells out, and skipped by name where the
 * parser is not installed. What it does NOT prove is that a platform
 * accepts the SEMANTICS; that needs an account and stays manual.
 */
class TestPaasParsers extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  private val web = Service(
    name = "web",
    run = Run.Image("ghcr.io/okay/web", "1.4"),
    // a value with a quote and a backslash in it: the escaping is
    // where a hand-rolled renderer breaks, and a parser is the only
    // thing that notices
    settings = Settings.of("okay")("pages" -> "/app/pages", "motd" -> """a "quoted" \ value"""),
    secrets = Vector(Secret("env:ADMIN_TOKEN")),
    needs = Vector(
      Need.Port(8080), Need.Volume("/app/data", size = "3Gi"),
      Need.Database(Engine.Postgres, "16", "shop"),
      Need.Cache(Engine.Redis, "7"),
      Need.Dns("shop.example.com"), Need.Tls(TlsMode.Proxy), Need.Region("iad")),
    scale = Scale(2),
    resources = Some(Resources("100m", "256Mi", "1", "512Mi")))

  private val worker = Service(
    name = "worker",
    run = Run.Module("okayScript", "okay-script", "okay.script.Serve"),
    needs = Vector(Need.Neighbour("web"), Need.Region("iad")))

  private val shop = Deployment("shop", Vector(web, worker))

  private def rendered(t: Target)(body: Path => Unit): Unit =
    val root = Files.createTempDirectory("okay-paas")
    try
      Deployment.write(shop, t, root): Unit
      body(root.resolve(Deployment.dir(shop, t.name)))
    finally
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]())
        .forEach(p => Files.deleteIfExists(p): Unit)

  private def has(cmd: Vector[String]): Boolean = Shell.run(cmd).ok

  test("every fly.toml is TOML a real parser reads, and the values survive it") {
    assume(has(Vector("python3", "-c", "import tomllib")), "python3 with tomllib is not installed")
    rendered(Paas.Fly) { dir =>
      for name <- Vector("web", "worker") do
        val toml = dir.resolve(name).resolve("fly.toml")
        val out = Shell.run(Vector("python3", "-c",
          """import tomllib,sys,json
            |with open(sys.argv[1],'rb') as f: print(json.dumps(tomllib.load(f)))
            |""".stripMargin, toml.toString))
        assert(out.ok, s"$name/fly.toml is not TOML:\n${out.text}\n---\n${Files.readString(toml, UTF_8)}")
        // and it parsed to what the value said, not merely to
        // something
        val parsed = okay.codec.Json.parse(out.text)
        parsed match
          case okay.codec.Json.JObj(fs) =>
            assertEquals(fs.collectFirst { case ("app", okay.codec.Json.JStr(a)) => a }, Some(s"shop-$name"))
            assertEquals(fs.collectFirst { case ("primary_region", okay.codec.Json.JStr(r)) => r }, Some("iad"))
          case other => fail(s"tomllib did not produce an object: $other")
    }
  }

  test("a quoted, backslashed setting survives the TOML round trip unchanged") {
    assume(has(Vector("python3", "-c", "import tomllib")), "python3 with tomllib is not installed")
    rendered(Paas.Fly) { dir =>
      val out = Shell.run(Vector("python3", "-c",
        """import tomllib,sys
          |with open(sys.argv[1],'rb') as f: print(tomllib.load(f)['env']['OKAY_MOTD'])
          |""".stripMargin, dir.resolve("web").resolve("fly.toml").toString))
      assert(out.ok, out.text)
      assertEquals(out.text.trim, """a "quoted" \ value""")
    }
  }

  test("render.yaml is YAML a real parser reads, and the services are the ones the value named") {
    assume(has(Vector("ruby", "-ryaml", "-e", "1")), "ruby with YAML is not installed")
    rendered(Paas.Render) { dir =>
      val f = dir.resolve("render.yaml")
      val out = Shell.run(Vector("ruby", "-ryaml", "-rjson", "-e",
        """print JSON.dump(YAML.safe_load(File.read(ARGV[0])))""", f.toString))
      assert(out.ok, s"render.yaml is not YAML:\n${out.text}\n---\n${Files.readString(f, UTF_8)}")
      okay.codec.Json.parse(out.text) match
        case okay.codec.Json.JObj(fs) =>
          val services = fs.collectFirst { case ("services", okay.codec.Json.JArr(vs)) => vs }
            .getOrElse(fail(s"no services: ${out.text}"))
          assertEquals(services.length, 2)
          val names = services.collect { case okay.codec.Json.JObj(s) =>
            s.collectFirst { case ("name", okay.codec.Json.JStr(n)) => n } }.flatten
          assertEquals(names, Vector("web", "worker"))
          assert(fs.exists(_._1 == "databases"), out.text)
        case other => fail(s"not a mapping: $other")
    }
  }

  test("the same awkward value survives the YAML round trip") {
    assume(has(Vector("ruby", "-ryaml", "-e", "1")), "ruby with YAML is not installed")
    rendered(Paas.Render) { dir =>
      val out = Shell.run(Vector("ruby", "-ryaml", "-e",
        """y = YAML.safe_load(File.read(ARGV[0]))
          |v = y["services"][0]["envVars"].find { |e| e["key"] == "OKAY_MOTD" }
          |print v["value"]""".stripMargin, dir.resolve("render.yaml").toString))
      assert(out.ok, out.text)
      assertEquals(out.text.trim, """a "quoted" \ value""")
    }
  }
