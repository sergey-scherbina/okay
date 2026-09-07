package okay.deploy

import okay.conf.Secret

import java.nio.file.{Files, Path}

/**
 * The strongest gate in this arc (specs/deployment.md, stage 3, "The
 * gate"), now over all THREE clouds.
 *
 * `terraform init` downloads the AWS provider and `terraform validate`
 * then checks every resource type, every required argument and every
 * attribute reference against that provider's own schema. That is a
 * SEMANTIC check: stage 2's parsers proved the files were well-formed,
 * and this proves they describe resources that exist.
 *
 * It is not hypothetical. The first rendering failed init outright (an
 * unquoted environment value), and a deliberate `desired_kount` is
 * rejected with "Did you mean desired_count?" — which is the test
 * below that keeps this suite honest about what it is checking.
 *
 * Live, docker-dependent, and it needs the network once to fetch the
 * provider. What it still does not prove is that an apply succeeds:
 * quotas, IAM and instance classes need an account, and stay manual.
 */
class TestCloudsTerraform extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private val web = Service(
    name = "web",
    run = Run.Image("ghcr.io/okay/web", "1.4"),
    settings = Settings.of("okay")("pages" -> "/app/pages", "motd" -> """a "quoted" value"""),
    secrets = Vector(Secret("env:ADMIN_TOKEN")),
    needs = Vector(
      Need.Port(8080), Need.Volume("/app/data", size = "3Gi"),
      Need.Database(Engine.Postgres, "16.3", "shop"),
      Need.Cache(Engine.Redis, "7.1"),
      Need.Dns("shop.example.com"), Need.Tls(TlsMode.Acme),
      Need.Region("eu-central-1")),
    scale = Scale(2),
    resources = Some(Resources("100m", "256Mi", "1", "2Gi")))

  private val worker = Service(
    name = "worker",
    run = Run.Module("okayScript", "okay-script", "okay.script.Serve"),
    needs = Vector(Need.Neighbour("web"), Need.Region("eu-central-1"), Need.Port(9000, public = false)))

  private val shop = Deployment("shop", Vector(web, worker))

  private val image = "hashicorp/terraform:latest"

  private def terraform(dir: Path, args: String*): Shell.Out =
    Shell.run(Vector("docker", "run", "--rm", "-v", s"${dir.toString}:/w", "-w", "/w", image) ++
      args.toVector :+ "-no-color")

  /** a rendered deployment, initialised once — `init` is the slow part
   * (it fetches the provider), so the tests that follow share one */
  private def initialised(d: Deployment, target: Target = Aws)(body: Path => Unit): Unit =
    assume(Doctor.probe(Tools.docker).ok, "no usable docker on this machine")
    val root = Files.createTempDirectory("okay-tf")
    try
      Deployment.write(d, target, root): Unit
      val dir = root.resolve(Deployment.dir(d, target.name))
      val init = terraform(dir, "init", "-input=false")
      assume(init.ok || !init.text.contains("Failed to query available provider packages"),
        s"the provider registry is not reachable:\n${init.tail(4)}")
      assert(init.ok, s"terraform init rejected the rendering:\n${init.text}")
      body(dir)
    finally
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]())
        .forEach(p => Files.deleteIfExists(p): Unit)

  test("terraform validates the rendering against the AWS provider's own schema") {
    initialised(shop) { dir =>
      val out = terraform(dir, "validate")
      assert(out.ok, s"terraform validate rejected the rendering:\n${out.text}")
      assert(out.text.contains("The configuration is valid"), out.text)
    }
  }

  test("and it would have caught a wrong argument — which is what makes the test above mean something") {
    initialised(shop) { dir =>
      val f = dir.resolve("worker.tf")
      val good = Files.readString(f)
      Files.writeString(f, good.replace("desired_count", "desired_kount")): Unit
      val out = terraform(dir, "validate")
      assert(!out.ok, s"a misspelled argument was accepted:\n${out.text}")
      assert(out.text.contains("Unsupported argument"), out.text)
      assert(out.text.contains("desired_count"), out.text)
      Files.writeString(f, good): Unit
      assert(terraform(dir, "validate").ok)
    }
  }

  test("terraform fmt agrees with what we wrote — a generated file people read should not look generated") {
    initialised(shop) { dir =>
      val out = terraform(dir, "fmt", "-check", "-diff")
      assert(out.ok, s"terraform fmt would reformat the rendering:\n${out.text}")
    }
  }

  test("a deployment with nothing but one service still initialises and validates") {
    initialised(Deployment("plain", Vector(Service("only", Run.Image("nginx", "alpine"),
      needs = Vector(Need.Region("eu-central-1")))))) { dir =>
      val out = terraform(dir, "validate")
      assert(out.ok, out.text)
    }
  }

  // ---- the other two clouds, same gate --------------------------------

  /** what gcp can hold: no volume, because Cloud Run has no durable
   * directory and the target refuses one by name */
  private val onGcp = Deployment("shop", Vector(
    web.copy(needs = web.needs.filterNot(_.isInstanceOf[Need.Volume])
      .filterNot(_.isInstanceOf[Need.Region]) :+ Need.Region("europe-west1")),
    worker.copy(needs = Vector(Need.Neighbour("web"), Need.Region("europe-west1")))))

  private val onAzure = Deployment("shop", Vector(
    web.copy(needs = web.needs.filterNot(_.isInstanceOf[Need.Region]) :+ Need.Region("westeurope")),
    worker.copy(needs = Vector(Need.Neighbour("web"), Need.Region("westeurope")))))

  test("gcp validates against the google provider's own schema") {
    initialised(onGcp, Gcp) { dir =>
      val out = terraform(dir, "validate")
      assert(out.ok, s"terraform validate rejected the gcp rendering:\n${out.text}")
      assert(terraform(dir, "fmt", "-check", "-diff").ok, "terraform fmt would reformat the gcp rendering")
    }
  }

  test("azure validates against the azurerm provider's own schema, volume and all") {
    initialised(onAzure, Azure) { dir =>
      val out = terraform(dir, "validate")
      assert(out.ok, s"terraform validate rejected the azure rendering:\n${out.text}")
      assert(terraform(dir, "fmt", "-check", "-diff").ok, "terraform fmt would reformat the azure rendering")
      // the volume really is in there: this is gcp's refusal being a
      // decision rather than a gap
      assert(java.nio.file.Files.readString(dir.resolve("web.tf")).contains("AzureFile"))
    }
  }
