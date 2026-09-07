package okay.deploy

import okay.conf.Secret

import java.nio.file.{Files, Path}

/**
 * What helm itself thinks of the rendering (specs/deployment.md,
 * stage 1, "The gate").
 *
 * Live-tagged because it shells out, which is this repository's rule
 * for every suite that leaves the JVM — helm being pure is not an
 * exception anyone gets to make for their own tool. It runs under
 * `sbt integrationTest` and is skipped where helm is absent.
 *
 * It earned its keep on the first run: `helm lint` rejected the first
 * rendering for an ingress annotation reading `.Values.ingress.issuer`
 * where `values.yaml` had no `ingress` key at all. A golden-file test
 * would have been perfectly happy with that chart.
 */
class TestClusterHelm extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  private val web = Service(
    name = "web",
    run = Run.Image("ghcr.io/okay/web", "1.4"),
    settings = Settings.of("okay")("pages" -> "/app/pages", "port" -> "8080"),
    secrets = Vector(Secret("env:ADMIN_TOKEN")),
    needs = Vector(
      Need.Port(8080), Need.Volume("/app/data"),
      Need.Database(Engine.Postgres, "16", "shop"),
      Need.Cache(Engine.Redis, "7"),
      Need.Dns("shop.example.com"), Need.Tls(TlsMode.Acme)),
    scale = Scale(3))

  private val worker = Service(
    name = "worker",
    run = Run.Module("okayScript", "okay-script", "okay.script.Serve"),
    needs = Vector(Need.Neighbour("web")))

  private val shop = Deployment("shop", Vector(web, worker))

  private def chart(d: Deployment)(body: Path => Unit): Unit =
    assume(Doctor.probe(Tools.helm).ok, "helm is not installed")
    val root = Files.createTempDirectory("okay-chart")
    try
      Deployment.write(d, Cluster, root): Unit
      body(root.resolve(Deployment.dir(d, Cluster.name)))
    finally
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]())
        .forEach(p => Files.deleteIfExists(p): Unit)

  test("helm lint accepts the chart") {
    chart(shop) { dir =>
      val out = Shell.run(Vector("helm", "lint", dir.toString))
      assert(out.ok, out.text)
      assert(out.text.contains("0 chart(s) failed"), out.text)
    }
  }

  test("helm template renders it, and what comes out is what the value said") {
    chart(shop) { dir =>
      val out = Shell.run(Vector("helm", "template", "shop", dir.toString))
      assert(out.ok, out.text)
      val y = out.text

      // the release name reached every object
      assert(y.contains("name: shop-web"), y)
      assert(y.contains("name: shop-web-db"), y)
      // values flowed
      assert(y.contains("replicas: 3"), y)
      assert(y.contains("image: \"ghcr.io/okay/web:1.4\""), y)
      assert(y.contains("storage: 1Gi"), y)
      // the DB_URL was built with the cluster's own DNS name
      assert(y.contains("DB_URL: \"jdbc:postgresql://shop-web-db:5432/shop\""), y)
      // the issuer resolved rather than exploding, which is the
      // defect this suite caught first
      assert(y.contains("cert-manager.io/cluster-issuer: letsencrypt-prod"), y)
      // and nothing rendered a Secret
      assert(!y.contains("kind: Secret"), y)
    }
  }

  test("helm template with an override changes exactly what the operator named") {
    chart(shop) { dir =>
      val out = Shell.run(Vector("helm", "template", "shop", dir.toString,
        "--set", "web.replicaCount=7", "--set", "ingress.issuer=letsencrypt-staging"))
      assert(out.ok, out.text)
      assert(out.text.contains("replicas: 7"), out.text)
      assert(out.text.contains("cert-manager.io/cluster-issuer: letsencrypt-staging"), out.text)
      // the things not named are untouched
      assert(out.text.contains("image: \"ghcr.io/okay/web:1.4\""), out.text)
    }
  }

  test("every rendered document is a Kubernetes object with a kind and a name") {
    chart(shop) { dir =>
      val out = Shell.run(Vector("helm", "template", "shop", dir.toString))
      assert(out.ok, out.text)
      val docs = out.text.split("(?m)^---$").toVector.map(_.trim).filter(_.nonEmpty)
      assert(docs.length >= 8, docs.length.toString)
      for doc <- docs do
        assert(doc.linesIterator.exists(_.startsWith("kind: ")), doc.take(200))
        assert(doc.contains("  name: "), doc.take(200))
    }
  }

  test("a chart with one service and nothing else still lints") {
    chart(Deployment("plain", Vector(Service("only", Run.Image("nginx", "alpine"))))) { dir =>
      val out = Shell.run(Vector("helm", "lint", dir.toString))
      assert(out.ok, out.text)
    }
  }
